#### MNL logit type share data downscaling ###
require(nloptr)
require(tidyr)
require(dplyr)
require(tibble)

MAX_EXP = 200

mnl.mu = function(x,mu,areas,restrictions = NULL,cutoff = 0.0001) {
  mu = mu * matrix(x,nrow(mu),ncol(mu),byrow = TRUE)
  mu = mu / rowSums(cbind(1,mu)) * areas
  if (!is.null(restrictions)) {
    mu[restrictions == 1] = 0
  }
  mu[mu<cutoff] = 0
  return(mu)
}

optim.fun.mnl = function(x,mu,areas,targets,restrictions) {
  x.mu = x[1:length(targets)]
  mu = mnl.mu(x.mu,mu,areas,restrictions)
  return( sum((colSums(mu) - targets)^2) )
}

areas.sum_to = function(res, curr.areas, priors, xmat, xmat.proj) {
  curr.areas = res$out.res %>% 
    group_by(ns,lu.to) %>% 
    summarize(value = sum(value),.groups = "keep") %>% 
    rename("lu.from" = "lu.to")
  # correct for small numerical mistakes
  if (min(curr.areas$value) < 0 && min(curr.areas$value) > -10^-10) {
    curr.areas$value[curr.areas$value < 0] = 0
  }
  return(curr.areas)
}


# Arguments:
#  targets ... dataframe with columns times, lu.from, lu.to & value (all targets >= 0)
#  start.areas   ... dataframe with columns ns, lu.from & value 
#  betas   ... dataframe with columns Ks, lu.from, lu.to & value
#  xmat    ... Ns x Ks  matrix (rownames ns and colnames Ks)
#  OPTIONAL:
#  areas.update.fun .. function providing update for dynamic xmat columns, 
#                         must take as arguments res, curr.areas, priors, xmat.proj
#                         must dataframe with columns ns, lu.from & value
#                      defaults to areas.sum_to() which sums over lu.to
#  xmat.coltypes ... Ks vector, each can be either "static", "dynamic", or "projected"
#  xmat.proj     ... dataframe with columns times, Ns, Ks, must be present for each xmat.coltype specified as projected 
#  xmat.dyn.fun  ... function providing update for dynamic xmat columns, 
#                         must take as arguments res, curr.areas, priors, xmat.proj
#                         must return Ns x Ks(dynamic) columns
#  priors  ... dataframe with columns ns, lu.from, lu.to (priors >= 0)  
#  restrictions ... dataframe with columns ns, lu.from, lu.to (restrictions \in (0,1))
# Returns:
#  out.res ... dataframe with columns times, Ns, lu.from, lu.to & value (area allocation)
#  out.solver ... return list of solvers nloptr, one per year & lu.from
downscale = function(targets,start.areas,xmat,betas,
                     areas.update.fun = areas.sum_to,
                     xmat.coltypes = rep("static",ncol(xmat)),xmat.proj = NULL,xmat.dyn.fun = NULL,
                     priors = NULL,restrictions=NULL,
                     err.txt = "") {
  if (!all(start.areas$value >= 0)) {stop(paste0(err.txt,"All start.areas must be larger or equal to zero."))}
  if (!all(xmat.coltypes %in% c("static","dynamic","projected"))) {
    stop(paste0(err.txt,"All xmat.coltypes must be either static|dynamic|projected"))}
  if (length(xmat.coltypes) != ncol(xmat)) {stop(paste0(err.txt,"xmat.coltypes must be specified for all xmat columns."))}
  # for projected columns, make sure xmat.proj is supplied
  if (any(xmat.coltypes == "projected")) {
    proj.colnames = colnames(xmat)[xmat.coltypes == "projected"]
    if (is.null(xmat.proj)) {stop(paste0(err.txt,"Columns are specified as projected, but xmat.proj missing."))}
    chck.xmat = expand.grid(times = unique(targets$times), Ks = colnames(xmat)[xmat.coltypes == "projected"]) %>% 
      left_join(
        xmat.proj %>% group_by(times,Ks) %>% summarize(n = n(),.groups = "keep"),by = c("times", "Ks")
      )
    if (any(is.na(chck.xmat$n))) {stop(paste0(err.txt,"xmat.proj must provide values for all times and projected Ks."))}
  }
  if (any(xmat.coltypes == "dynamic") && is.null(xmat.dyn.fun)) {
    stop(paste0(err.txt,"Dynamic columns specified but missing xmat.dyn.fun for update."))}
  if (any(xmat.coltypes == "dynamic")) {dyn.colnames = colnames(xmat)[xmat.coltypes == "dynamic"]}
  
  # Set starting values
  curr.areas = start.areas
  curr.xmat = xmat
  curr.priors = priors
  curr.restrictions = restrictions
  
  times = unique(targets$times)
  out.solver <- list()
  for (curr.time in times) {
    # Extract targets
    curr.targets = filter(targets,times == curr.time) %>% select(-times)
    
    res = downscale.mnl.multeq(targets = curr.targets,areas = curr.areas,
                               xmat = curr.xmat,betas = betas,priors = curr.priors,
                               restrictions=curr.restrictions,err.txt = paste0(curr.time,"-"))
    out.solver[[curr.time]] = res$out.solver
    
    # update curr.area
    curr.areas = areas.update.fun(res, curr.areas, priors, xmat.proj)
    
    # update projected xmats
    if (any(xmat.coltypes == "projected")) {
      tmp.proj = xmat.proj %>% 
        filter(times == curr.time) %>% 
        dplyr::select(-times) %>% 
        pivot_wider(names_from = Ks,values_from = "value") 
      xmat[,proj.colnames] = as.matrix(tmp.proj[match(row.names(xmat),tmp.proj$ns),proj.colnames])
    }
    # update dynamic xmats
    if (any(xmat.coltypes == "dynamic")) {
      tmp.proj = xmat.dyn.fun(res, curr.areas, priors, xmat, xmat.proj)
      xmat[,dyn.colnames] = as.matrix(tmp.proj[,dyn.colnames])
    }
    # aggregate results over dataframes
    res.agg = data.frame(times = curr.time,res$out.res)
    if(curr.time==times[1]){out.res <- res.agg
    } else {
      out.res = bind_rows(out.res,res.agg)
    }
  }
  return(list(out.res = out.res,out.solver = out.solver))
}


# Arguments:
#  targets ... dataframe with columns lu.from, lu.to & value (all targets >= 0)
#  areas   ... dataframe with columns ns, lu.from & value (for each lu.from, sum(areas_lu.from) >= sum(targets_lu.from))
#  xmat    ... Ns x Ks  matrix (rownames ns and colnames Ks)
#  betas   ... dataframe with columns Ks, lu.from, lu.to & value
#  OPTIONAL:
#  priors  ... dataframe with columns ns, lu.from, lu.to (priors >= 0)  
#  restrictions ... dataframe with columns ns, lu.from, lu.to (restrictions \in (0,1))
# dimensions p, p1, p2, p3 must be named, code throws errors if names do not match
# Returns:
# out.res ... dataframe with columns Ns, lu.from, lu.to & value (area allocation)
# out.solver ... return list of solvers nloptr, one per lu.from
downscale.mnl.multeq = function(targets,areas,xmat,betas,priors = NULL,restrictions=NULL,err.txt = "") {
  if(sum(c("lu.from","lu.to")%in%colnames(targets))!=2 ){stop(paste0(err.txt,"Targets must contain lu.from and lu.to column"))}
  if (!all(areas$value >= 0)) {stop(paste0(err.txt,"All areas must be larger or equal to zero."))}
  if (any(targets$lu.from == targets$lu.to)) {stop(paste0(err.txt,"Targets lu.from must be unequal to lu.to."))}
  if (any(betas$lu.from == betas$lu.to)) {stop(paste0(err.txt,"Betas lu.from must be unequal to lu.to."))}
  
  lu.from <- unique(targets$lu.from)
  lu.to <- unique(targets$lu.to)
  # check if all targets are covered as either betas or priors
  chck.names = targets  %>% left_join(
    betas %>% group_by(lu.from,lu.to) %>% summarize(n = n(),.groups = "keep"),by = c("lu.from", "lu.to"))
  chck.names$n[is.na(chck.names$n)] = 0
  if (!is.null(priors)) {
    if (any(paste0(priors$lu.from) == paste0(priors$lu.to))) {stop(paste0(err.txt,"Priors lu.from must be unequal to lu.to."))}
    chck.names = chck.names %>% 
      left_join(
        priors %>% group_by(lu.from,lu.to) %>% summarize(n2 = n(),.groups = "keep"),by = c("lu.from", "lu.to"))
    chck.names$n2[is.na(chck.names$n2)] = 0
    chck.names$n = chck.names$n + chck.names$n2
  }
  if (any(chck.names$n == 0)) {stop(paste0(err.txt,"Missing betas or priors for targets!"))}
  
  out.solver <- list()
  curr.lu.from <- lu.from[1]
  for(curr.lu.from in lu.from){
    # Extract targets
    curr.targets = filter(targets,lu.from == curr.lu.from)$value 
    names(curr.targets) <- targets$lu.to[targets$lu.from == curr.lu.from]
    
    # Extract areas
    curr.areas = filter(areas,lu.from == curr.lu.from)$value
    names(curr.areas) <- areas$ns[areas$lu.from == curr.lu.from]
    
    # Extract betas
    curr.betas = filter(betas,lu.from == curr.lu.from) %>% 
      pivot_wider(names_from = lu.to,values_from = value,id_cols = Ks) %>% 
      column_to_rownames(var = "Ks")
    curr.betas = as.matrix(curr.betas)
    
    # Extract xmat
    curr.xmat <- xmat[match(names(curr.areas),row.names(xmat)),
                      match(row.names(curr.betas), colnames(xmat))]
    
    # Extract priors
    if (!is.null(priors) && any(priors$lu.from == curr.lu.from)) {
      curr.priors = filter(priors,lu.from == curr.lu.from) %>% 
        pivot_wider(names_from = lu.to,values_from = value,id_cols = ns) %>% 
        column_to_rownames(var = "ns")
      curr.priors = curr.priors[match(names(curr.areas),row.names(curr.priors)),,drop = FALSE]
      # check if betas have been provided for priors already
      if (any(colnames(curr.betas) %in% colnames(curr.priors))) {
        warning(paste0(err.txt,
                       "Priors provided for lu.from/lu.to combinations for which betas exist.\n These will be overwritten."))
        curr.betas = curr.betas[,-which(colnames(curr.betas) %in% colnames(curr.priors)),drop = FALSE]
      }
      curr.priors = as.matrix(curr.priors)
    } else {curr.priors = NULL}
    
    # Extract restrictions
    if (!is.null(restrictions) && any(restrictions$lu.from == curr.lu.from)) {
      curr.restrictions = filter(restrictions,lu.from == curr.lu.from) %>% 
        pivot_wider(names_from = lu.to,values_from = value,id_cols = ns) %>% 
        column_to_rownames(var = "ns")
      curr.restrictions = curr.restrictions[match(names(curr.areas),row.names(curr.restrictions)),,drop = FALSE]
      curr.restrictions = as.matrix(curr.restrictions)
    } else {curr.restrictions = NULL}
    
    res<- downscale.mnl(targets=curr.targets,areas=curr.areas,xmat=curr.xmat,
                        betas=curr.betas,priors = curr.priors,restrictions = curr.restrictions,
                        err.txt = paste0(err.txt,curr.lu.from,": "))
    out.solver[[curr.lu.from]] = res$out.solver
    
    # add residual own flows in output
    res$out.res2 = data.frame(ns = names(curr.areas),
                              curr.areas - rowSums(res$out.res),res$out.res)
    colnames(res$out.res2)[2] = paste0(curr.lu.from)
    # pivot into long format
    res.agg <- data.frame(
      lu.from=curr.lu.from,res$out.res2 %>%  
        pivot_longer(cols = -c(ns),names_to = "lu.to"))
    
    # aggregate results over dataframes
    if(curr.lu.from==lu.from[1]){out.res <- res.agg
    } else {
      out.res = bind_rows(out.res,res.agg)
    }
  }
  return(list(out.res = out.res,out.solver = out.solver))
}


# Arguments:
# targets ... p x 1  vector (with at least one targets >= 0)
# areas   ... n x 1  vector (with areas >= 0; sum(areas) >= sum(targets) )
# xmat    ... n x k  matrix
# betas   ... k x p1 matrix (with p = p1 + p2)
# priors  ... n x p2 matrix (OPTIONAL; with priors >= 0)  
# restrictions ... n x p3 matrix (OPTIONAL; with p3 < p; restrictions \in (0,1))
# dimensions p, p1, p2, p3 must be named, code throws errors if names do not match
# Returns:
# out.res ... n x p vector of area allocations
# out.solver ... return list of solver nloptr
downscale.mnl = function(targets,areas,xmat,betas,priors = NULL,restrictions=NULL,err.txt = "") {
  if (!all(targets >=0)) {
    targets[targets<0] = 0
    warning(paste0(err.txt,"Set negative targets to 0."))
  }
  if (!all(areas >= 0)) {stop(paste0(err.txt,"All areas must be larger or equal to zero."))}
  if (!sum(areas) >= sum(targets)) {
    targets = targets / sum(targets) * sum(areas)
    warning(paste0(err.txt,"Sum of areas larger than sum of targets: lowered all targets to equal sum(areas)."))
  }
  
  p = length(targets)
  n = length(areas)
  p1 = ncol(betas)
  k = nrow(betas)
  if (ncol(xmat)!=k || nrow(xmat)!=n) {stop(paste0(err.txt,"Dimensions of xmat, ares and betas do not match."))}
  if (!is.null(priors)) { 
    p2 = ncol(priors)
    if (any(priors<0)) {stop(paste0(err.txt,"Priors must be strictly non-negative."))}
  } else {p2 = 0}
  if (!p == p1 + p2) {stop(paste0(err.txt,"Dimensions of betas, targets and priors does not match."))}
  if (is.null(colnames(betas)) || is.null(names(targets)) || (p2>0 && is.null(colnames(priors)))) {
    stop(paste0(err.txt,"Betas, targets and priors need colnames."))}
  if (!all(colnames(betas) %in% names(targets))) {stop(paste0(err.txt,"Names of betas and targets do not match."))}
  
  # check restrictions for consistency
  if (!is.null(restrictions)) {
    if (!all(colnames(restrictions) %in% names(targets))) {stop(paste0(err.txt,"Names of restrictions and targets do not match."))}
    if (any(!restrictions %in% c(0,1))) {stop(paste0(err.txt,"Restrictions must be binary only!"))}
    if (nrow(restrictions)!=n) {stop(paste0(err.txt,"Dimension of restrictions does not match."))}
    restr.mat = matrix(0,n,p); colnames(restr.mat) = names(targets)
    restr.mat[,colnames(restrictions)] = restrictions
  }
  
  # out.res contains downscaled estimates; priors.mu econometric & other priors for estimation
  out.res = priors.mu = matrix(0,n,p) 
  colnames(out.res) = colnames(priors.mu) = names(targets)
  # match econometric priors
  priors.mu[,colnames(betas)] = xmat %*% betas
  # make sure the priors are numerically well behaved
  priors.mu[priors.mu > MAX_EXP] = MAX_EXP
  priors.mu[priors.mu < -MAX_EXP] = -MAX_EXP
  priors.mu[,colnames(betas)] = exp(priors.mu[,colnames(betas)])
  # match other priors (if they exist)
  if (p2 > 0) {
    priors.mu[,colnames(priors)] = priors
  }
  # remove targets that are all zero
  not.zero = (targets != 0)
  if (!all(not.zero)) {
    if (all(targets == 0)) {return(list(out.res = out.res, out.solver = NULL))}
    targets = targets[not.zero]
    priors.mu = priors.mu[,not.zero,drop = FALSE]
    if (!is.null(restrictions)) {restr.mat = restr.mat[,not.zero,drop = FALSE]}
  }
  x0 = targets / sum(targets + 1)
  opts <- list(#"algorithm" = "NLOPT_LN_SBPLX",
    # "algorithm" = "NLOPT_LN_NELDERMEAD",
    "algorithm" = "NLOPT_LN_SBPLX",
    #"local_opts" = list("algorithm" = "NLOPT_LN_SBPLX","xtol_rel" = 1.0e-7),
    "xtol_rel" = 1.0e-20,
    "xtol_abs" = 1.0e-20,
    "maxeval" = 1600
  )
  redo = TRUE; countr = 1;
  while (redo) {
    res.x = nloptr(x0,optim.fun.mnl,
                   lb = rep(10^-20,length(x0)),opts=opts,ub = rep(exp(MAX_EXP),length(x0)),
                   mu = priors.mu,areas = areas,targets = targets,restrictions = restr.mat)
    if (res.x$objective < 10^-8 || countr > 3) {
      redo =FALSE
      res.x$par = res.x$solution
    } else {
      countr = countr + 1
      x0 = res.x$solution
    }
  }
  out.mu = mnl.mu(res.x$solution[1:length(targets)],priors.mu,areas,restr.mat)
  if (all(not.zero)) {out.res = out.mu
  } else {out.res[,not.zero] = out.mu}
  return(list(out.res = out.res, out.solver = res.x))
}

# lu.to = c("grass","forest","other")
# lu.from = c("grass","forest","other")
# times = seq(2000,2050,by = 10)
# n = 1000
# p = 3
# k = 5 + length(lu.from)
# 
# start.areas = expand.grid(ns = 1:n,lu.from = lu.from,stringsAsFactors = F)
# start.areas = cbind(start.areas, value = 5+runif(nrow(start.areas)))
# 
# lu.areas <- start.areas %>% group_by(lu.from) %>% summarise(value=sum(value))
# targets = expand.grid(times = times,
#                       lu.from = lu.from,
#                       lu.to = lu.to,stringsAsFactors = F)
# targets = filter(targets,lu.from != lu.to)
# targets = cbind(targets,
#                 value = rnorm(nrow(targets),
#                               left_join(targets,lu.areas,by = "lu.from")$value / (length(lu.to)*length(times)),
#                               (length(lu.to)*n/(5* length(times))   )))
# targets$value[targets$value<0] = 0
# 
# betas = array(sample(-10:10,k*length(lu.to)*length(lu.from),replace = T),
#                c(k,length(lu.from),length(lu.to)))
# dimnames(betas)[[1]] = c(lu.from,paste0("X.", c(1:(k - length(lu.from)))))
# dimnames(betas)[[2]] = lu.from
# dimnames(betas)[[3]] = lu.to
# betas = reshape2::melt(betas,varnames = c("Ks","lu.from","lu.to"))
# betas <- betas[betas$lu.from!=betas$lu.to,]
# 
# xmat = matrix(rnorm(n*k),n,k)
# row.names(xmat) <- 1:n
# colnames(xmat) <- unique(betas$Ks)
# 
# xmat.proj = expand.grid(ns = 1:n,
#                         times = times,
#                         Ks = "X.5")
# xmat.proj = cbind(xmat.proj,value = rnorm(nrow(xmat.proj)))
# 
# priors = expand.grid(ns = c(1:n),
#                     lu.from = "grass",
#                     lu.to = "other")
# priors = cbind(priors,value = runif(nrow(priors),max = 5))
# priors = subset(priors,paste0(lu.from) != paste0(lu.to))
# 
# restrictions = expand.grid(ns = paste0(c(1:n)),
#                      lu.from = "forest",
#                      lu.to = "other",stringsAsFactors = F)
# restrictions = cbind(restrictions,value = sample(c(0,1),nrow(restrictions),replace = TRUE,prob = c(.7,.3)))
# restrictions = subset(restrictions,paste0(lu.from) != paste0(lu.to))
# 
# my.fun = function(res, curr.areas, priors, xmat, xmat.proj) {
#   tmp.mat = curr.areas %>% pivot_wider(names_from = lu.from,values_from = value)
#   tmp.mat = as.matrix(tmp.mat[match(row.names(xmat),tmp.mat$ns),-1])
#   return(tmp.mat)
# }
# 
# res1 = downscale(targets = targets,start.areas = start.areas,xmat = xmat,
#                  betas = betas,
#                  xmat.coltypes = c(rep("dynamic",3),rep("static",4),"projected"),
#                  xmat.proj = xmat.proj,xmat.dyn.fun = my.fun,
#                  restrictions = restrictions,priors = priors)
# print(
# targets %>%
#   left_join(
#     res1$out.res %>%
#       group_by(lu.from,lu.to,times) %>%
#       summarize(downscale.value = sum(value),.groups = "keep"),by = c("lu.from", "lu.to","times") ) %>%
#   mutate(diff = value - downscale.value)
# )
# 
# test.restrictions = res1$out.res %>%
#   left_join(restrictions %>% rename("restr" = "value"),by = c("lu.from", "ns", "lu.to"))  %>%
#   filter(restr == 1)
# cat("Restrictions: ",all(test.restrictions$value == 0),"\n")