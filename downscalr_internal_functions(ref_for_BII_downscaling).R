# Place holders ------

PLCHOLD_REGION = "NA_REGION"
PLCHOLD_LU = "NA_LU"
PLCHOLD_K = "NA_K"
PLCHOLD_T = "NA_TIME"


# Completing functions ------
#' Complete input targets
#' Complete input targets
#'
#' @param targets Dataframe of targets, must have columns lu.to and value
#'
#' @return Completed dataframe with lu.from, times columns and all combinations
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_targets = function(targets) {
  lu.from = lu.to = ns = NULL
  if (!tibble::has_name(targets,"lu.from")) {
    targets = cbind(lu.from = PLCHOLD_LU,targets)
  } else {
    if (any(targets$lu.from == PLCHOLD_LU)) stop(paste0("The lu.from ",PLCHOLD_LU," is reserved, use another name."))
  }
  if (!tibble::has_name(targets,"times")) {
    targets = cbind(times = PLCHOLD_T,targets)
  } else {
    if (any(targets$times == PLCHOLD_T)) stop(paste0("The times ",PLCHOLD_T," is reserved, use another label."))
  }

  # Add all combinations
  targets = targets %>%
    dplyr::right_join(targets %>%
                        tidyr::expand(nesting(lu.from,lu.to),.data$times),
                      by = c("times", "lu.from", "lu.to")) %>%
    filter(as.character(.data$lu.from) != as.character(.data$lu.to)) %>%
    tidyr::replace_na(list(value = 0))
  targets = dplyr::arrange(targets,.data$lu.from,.data$lu.to,.data$times)

  if(any(sapply(targets[,-which(names(targets)=="value")], class)!="factor") || !is.numeric(targets$value)) {warning(paste0("Column class has been changed in targets!"));
    targets <- targets %>% dplyr::mutate_at(vars(-c("value")), as.factor)}

  return(targets)
}

#' Complete input targets for population downscaling
#'
#' @param targets Dataframe of targets, must have columns pop.type and value
#'
#' @return Completed dataframe with pop.type, times columns and all combinations
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_targets_pop = function(targets) {
  pop.type = ns = times = NULL
  if (!tibble::has_name(targets,"times")) {
    targets = cbind(times = PLCHOLD_T,targets)
  } else {
    if (any(targets$times == PLCHOLD_T)) stop(paste0("The times ",PLCHOLD_T," is reserved, use another label."))
  }

  # Add all combinations
  targets = targets %>%
    dplyr::right_join(targets %>%
                        tidyr::expand(pop.type,times),
                      by = c("times", "pop.type"))  %>%
    tidyr::replace_na(list(value = 0))
  targets = dplyr::arrange(targets,pop.type,times)


  if(any(sapply(targets[,-which(names(targets)=="value")], class)!="factor") || !is.numeric(targets$value)) {warning(paste0("Column class has been changed in targets!"));
    targets <- targets %>% dplyr::mutate_at(vars(-c("value")), as.factor)}

  return(targets)
}

#' Complete input areas
#'
#' @param areas Dataframe of areas, must have columns ns and value
#'
#' @return Completed dataframe with lu.from and all combinations
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_areas = function(areas) {
  if (!tibble::has_name(areas,"lu.from")) {
    areas = cbind(lu.from = PLCHOLD_LU,areas)
  } else {
    if (any(areas$lu.from == PLCHOLD_LU)) stop(paste0("The lu.from ",PLCHOLD_LU," is reserved, use another name."))
  }

  if(any(sapply(areas[,-which(names(areas)=="value")], class)!="factor") || !is.numeric(areas$value)) {warning(paste0("Column class has been changed in start.areas!"));
    areas <- areas %>% dplyr::mutate_at(vars(-c("value")), as.factor)}

  # Add all combinations
  areas = areas %>%
    dplyr::right_join(areas %>%
                        tidyr::expand(.data$lu.from,.data$ns),by = c("ns", "lu.from")) %>%
    tidyr::replace_na(list(value = 0))
  areas = dplyr::arrange(areas,.data$lu.from,.data$ns)
  return(areas)
}

#' Complete input xmat
#'
#' @param xmat Dataframe of xmat, must have columns ns, ks and value
#'
#' @return Completed dataframe
#'
#' Internal function. Placeholder in case of needed additional completions.
#' @keywords internal
complete_xmat = function(xmat) {
  xmat = dplyr::arrange(xmat,.data$ks,.data$ns) %>%
    dplyr::right_join(xmat %>%
                        tidyr::expand(.data$ns,.data$ks),
                      by = c("ns", "ks")) %>%
    tidyr::replace_na(list(value = 0)) %>%
    dplyr::mutate(ns = as.character(.data$ns), ks = as.character(.data$ks))

  if(any(sapply(xmat[,-which(names(xmat)=="value")], class)!="factor") || !is.numeric(xmat$value)) {warning(paste0("Column class has been changed in xmat!"));
    xmat <- xmat %>% dplyr::mutate_at(vars(-c("value")), as.factor)}

  return(xmat)
}

#' Complete input betas
#'
#' @param betas Dataframe of betas, must have columns ks, lu.from and lu.to and value
#'
#' @return Completed dataframe, with added lu.from
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_betas = function(betas) {
  if (!tibble::has_name(betas,"lu.from")) {
    betas = cbind(lu.from = PLCHOLD_LU,betas)
  } else {
    if (any(betas$lu.from == PLCHOLD_LU)) stop(paste0("The lu.from ",PLCHOLD_LU," is reserved, use another name."))
  }
  betas = dplyr::arrange(betas,.data$lu.from,.data$lu.to,.data$ks)

  if(any(sapply(betas[,-which(names(betas)=="value")], class)!="factor") || !is.numeric(betas$value)) {warning(paste0("Column class has been changed in betas!"));
    betas <- betas %>% dplyr::mutate_at(vars(-c("value")), as.factor)}

  return(betas)
}

#' Complete input betas for population downscaling
#'
#' @param betas Dataframe of betas, must have columns ks, lu.from and lu.to and value
#'
#' @return Completed dataframe, with added lu.from
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_betas_pop = function(betas) {
  if (!tibble::has_name(betas,"pop.type")) {
    betas = cbind(pop.type = PLCHOLD_POPT,betas)
  } else {
    if (any(betas$pop.type == PLCHOLD_POPT)) stop(paste0("The pop.type ",PLCHOLD_POPT," is reserved, use another name."))
  }
  betas = dplyr::arrange(betas,.data$pop.type,.data$ks)

  if(any(sapply(betas[,-which(names(betas)=="value")], class)!="factor") || !is.numeric(betas$value)) {warning(paste0("Column class has been changed in betas!"));
    betas <- betas %>% dplyr::mutate_at(vars(-c("value")), as.factor)}

  return(betas)
}

#' Complete input priors
#'
#' @param priors Dataframe of priors, must have columns ns, lu.to and value
#' @param xmat Dataframe of xmat, must have columns ns, ks and value
#' @param xmat Dataframe of targets, must have columns lu.from and times
#'
#' @return Completed dataframe with times, lu.from, lu.to, weight and all combinations
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_priors = function(priors,xmat,targets) {
  # lu.from & lu.to defined to fool the package checker with dplyr namebindings
  #   (.data$ does not work in nested function)
  lu.from = lu.to = ns =  NULL

  if (!tibble::has_name(priors,"lu.from")) {
    priors = cbind(lu.from = PLCHOLD_LU,priors)
  }  else {
    if (any(priors$lu.from == PLCHOLD_LU)) stop(paste0("The lu.from ",PLCHOLD_LU," is reserved, use another name."))
  }
  if (tibble::has_name(priors,"weight")) {
    if (any(priors$weight < 0) || any(priors$weight >1 )) {
      stop("All prior weights must be between 0 and 1.")
    }
  } else {
    priors = cbind(priors,weight = 1)
  }
  if (!tibble::has_name(priors,"times")) {
    priors = priors %>%
      right_join(priors %>%
                   expand(times = unique(targets$times),lu.from,lu.to,ns),
                 by= c("ns","lu.from","lu.to"))
  }
  #Add all combinations
  priors = priors %>%  dplyr::right_join(
    priors %>% right_join(dplyr::select(xmat,ns) %>% distinct(),by= c("ns")) %>%
      tidyr::expand(.data$ns,nesting(lu.from,lu.to,times))  %>% filter(!is.na(lu.from) & !is.na(lu.to)),
    by= c("ns", "lu.from", "lu.to","times")) %>%
    filter(lu.from != lu.to) %>%
    tidyr::replace_na(list(value = 0, weight = 0))
  priors = dplyr::arrange(priors,.data$lu.from,.data$lu.to,.data$ns)

  if (!is.null(priors)) {
    if (is.null(priors$weight)) {
      if(any(sapply(priors[,-which(names(priors)=="value")], class)!="factor") || !is.numeric(priors$value)) {warning(paste0("Column class has been changed in priors!"));
        priors <- priors %>% dplyr::mutate_at(vars(-c("value")), as.factor)}
    } else {
      if(any(sapply(priors[,-which(names(priors)%in%c("value","weight"))], class)!="factor") || !is.numeric(priors$value) || !is.numeric(priors$weight)) {warning(paste0("Column class has been changed in priors!"));
        priors <- priors %>% dplyr::mutate_at(vars(-c("value","weight")), as.factor)}
    }
  }

  return(priors)
}



#' Complete input priors2
#'
#' @param priors Dataframe of priors, must have columns ns, lu.to and value
#' @param xmat Dataframe of xmat, must have columns ns, ks and value
#' @param xmat Dataframe of targets, must have columns lu.from and times
#'
#' @return Completed dataframe with times, lu.from, lu.to, weight and all combinations
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_priors2 = function(priors,xmat,targets) {
  # lu.from & lu.to defined to fool the package checker with dplyr namebindings
  #   (.data$ does not work in nested function)
  lu.from = lu.to = ns =  NULL

  if (!tibble::has_name(priors,"lu.from")) {
    priors = cbind(lu.from = PLCHOLD_LU,priors)
  }  else {
    if (any(priors$lu.from == PLCHOLD_LU)) stop(paste0("The lu.from ",PLCHOLD_LU," is reserved, use another name."))
  }
  if (tibble::has_name(priors,"weight")) {
    if (any(priors$weight < 0) || any(priors$weight >1 )) {
      stop("All prior weights must be between 0 and 1.")
    }
  } else {
    priors = cbind(priors,weight = 1)
  }
  if (!tibble::has_name(priors,"times")) {
    priors = priors %>%
      right_join(priors %>%
                   expand(times = unique(targets$times),lu.from,lu.to,ns),
                 by= c("ns","lu.from","lu.to"))
  }
  #Add all combinations
  priors = priors %>%  dplyr::right_join(
    priors %>% right_join(dplyr::select(xmat,ns) %>% distinct(),by= c("ns")) %>%
      tidyr::expand(.data$ns,nesting(lu.from,lu.to,times))  %>% filter(!is.na(lu.from) & !is.na(lu.to)),
    by= c("ns", "lu.from", "lu.to","times")) %>%
    filter(lu.from != lu.to) %>%
    tidyr::replace_na(list(value = 0, weight = 0))
  priors = dplyr::arrange(priors,.data$lu.from,.data$lu.to,.data$ns)

  if (!is.null(priors)) {
    if (is.null(priors$weight)) {
      if(any(sapply(priors[,-which(names(priors)=="value")], class)!="factor") || !is.numeric(priors$value)) {warning(paste0("Column class has been changed in priors!"));
        priors <- priors %>% dplyr::mutate_at(vars(-c("value")), as.factor)}
    } else {
      if(any(sapply(priors[,-which(names(priors)%in%c("value","weight"))], class)!="factor") || !is.numeric(priors$value) || !is.numeric(priors$weight)) {warning(paste0("Column class has been changed in priors!"));
        priors <- priors %>% dplyr::mutate_at(vars(-c("value","weight")), as.factor)}
    }
  }

  return(priors)
}

#' Complete input restrictions
#'
#' @param restrictions Dataframe of restrictions, must have columns ns, lu.to and value
#' @param xmat Dataframe of xmat, must have columns ns, ks and value
#'
#' @return Completed dataframe with lu.from and all combinations
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_restrictions = function(restrictions,xmat) {
  if (!tibble::has_name(restrictions,"lu.from")) {
    restrictions = cbind(lu.from = PLCHOLD_LU,restrictions)
  } else {
    if (any(restrictions$lu.from == PLCHOLD_LU)) stop(paste0("The lu.from ",PLCHOLD_LU," is reserved, use another name."))
  }
  # Add all combinations
  # lu.from & lu.to defined to fool the package checker with dplyr namebindings
  #   (.data$ does not work in nested function)
  lu.from = lu.to = ns = NULL
  restrictions = restrictions %>%  dplyr::right_join(
    restrictions %>% right_join(select(xmat,ns) %>% distinct(),by= c("ns")) %>%
      tidyr::expand(.data$ns,nesting(lu.from,lu.to))  %>% filter(!is.na(lu.from) & !is.na(lu.to))
    ,by= c("ns", "lu.from", "lu.to")) %>%
    tidyr::replace_na(list(value = 0))
  restrictions = dplyr::arrange(restrictions,.data$lu.from,.data$lu.to,.data$ns)

  if (!is.null(restrictions)) {
    if(any(sapply(restrictions[,-which(names(restrictions)=="value")], class)!="factor") || !is.numeric(restrictions$value)) {warning(paste0("Column class has been changed in restrictions!"));
      restrictions <- restrictions %>% dplyr::mutate_at(vars(-c("value")), as.factor)}
  }

  return(restrictions)
}

#' Complete input xmat.coltypes
#'
#' @param xmat.coltypes Dataframe of xmat.coltypes, must have columns ks and value;
#' if it is NULL, this will be created as all static
#' deprecated: can also be a vector
#'
#' @return Completed dataframe with ks and values
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_xmat.coltypes = function(xmat.coltypes,xmat) {
  if (is.null(xmat.coltypes)) {
    xmat.coltypes = data.frame(ks = unique(xmat$ks),
                               value = "static")
  }
  # Handle legacy xmat.coltypes with warning
  if (is.vector(xmat.coltypes)) {
    warning("Depreciated: xmat.coltypes should be a dataframe with columns ks and value, not a vector.\n
            Attempting to convert to appropriate coltype.\n
            This will be removed in future versions.")
    if (is.null(names(xmat.coltypes))) stop("No names in xmat.coltypes")
    ks = unique(xmat$ks)
    xmat.coltypes2 = data.frame(ks = ks,value = xmat.coltypes)
    xmat.coltypes2$value[xmat.coltypes2$ks %in% names(xmat.coltypes)] = xmat.coltypes
    xmat.coltypes = xmat.coltypes2
  }
  return(xmat.coltypes)
}

#' Complete input xmat.proj
#'
#' @param xmat.proj Dataframe of xmat.proj, must have columns ns, ks and value
#'
#' @return Completed dataframe with times and all combinations
#'
#' Internal function. Adds missing columns and completes potential sparse dataframes.
#' @keywords internal
complete_xmat.proj = function(xmat.proj) {
  if (!tibble::has_name(xmat.proj,"times")) {
    xmat.proj = cbind(times = PLCHOLD_T,xmat.proj)
  } else {
    if (any(xmat.proj$times == PLCHOLD_T)) stop(paste0("The times ",PLCHOLD_T," is reserved, use another label."))
  }
  xmat.proj = dplyr::arrange(xmat.proj,.data$times,.data$ks,.data$ns)

  if (!is.null(xmat.proj)) {
    if(any(sapply(xmat.proj[,-which(names(xmat.proj)=="value")], class)!="factor") || !is.numeric(xmat.proj$value)) {warning(paste0("Column class has been changed in xmat.proj!"));
      xmat.proj <- xmat.proj %>% dplyr::mutate_at(vars(-c("value")), as.factor)}
  }

  return(xmat.proj)
}

#' Check for targets to area mismatch over timesteps
#'
#' @param targets Dataframe of targets, must have columns lu.to and value
#' @param areas Dataframe of areas, must have columns ns and value#' @
#'
#' Internal function. Checks if targets are fullfillable over time before downscaling.
#' @keywords internal
#'
target_area_check <- function(targets, areas){

  temp_curr.lu_levels = temp_lu.from.targets = temp_lu.to.targets = lu.from = value = times = value_net = lu.to = value.lu.to = value.lu.from = . = NULL

  timesteps <- base::sort(base::unique(targets$times))

  jjj <- timesteps[1]
  for(jjj in timesteps){

    if(jjj==timesteps[1]) {
      temp_curr.lu_levels <- areas %>%
        dplyr::group_by(lu.from) %>%
        dplyr::summarize(value=sum(value))
    }

    temp_lu.from.targets <- targets %>%
      dplyr::filter(times == c(jjj)) %>%
      dplyr::group_by(lu.from) %>%
      dplyr::summarize(value.lu.from=sum(value))

    temp_curr.lu_levels <- temp_curr.lu_levels %>%
      dplyr::left_join(temp_lu.from.targets, by=c("lu.from")) %>%
      base::replace(is.na(.),0) %>%
      dplyr::mutate(value=value-value.lu.from) %>%
      dplyr::select(!value.lu.from)

    temp_lu.to.targets <- targets %>%
      dplyr::filter(times == c(jjj), value!=0) %>%
      dplyr::left_join(temp_curr.lu_levels %>%
                         dplyr::rename(value_net="value"), by="lu.from") %>%
      dplyr::group_by(lu.from) %>%
      dplyr::mutate(value=ifelse(value_net<0, value+(value_net/n()), value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(lu.to) %>%
      dplyr::summarize(value.lu.to=sum(value))

    if(any(temp_curr.lu_levels$value<0)) {
      base::cat(paste0("Total area to target mismatch in timestep ", jjj," in the following class(es):"),
                knitr::kable(temp_curr.lu_levels %>%
                               dplyr::filter(value<0) %>%
                               dplyr::rename(class="lu.from")), sep="\n")
    }

    temp_curr.lu_levels <- temp_curr.lu_levels %>%
      dplyr::mutate(value=ifelse(value<0,0,value)) %>%
      dplyr::left_join(temp_lu.to.targets, by=c("lu.from"="lu.to")) %>%
      base::replace(is.na(.),0) %>%
      dplyr::mutate(value=value+value.lu.to) %>%
      dplyr::select(!value.lu.to)

  }

}



#------Checking functions -----------


#' Error check inputs
#'
#' @param targets A dataframe with columns times, lu.from (optional), lu.to and value (all targets >= 0)
#' @param areas  A dataframe of areas with columns lu.from (optional), ns and value, with all areas >= 0 and with sum(areas) >= sum(targets)
#' @param xmat A dataframe of explanatory variables with columns ks and value
#' @param betas A dataframe of coefficients with columns ks, lu.from (optional), lu.to & value
#' @param areas.update.fun function providing update for dynamic xmat columns, must take as arguments res, curr.areas, priors, xmat.proj, must dataframe with columns ns, lu.from & value defaults to areas.sum_to() which sums over lu.to
#' @param xmat.coltypes A dataframce with columns ks and string value, can be either "static", "dynamic", or "projected"
#' @param xmat.proj dataframe with columns times, ns, ks, must be present for each xmat.coltype specified as projected
#' @param xmat.dyn.fun function providing update for dynamic xmat columns, must take as arguments res, curr.areas, priors, xmat.proj must return ns x ks(dynamic) columns
#' @param priors A dataframe of priors (if no \code{betas} were supplied) with columns ns, lu.from (optional), lu.to (with priors >= 0)
#' @param restrictions A dataframe with columns ns, lu.from (optional), lu.to and value. Values must be zero or one. If restrictions are one, the MNL function is set to zero
#'
#' Internal function. Must throw errors, no return value if inputs do not match the specification. Handle all error checking here
#' Use this for all error checking of inputs.
#'
#' @keywords internal
err_check_inputs = function(targets,areas,xmat,betas,
                            areas.update.fun,xmat.coltypes,
                            xmat.proj,xmat.dyn.fun,
                            priors,restrictions,err.txt) {
  # check NA
  if (any(is.na(targets)) ||
      any(is.na(areas)) ||
      any(is.na(xmat)) ||
      any(is.na(betas))) {stop(paste0(err.txt,"Input contains NA values"))}
  if (!is.null(priors) && any(is.na(priors))) {stop(paste0(err.txt,"Input contains NA values"))}
  if (!is.null(restrictions) && any(is.na(restrictions))) {stop(paste0(err.txt,"Input contains NA values"))}
  if (!is.null(xmat.coltypes) && any(is.na(xmat.coltypes))) {stop(paste0(err.txt,"Input contains NA values"))}
  if (!is.null(xmat.proj) && any(is.na(xmat.proj))) {stop(paste0(err.txt,"Input contains NA values"))}

  # check rows
  if (nrow(targets) < 1) {stop(paste0(err.txt,"No observations in targets!"))}
  if (nrow(areas) < 1) {stop(paste0(err.txt,"No observations in areas!"))}
  if (nrow(xmat) < 1) {stop(paste0(err.txt,"No observations in xmat!"))}
  if (nrow(betas) < 1) {stop(paste0(err.txt,"No observations in betas!"))}
  if (!is.null(priors)) {
    if (nrow(priors) < 1) {stop(paste0(err.txt,"No observations in priors!"))}
  }
  if (!is.null(restrictions)) {
    if (nrow(restrictions) < 1) {stop(paste0(err.txt,"No observations in restrictions!"))}
  }
  if (!is.null(xmat.proj)) {
    if (nrow(xmat.proj) < 1) {stop(paste0(err.txt,"No observations in xmat.proj!"))}
  }
  if (!is.null(xmat.coltypes)) {
    if (nrow(xmat.coltypes) < 1) {stop(paste0(err.txt,"No observations in xmat.coltypes!"))}
  }

  # check correct names
  check_names = all(tibble::has_name(targets, c("lu.from","lu.to","times","value")))
  if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in targets."))}
  check_names = all(tibble::has_name(xmat, c("ks","ns","value")))
  if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in xmat"))}
  check_names = all(tibble::has_name(areas, c("lu.from","ns","value")))
  if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in areas"))}
  check_names = all(tibble::has_name(betas, c("ks","lu.from","lu.to","value")))
  if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in betas"))}
  if (!is.null(priors)) {
    check_names = all(tibble::has_name(priors, c("ns","lu.from","lu.to","value")))
    if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in priors"))}
  }
  if (!is.null(restrictions)) {
    check_names = all(tibble::has_name(restrictions, c("ns","lu.from","lu.to","value")))
    if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in restrictions"))}
  }
  if (!is.null(xmat.proj)) {
    check_names = all(tibble::has_name(xmat.proj, c("ns","ks","times","value")))
    if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in xmat.proj"))}
  }
  if (!is.null(xmat.coltypes)) {
    check_names = all(tibble::has_name(xmat.coltypes, c("ks","value")))
    if (!all(check_names)) {stop(paste0(err.txt,"Missing columns in xmat.proj"))}
  }

  # check values
  if (!all(targets$value >=0)) {
    targets$value[targets$value<0] = 0
    stop(paste0(err.txt,"Negative targets!"))
  }
  if (!all(areas$value >= 0)) {stop(paste0(err.txt,"All areas must be larger or equal to zero."))}
  if (!is.null(restrictions)) {
    if (!all(restrictions$value %in% c(0,1))) {stop(paste0(err.txt,"Restrictions must be 0 or 1"))}
  }
  if (!is.null(priors)) {
    if (!all(priors$value >=0)) {stop(paste0(err.txt,"Negative priors, must be >=0"))}
  }
  # check if all targets are covered as either betas or priors
  chck.names = targets  %>% dplyr::left_join(
    betas %>% dplyr::group_by(.data$lu.from,.data$lu.to) %>% dplyr::summarize(n = n(),.groups = "keep"),by = c("lu.from", "lu.to"))
  chck.names$n[is.na(chck.names$n)] = 0
  if (!is.null(priors)) {
    if (any(paste0(priors$lu.from) == paste0(priors$lu.to))) {stop(paste0(err.txt,"Priors lu.from must be unequal to lu.to."))}
    chck.names = chck.names %>%
      left_join(
        priors %>% dplyr::group_by(.data$lu.from,.data$lu.to) %>% dplyr::summarize(n2 = n(),.groups = "keep"),by =  c("lu.from", "lu.to"))
    chck.names$n2[is.na(chck.names$n2)] = 0
    chck.names$n = chck.names$n + chck.names$n2
  }
  if (any(chck.names$n == 0)) {
    name1 = chck.names %>% dplyr::filter(chck.names$n == 0)
    stop(paste0(err.txt,"Missing betas or priors for targets: ",name1$lu.from[1],".",name1$lu.to[1],"!"))
  }
  # check if all targets are below the areas
  err.check = targets %>% dplyr::group_by(.data$times) %>%
    dplyr::summarise(total = sum(.data$value))
  if (any(sum( areas$value) < err.check$total )) {stop(paste0(err.txt,"Sum of areas larger than sum of targets."))}
  # check xmat.coltypes
  if (!all(xmat.coltypes$value %in% c("static","dynamic","projected"))) {
    stop(paste0(err.txt,"All xmat.coltypes values must be either static,dynamic, or projected"))}
  # for projected columns, make sure xmat.proj is supplied
  if (any(xmat.coltypes$value == "projected")) {
    if (is.null(xmat.proj)) {stop(paste0(err.txt,"Columns are specified as projected, but xmat.proj missing."))}
    chck.xmat = expand.grid(times = unique(targets$times),
                            ks = dplyr::filter(xmat.coltypes,.data$value == "projected")$ks) %>%
      left_join(
        xmat.proj %>% dplyr::group_by(.data$times,.data$ks) %>% dplyr::summarize(n = n(),.groups = "keep"),by = c("times", "ks")
      )
    if (any(is.na(chck.xmat$n))) {stop(paste0(err.txt,"xmat.proj must provide values for all times and projected ks."))}
  }
  if (any(xmat.coltypes$value == "dynamic") && is.null(xmat.dyn.fun)) {
    stop(paste0(err.txt,"Dynamic columns specified but missing xmat.dyn.fun for update."))}

  # check completeness
  # betas: Check if we have all ks
  ks = unique(xmat$ks)
  if (!all(ks %in% betas$ks)) {stop(paste0(err.txt,"Missing variables in betas (reference xmat)!"))}
  # xmat: Check if we have all ns
  ns = unique(areas$ns)
  if (!all(ns %in% xmat$ns)) {stop(paste0(err.txt,"Missing pixels in xmat (reference areas)!"))}
  # xmat: Check if we have all combinations
  expanded = xmat %>% tidyr::expand(.data$ks,.data$ns)
  if (nrow(expanded) != nrow(xmat)) {stop(paste0(err.txt,"Missing variables for pixels."))}
  if (!is.null(priors)) {
    # priors: Check if we have all ns
    ns = unique(areas$ns)
    if (!all(ns %in% priors$ns)) {stop(paste0(err.txt,"Missing pixels in priors (reference areas)!"))}
  }
  if (!is.null(restrictions)) {
    # restrictions: Check if we have all ns
    ns = unique(areas$ns)
    if (!all(ns %in% restrictions$ns)) {stop(paste0(err.txt,"Missing pixels in restrictions (reference areas)!"))}
  }
  if (!is.null(xmat.proj)) {
    # xmat.proj: Check if we have all ns
    ns = unique(areas$ns)
    if (!all(ns %in% xmat.proj$ns)) {stop(paste0(err.txt,"Missing pixels in xmat.proj (reference areas)!"))}
    # xmat.proj: Check if we have all combinations
    expanded = length(unique(xmat.proj$ks)) * length(unique(xmat.proj$times)) * length(ns)
    if (expanded != nrow(xmat.proj)) {stop(paste0(err.txt,"Missing variable/ns/times combination in xmat.proj."))}
  }
}



# Downscaling functions ------

# Downscale2: for overcoming the "Input values contain NA" issue from err_check_input by NA values in the times column of the priors dataframe
# using a different complete_priors2 to replace the default complete_priors. in complete_priors2, the final complementary process includes times, so it now has a priors with full times (no NA in times)
# Yazhen, 20230812
# Should be further examined: because theoretically, if priors_RstLnd has all ns grids for a region, then this time_completing should have been done in a former place within complete_prior() function. Maybe the priors_RstLnd has some NA values for certain grids...then these grids' data is also missing in the original Curr_suit_srp...
#' Downscaling of Land-Use (Change) Data
#'
#' Performs downscaling of land-use data over specified time steps using a range of inputs, including targets, areas, explanatory variables, and priors. It supports both bias correction and non-targeted downscaling methods.
#'
#' @param targets A dataframe with mandatory columns: `times` (character), `lu.to` (character), and `value` (numeric, all values >= 0). Optional column: `lu.from` (character). Represents the downscaling targets for each time step and land-use change.
#' @param start.areas A dataframe with starting areas. Includes mandatory columns: `ns` (character, representing grid IDs for downscaling) and `value` (numeric, all areas >= 0 and sum of areas >= sum of targets). Optional column: `lu.from` (character).
#' @param times A character vector of time steps for downscaling. The first time step must be present in `targets`. If `NULL`, times are derived from unique values in `targets`. Default is `NULL`.
#' @param xmat A dataframe with explanatory variables for econometric priors. Includes columns: `ns` (character), `ks` (character), and `value` (numeric). If `NULL`, a placeholder is used. Default is `NULL`.
#' @param betas A dataframe of coefficients for econometric priors. Includes columns: `ks` (character), `lu.to` (character), and `value` (numeric). Optional column: `lu.from` (character). If `NULL`, a placeholder is used. Default is `NULL`.
#' @param areas.update.fun A function providing an update for dynamic xmat columns. Takes as arguments `res`, `curr.areas`, `priors`, `xmat.proj` and must return a dataframe with columns `ns`, `ks`, and `value`. Defaults to `areas.sum_to()`, which sums over `lu.to`.
#' @param xmat.coltypes A vector `ks`, with each element being either "static", "dynamic", or "projected". Determines how different columns in `xmat` are treated during the downscaling process.
#' @param xmat.proj A dataframe with projections. Includes columns: `times` (character), `ns` (character), `ks` (character), and `value` (numeric). Required for each `xmat.coltype` specified as projected.
#' @param xmat.dyn.fun A function providing updates for dynamic xmat columns. Takes as arguments `res`, `curr.areas`, `priors`, `xmat.proj` and must return a dataframe with `ns x ks(dynamic)` columns.
#' @param priors A dataframe with exogenous priors. Includes columns: `times` (character, optional), `ns` (character), `lu.from` (character, optional), `lu.to` (character), and `value` (numeric, >= 0). An optional `weight` column (numeric, 0 <= weight <= 1) can be supplied to adjust the influence of exogenous priors.
#' @param restrictions A dataframe with restrictions. Includes columns: `ns` (character), `lu.from` (character, optional), `lu.to` (character), and `value` (numeric). Values must be either zero or one, indicating whether the MNL function should be set to zero for certain combinations.
#' @param options A list of solver options. Use `\link{downscale_control}` to obtain default options and for more detailed information.
#'
#' @details The function integrates various data inputs to match `p` targets using either projections from an MNL-type model or exogenous priors. Appropriate input validation and preprocessing are performed before downscaling.
#'
#' @return A list containing three elements:
#' * `out.res`: A dataframe with columns `times`, `ns`, `lu.from`, `lu.to`, and `value` (area allocation).
#' * `out.solver`: A list detailing the solver output.
#' * `ds.inputs`: A list documenting all the inputs used in the downscaling function.
#'
#' @export downscale
#' @import nloptr
#' @import tidyr
#' @import dplyr
#' @import tibble
#'
#' @examples
#' require(dplyr)
#' require(tidyr)
#' require(tibble)
#' betas = NULL
#' for (jj in unique(argentina_luc$lu.from)) {
#'  Y = dplyr::filter(argentina_luc,lu.from == jj & Ts == 2000) %>%
#'    pivot_wider(names_from = lu.to)
#'  X = argentina_df$xmat %>% tidyr::pivot_wider(names_from = "ks") %>%
#'    dplyr::arrange(match(ns,Y$ns))
#'  Y = Y %>% dplyr::select(-c(lu.from,Ts,ns))
#'  X = X %>% dplyr::select(-c(ns))
#'  res1 <- mnlogit(as.matrix(X), as.matrix(Y),baseline = which(colnames(Y) == jj),
#'           niter = 3,nburn = 2)
#'  betas = betas %>% dplyr::bind_rows(
#'   apply(res1$postb, c(1, 2), mean) %>%
#'   as.data.frame() %>% tibble::rownames_to_column("ks") %>%
#'   pivot_longer(cols = -c(1),names_to = "lu.to") %>%
#'   dplyr::mutate(lu.from = jj,.before="lu.to")
#'  )
#' }
#' ns = unique(argentina_df$lu_levels$ns)
#' priors = data.frame(ns = as.character(ns),lu.from="Cropland",
#'             lu.to="Forest",value = as.numeric(runif(length(ns))))
#' res1 = downscale(targets = argentina_FABLE %>% dplyr::filter(times == "2010"),
#'          start.areas = argentina_df$lu_levels,
#'          xmat = argentina_df$xmat,
#'          betas = betas %>% dplyr::filter(lu.from!="Cropland" | lu.to!="Forest"),
#'          priors = priors)
#'
#'  dgp1 = sim_luc(1000,tt = 3)
#'  res1 = downscale(targets = dgp1$targets,start.area = dgp1$start.areas,
#'         xmat = dgp1$xmat,betas = dgp1$betas,times = c(1:3))
downscale2 = function(targets,
                      start.areas,
                      times = NULL,
                      xmat = NULL,
                      betas = NULL,
                      areas.update.fun = areas.sum_to,
                      xmat.coltypes = NULL,
                      xmat.proj = NULL,
                      xmat.dyn.fun = xmat.sum_to,
                      priors = NULL,
                      restrictions = NULL,
                      options = downscale_control()) {

  lu.from = value = proj = ks = dyn = lu.to = NULL
  # Handle input checking
  err.txt = options$err.txt
  targets = complete_targets(targets)
  start.areas = complete_areas(start.areas)
  target_area_check(targets, start.areas)
  if (is.null(xmat)) {
    xmat = data.frame(ns = unique(start.areas$ns),
                      ks = PLCHOLD_K,
                      value = 0)
  }
  xmat = complete_xmat(xmat)
  if (is.null(betas)) {
    betas = data.frame(
      lu.from = paste0(PLCHOLD_LU, 1),
      lu.to = paste0(PLCHOLD_LU, 2),
      ks = unique(xmat$ks),
      value = 0
    )
  }
  betas = complete_betas(betas)
  xmat.coltypes = complete_xmat.coltypes(xmat.coltypes, xmat)
  if (!is.null(priors)) {
    # priors = complete_priors(priors, xmat, targets)
    priors = complete_priors2(priors, xmat, targets)
  }
  if (!is.null(restrictions)) {
    restrictions = complete_restrictions(restrictions, xmat)
  }
  if (!is.null(xmat.proj)) {
    xmat.proj = complete_xmat.proj(xmat.proj)
  }
  err_check_inputs(
    targets,
    start.areas,
    xmat,
    betas,
    areas.update.fun,
    xmat.coltypes,
    xmat.proj,
    xmat.dyn.fun,
    priors,
    restrictions,
    err.txt
  )

  # save column types of xmat
  if (any(xmat.coltypes$value == "projected")) {
    proj.colnames = filter(xmat.coltypes, .data$value == "projected")$ks
  }
  if (any(xmat.coltypes$value == "dynamic")) {
    dyn.colnames = filter(xmat.coltypes, .data$value == "dynamic")$ks
  }

  # Set starting values
  curr.areas = start.areas
  curr.xmat = xmat
  curr.restrictions = restrictions

  if (is.null(times)) {
    times = unique(targets$times)
  }
  out.solver <- list()
  for (curr.time in times) {
    # Extract targets
    curr.targets = filter(targets, times == curr.time) %>% dplyr::select(-times)

    # Extract priors
    if (any(priors$times == curr.time)) {
      curr.priors = filter(priors, times == curr.time) %>% dplyr::select(-times)
    } else {
      curr.priors = NULL
    }

    # BUGFIX TK: added check for missing targets for every time iteration
    #   This was before only done for the initial period, resulting in missed
    #     land cover.
    # check if all lu in curr.areas in curr.targets
    if (!all(unique(curr.areas$lu.from) %in% unique(curr.targets$lu.from))) {
      # if not save them to a table and add them back manually
      missing_luc = unique(curr.areas$lu.from)[!unique(curr.areas$lu.from) %in% unique(curr.targets$lu.from)]
      missing_luc = dplyr::filter(curr.areas, lu.from %in% missing_luc) %>%
        mutate(lu.to = lu.from)
    } else {
      missing_luc = NULL
    }

    if (options$solve_fun == "solve_biascorr") {
      curr.options = options
      curr.options$err.txt = paste0(curr.time, " ", curr.options$err.txt)
      res = solve_biascorr.mnl(
        targets = curr.targets,
        areas = curr.areas,
        xmat = curr.xmat,
        betas = betas,
        priors = curr.priors,
        restrictions = curr.restrictions,
        options = curr.options
      )
      out.solver[[as.character(curr.time)]] = res$out.solver
    } else if (options$solve_fun == "solve_notarget") {
      curr.options = options
      curr.options$err.txt = paste0(curr.time, " ", curr.options$err.txt)
      res = solve_notarget.mnl(
        targets = curr.targets,
        areas = curr.areas,
        xmat = curr.xmat,
        betas = betas,
        restrictions = curr.restrictions,
        options = curr.options
      )
      out.solver[[as.character(curr.time)]] = res$out.solver
    }

    # add not covered land-uses (because no targets exist)
    if (!is.null(missing_luc)) {
      res$out.res = res$out.res %>% bind_rows(missing_luc)
    }

    # update curr.area
    curr.areas = areas.update.fun(res, curr.areas, priors, xmat.proj)

    # update projected xmats
    if (any(xmat.coltypes$value == "projected")) {
      xmat = xmat %>%
        left_join(
          dplyr::filter(xmat.proj, times == curr.time) %>%
            dplyr::select(-times) %>% rename("proj" = "value") %>%
            mutate(ns = as.character(.data$ns)),
          by = c("ks", "ns")
        )
      xmat = xmat %>% mutate(value = ifelse(!is.na(.data$proj), .data$proj, value)) %>%
        dplyr::select(-proj)
    }
    # update dynamic xmats
    if (any(xmat.coltypes$value == "dynamic")) {
      tmp.proj = xmat.dyn.fun(res, curr.areas, priors, xmat, xmat.proj)
      xmat = xmat %>%
        left_join(
          tmp.proj %>% rename("dyn" = "value") %>%  mutate(ns = as.character(.data$ns)) %>%
            filter(ks %in% xmat.coltypes$ks[xmat.coltypes$value == "dynamic"]),
          by = c("ks", "ns")
        )
      xmat = xmat %>% mutate(value = ifelse(!is.na(.data$dyn), .data$dyn, value)) %>%
        dplyr::select(-dyn)
    }
    # aggregate results over dataframes
    res.agg = data.frame(times = curr.time, res$out.res)
    if (curr.time == times[1]) {
      out.res <- res.agg %>%  mutate(ns = as.character(.data$ns))
    } else {
      out.res = bind_rows(out.res, res.agg %>%  mutate(ns = as.character(.data$ns)))
    }
  }

  # Remove default values columns
  if (any(out.res$times == PLCHOLD_T)) {
    out.res = out.res %>% dplyr::select(-times)
  }
  if (any(out.res$lu.from == PLCHOLD_LU)) {
    out.res = out.res %>% dplyr::select(-lu.from)
  }
  if (any(out.res$lu.to == PLCHOLD_LU)) {
    out.res = out.res %>% dplyr::filter(lu.to != PLCHOLD_LU)
  }

  ret <- list(
    out.res = out.res,
    out.solver = out.solver,
    ds.inputs = list(
      targets = targets,
      start.areas = start.areas,
      xmat = xmat,
      betas = betas,
      areas.update.fun = areas.update.fun,
      xmat.coltypes = xmat.coltypes,
      xmat.proj = xmat.proj,
      xmat.dyn.fun = xmat.dyn.fun,
      priors = priors,
      restrictions = restrictions,
      options = options
    )
  )

  class(ret) = "downscalr"

  return(ret)
}


# DownscaleBII: for downscaling while introducing a BII constraints (which iteratively run downscale for each time step with gradually higher weighting putting from prior/xmat to a BII prior)
# adapted from Downscale2
# Revision: introducing priors to all LUC (which means for LUCs e.g. CrpLnd->NatLnd, it will be using a mixed priors: partly based on this newly-introduced BII-based priors (for each LUC across all SimUs), then (1-weight) based on the original econometric i.e. xmat * beta)
# Yazhen, 20230814

downscale_BII <- function (targets, start.areas, times = NULL, xmat = NULL, betas = NULL,
                           areas.update.fun = areas.sum_to, xmat.coltypes = NULL, xmat.proj = NULL,
                           xmat.dyn.fun = xmat.sum_to, priors = NULL, restrictions = NULL,
                           options = downscale_control())
{
  lu.from = value = proj = ks = dyn = lu.to = NULL
  err.txt = options$err.txt
  targets = complete_targets(targets)
  start.areas = complete_areas(start.areas)
  target_area_check(targets, start.areas)
  if (is.null(xmat)) {
    xmat = data.frame(ns = unique(start.areas$ns), ks = PLCHOLD_K,
                      value = 0)
  }
  xmat = complete_xmat(xmat)
  if (is.null(betas)) {
    betas = data.frame(lu.from = paste0(PLCHOLD_LU, 1),
                       lu.to = paste0(PLCHOLD_LU, 2), ks = unique(xmat$ks),
                       value = 0)
  }
  betas = complete_betas(betas)
  xmat.coltypes = complete_xmat.coltypes(xmat.coltypes,
                                         xmat)
  if (!is.null(priors)) {
    # priors = complete_priors(priors, xmat, targets)
    priors = complete_priors2(priors, xmat, targets)
  }

  priors <- priors %>%
    mutate(weight=ifelse((value>0)&!(
      (lu.from=="CrpLnd" & lu.to=="PltFor") |
        (lu.from=="Grass" & lu.to=="PltFor") |
        (lu.from=="OthNatLnd" & lu.to=="PltFor") |
        (lu.from=="PltFor" & lu.to=="OthNatLnd") |
        (lu.from=="CrpLnd" & lu.to=="RstLnd") |
        (lu.from=="Grass" & lu.to=="RstLnd") |
        (lu.from=="PltFor" & lu.to=="RstLnd")
    ),0,weight))


  if (!is.null(restrictions)) {
    restrictions = complete_restrictions(restrictions, xmat)
  }
  if (!is.null(xmat.proj)) {
    xmat.proj = complete_xmat.proj(xmat.proj)
  }
  err_check_inputs(targets, start.areas, xmat, betas, areas.update.fun,
                   xmat.coltypes, xmat.proj, xmat.dyn.fun, priors, restrictions,
                   err.txt)
  if (any(xmat.coltypes$value == "projected")) {
    proj.colnames = filter(xmat.coltypes, .data$value ==
                             "projected")$ks
  }
  if (any(xmat.coltypes$value == "dynamic")) {
    dyn.colnames = filter(xmat.coltypes, .data$value ==
                            "dynamic")$ks
  }
  curr.areas = start.areas
  curr.xmat = xmat
  curr.restrictions = restrictions
  if (is.null(times)) {
    times = unique(targets$times)
  }
  out.solver <- list()
  for (curr.time in times) {
    print(paste0("Downscale_BII goes to year: ",curr.time))
    curr.targets = filter(targets, times == curr.time) %>%
      dplyr::select(-times)
    if (any(priors$times == curr.time)) {
      curr.priors = filter(priors, times == curr.time) %>%
        dplyr::select(-times)
    }else {
      curr.priors = NULL
    }
    if (!all(unique(curr.areas$lu.from) %in% unique(curr.targets$lu.from))) {
      missing_luc_type = unique(curr.areas$lu.from)[!unique(curr.areas$lu.from) %in%
                                                      unique(curr.targets$lu.from)]
      missing_luc = dplyr::filter(curr.areas, lu.from %in%
                                    missing_luc_type) %>% mutate(lu.to = lu.from)
    }else {
      missing_luc = NULL
    }

    ## Yazhen: modify here for BII: add an additional BII-checking and re-downscaling loop (by adjusting BII-based priors for other LUC higher stepwise each time),  when executing downscaling for each step
    # P.S. Original: calling solve_biascorr.mnl() to DownScale for current time; each time just execute this and get results (no BII-based loop)
   
    if (options$solve_fun == "solve_biascorr") {
      curr.options = options
      curr.options$err.txt = paste0(curr.time, " ", curr.options$err.txt)

      updateDS = TRUE;  count_DS = 1

      while(updateDS){
        res = solve_biascorr.mnl_BII(targets = curr.targets,
                                     areas = curr.areas, xmat = curr.xmat, betas = betas,
                                     priors = curr.priors, restrictions = curr.restrictions,
                                     options = curr.options)
        out.solver[[as.character(curr.time)]] = res$out.solver

        curr.BII.diff <- check_BII_regional(resNow = res, rrrNow=rrr, checktime=curr.time)
        if(is.na(curr.BII.diff)){
          curr.BII.diff <- 0
          updateDS=FALSE
          count_DS = count_DS+1
        }else{
          if((curr.BII.diff>0)|(count_DS>=6)){
            updateDS=FALSE
          }else{
            curr.priors <- update_priors_HigherBIIweight(curr.priors)
            count_DS = count_DS+1
          }
        } # if-else
      } # while updateDS: looping to update DownScaling while introducing a more BII-weighted priors for other LUC flows (except -> RstLnd and PltFor) each time, for this period (curr.time)
    } # execution of the DS function (solve_biascorr.mnl_BII) for this period (curr.time)


    if (!is.null(missing_luc)) {
      res$out.res = res$out.res %>% bind_rows(missing_luc)
    }
    curr.areas = areas.update.fun(res, curr.areas, priors,
                                  xmat.proj)
    if (any(xmat.coltypes$value == "projected")) {
      xmat = xmat %>% left_join(dplyr::filter(xmat.proj,
                                              times == curr.time) %>% dplyr::select(-times) %>%
                                  rename(proj = "value") %>% mutate(ns = as.character(.data$ns)),
                                by = c("ks", "ns"))
      xmat = xmat %>% mutate(value = ifelse(!is.na(.data$proj),
                                            .data$proj, value)) %>% dplyr::select(-proj)
    }
    if (any(xmat.coltypes$value == "dynamic")) {
      tmp.proj = xmat.dyn.fun(res, curr.areas, priors,
                              xmat, xmat.proj)
      xmat = xmat %>% left_join(tmp.proj %>% rename(dyn = "value") %>%
                                  mutate(ns = as.character(.data$ns)) %>% filter(ks %in%
                                                                                   xmat.coltypes$ks[xmat.coltypes$value == "dynamic"]),
                                by = c("ks", "ns"))
      xmat = xmat %>% mutate(value = ifelse(!is.na(.data$dyn),
                                            .data$dyn, value)) %>% dplyr::select(-dyn)
    }
    res.agg = data.frame(times = curr.time, res$out.res)
    if (curr.time == times[1]) {
      out.res <- res.agg %>% mutate(ns = as.character(.data$ns))
    } else {
      out.res = bind_rows(out.res, res.agg %>% mutate(ns = as.character(.data$ns)))
    } # merge out.res for each time period
  } # end for looping: (curr.time in times)



  if (any(out.res$times == PLCHOLD_T)) {
    out.res = out.res %>% dplyr::select(-times)
  }
  if (any(out.res$lu.from == PLCHOLD_LU)) {
    out.res = out.res %>% dplyr::select(-lu.from)
  }
  if (any(out.res$lu.to == PLCHOLD_LU)) {
    out.res = out.res %>% dplyr::filter(lu.to != PLCHOLD_LU)
  }
  ret <- list(out.res = out.res, out.solver = out.solver,
              ds.inputs = list(targets = targets, start.areas = start.areas,
                               xmat = xmat, betas = betas, areas.update.fun = areas.update.fun,
                               xmat.coltypes = xmat.coltypes, xmat.proj = xmat.proj,
                               xmat.dyn.fun = xmat.dyn.fun, priors = priors, restrictions = restrictions,
                               options = options))
  class(ret) = "downscalr"
  return(ret)
}



#' Bias correction solver for multinomial logit type problems
#'
#' @param targets A dataframe with columns lu.from, lu.to and value (all targets >= 0)
#' @param areas A dataframe of areas with columns lu.from, ns and value, with all areas >= 0
#'   and with sum(areas) >= sum(targets)
#' @param xmat A dataframe of explanatory variables with columns ks and value.
#' @param betas A dataframe of coefficients with columns ks, lu.from, lu.to & value
#' @param priors A dataframe of priors (if no \code{betas} were supplied) with columns ns, lu.from, lu.to (with priors >= 0)
#' @param restrictions A dataframe with columns ns, lu.from, lu.to and value. Values must be zero or one. If restrictions are one, the MNL function is set to zero
#' @param options A list with solver options. Call \code{\link{downscale_control}} for default options and for more detail.
#'
#' @details Given \code{p} targets matches either the projections from an MNL-type model or exogeneous priors.
#'
#' You should not call this functions directly, call \code{\link{downscale}} instead.
#'
#' min \deqn{\sum  (  z ij areas_i  - targets_j )^2}
#' s.t. \deqn{ z_ij = \mu_ij}
#' \deqn{\mu_ij = \lambda_ij / (1 + \sum \lambda_ij )}
#' \deqn{\lambda_ij = x_j + \exp xmat_i betas_j} or \eqn{\lambda_ij = x_j + priors_ij}
#' \deqn{x_j >= 0}
#' with \eqn{i = 1,...,n} and  \eqn{j = 1,...,n}. #' For each target either betas and xmats or priors have to be supplied. Priors have to be strictly larger or equal to zero.
#'
#' When \code{cutoff} is specified, \eqn{z_ij} is defined as above if \eqn{mu_ij > cutoff}. If \eqn{mu_ij <= cutoff} then \eqn{z_ij = 0}. Per default \code{cutoff} is set to zero.
#'
#' Targets should be specified in a dataframe with at least a lu.to and a value column. All targets must be larger or equal to zero. If an lu.from column is supplied it has to be specified in all arguments. In this case the bias correction is performed for all lu.from classes.
#'
#' Areas correspond to either an areas per pixel (ns), with value or optionally the are of lu.from in a pixel. All areas must be larger The function expects lu.from
#'
#' Restrictions are binary and optional. If restrictions are supplied, in case \eqn{restrictions_ij = 1} then  \eqn{z_ij = 0}.
#'
#' @return A list containing
#' * \code{out.res} A \code{n x p} matrix of area allocations
#' * \code{out.solver} A list of the solver output
#'
#' @export solve_biascorr.mnl
#' @import nloptr
#' @import tidyr
#' @import dplyr
#' @import tibble
#'
#' @examples
#' ## A basic example
solve_biascorr.mnl = function(targets,areas,xmat,betas,priors = NULL,restrictions=NULL,
                              options = downscale_control()) {
  lu.from <- unique(targets$lu.from)
  lu.to <- unique(targets$lu.to)
  ks = unique(betas$ks)

  out.solver <- list()
  curr.lu.from <- lu.from[1]
  full.out.res = NULL
  for(curr.lu.from in lu.from){
    err.txt = paste0(curr.lu.from," ",options$err.txt)

    # Extract targets
    curr.targets = dplyr::filter(targets,lu.from == curr.lu.from)$value
    names(curr.targets) <- targets$lu.to[targets$lu.from == curr.lu.from]
    curr.lu.to = names(curr.targets)

    # Extract betas
    curr.betas = dplyr::filter(betas,lu.from == curr.lu.from & lu.to %in% curr.lu.to) %>%
      tidyr::pivot_wider(names_from = "lu.to",values_from = "value",id_cols = "ks") %>%
      tibble::column_to_rownames(var = "ks")
    curr.betas = as.matrix(curr.betas)

    # Extract xmat
    ## IMPORTANT CHECK ORDER OF VARIABLES FIXED
    curr.xmat = dplyr::filter(xmat,ks %in% row.names(curr.betas)) %>%
      tidyr::pivot_wider(names_from = "ks",values_from = "value",id_cols = "ns")  %>%
      tibble::column_to_rownames(var = "ns") %>%
      dplyr::select(rownames(curr.betas))
    curr.xmat = as.matrix(curr.xmat)

    # Extract areas
    curr.areas = dplyr::filter(areas,lu.from == curr.lu.from)$value
    names(curr.areas) <- areas$ns[areas$lu.from == curr.lu.from]
    # BUGFIX: MW, order of curr.areas wrong, need to re-arrange based on xmat
    if (nrow(curr.xmat) > 0) {
      curr.areas = curr.areas[match(rownames(curr.xmat),names(curr.areas))]
    }

    # Extract priors
    ## IMPORTANT CHECK ORDER OF VARIABLES SIMILARLY TO XMAT
    if (!is.null(priors) && any(priors$lu.from == curr.lu.from)) {
      curr.priors = dplyr::filter(priors,lu.from == curr.lu.from & lu.to %in% curr.lu.to) %>%
        tidyr::pivot_wider(names_from = lu.to,values_from = "value",id_cols = "ns") %>%
        tibble::column_to_rownames(var = "ns")
      curr.prior_weights = dplyr::filter(priors,lu.from == curr.lu.from & lu.to %in% curr.lu.to) %>%
        tidyr::pivot_wider(names_from = lu.to,values_from = "weight",id_cols = "ns") %>%
        tibble::column_to_rownames(var = "ns")
      name_match = match(names(curr.areas),row.names(curr.priors))
      curr.priors = curr.priors[name_match,,drop = FALSE]
      curr.prior_weights = curr.prior_weights[name_match,,drop = FALSE]
      # check if betas have been provided for priors already
      mixed_priors =c()
      nonmixed_priors = colnames(curr.priors)
      if (any(colnames(curr.betas) %in% colnames(curr.priors))) {
        mixed_priors = colnames(curr.betas)[which(colnames(curr.betas) %in% colnames(curr.priors))]
        nonmixed_priors = nonmixed_priors[!nonmixed_priors %in% mixed_priors]
        #warning(paste0(err.txt,
        #               "Priors provided for lu.from/lu.to combinations for which betas exist.\n These will be overwritten."))
        #curr.betas = curr.betas[,-which(colnames(curr.betas) %in% colnames(curr.priors)),drop = FALSE]
      }
      curr.priors = as.matrix(curr.priors)
    } else {curr.priors = NULL}

    # Extract restrictions
    if (!is.null(restrictions) && any(restrictions$lu.from == curr.lu.from)) {
      curr.restrictions = dplyr::filter(restrictions,lu.from == curr.lu.from) %>%
        tidyr::pivot_wider(names_from = lu.to,values_from = "value",id_cols = "ns") %>%
        tibble::column_to_rownames(var = "ns")
      curr.restrictions = curr.restrictions[match(names(curr.areas),row.names(curr.restrictions)),,drop = FALSE]
      curr.restrictions = as.matrix(curr.restrictions)
    } else {curr.restrictions = NULL}

    p = length(curr.targets)
    n = length(curr.areas)
    p1 = ncol(curr.betas)
    k = nrow(curr.betas)

    if (p1 > 0 ) {
      if (ncol(curr.xmat)!=k || nrow(curr.xmat)!=n) {
        stop(paste0(err.txt,"Dimensions of xmat, areas and betas do not match."))
      }
    }
    if (!is.null(curr.priors)) {
      #p2 = ncol(curr.priors)
      p2 = length(nonmixed_priors)
      p2_mixed = length(mixed_priors)
      if (any(curr.priors<0)) {stop(paste0(err.txt,"Priors must be strictly non-negative."))}
    } else {p2 = 0;p2_mixed = 0}

    # check restrictions for consistency
    if (!is.null(curr.restrictions) & any(colnames(curr.restrictions) %in% names(curr.targets))) {
      restr.mat = matrix(0,n,p); colnames(restr.mat) = names(curr.targets)
      restr.mat[,colnames(curr.restrictions)] = curr.restrictions
    } else {restr.mat = NULL}

    # out.res contains downscaled estimates; priors.mu econometric & other priors for estimation
    out.res = priors.mu = matrix(0,n,p)
    colnames(out.res) = colnames(priors.mu) = names(curr.targets)
    # match econometric priors
    priors.mu[,colnames(curr.betas)] = curr.xmat %*% curr.betas
    # make sure the priors are numerically well behaved
    priors.mu[priors.mu > options$MAX_EXP] = options$MAX_EXP
    priors.mu[priors.mu < -options$MAX_EXP] = -options$MAX_EXP
    priors.mu[,colnames(curr.betas)] = exp(priors.mu[,colnames(curr.betas)])
    # match other priors (if they exist)
    if (p2 > 0) {
      priors.mu[,nonmixed_priors] = curr.priors[,nonmixed_priors]
    }
    if (p2_mixed > 0) {
      w1 = curr.prior_weights[,mixed_priors,drop = FALSE]
      #re-scale exogeneous prior to priors.mu
      eco.priors_sum = apply(priors.mu[,mixed_priors,drop = FALSE],c(2),sum)
      exo.priors = curr.priors[,mixed_priors,drop = FALSE]
      exo.priors_sum = apply(exo.priors, 2, sum)
      exo.priors = t(
        (t(exo.priors) / exo.priors_sum) * eco.priors_sum   )
      priors.mu[,mixed_priors] = as.matrix((1-w1)*priors.mu[,mixed_priors] + w1*exo.priors)
    }
    # remove targets that are all zero
    not.zero = (curr.targets != 0)
    if (all(curr.targets == 0)) {

      #catch case if all targets are equal zero
      out.solver[[curr.lu.from]] = NULL
    } else {

      #cut out zero targets from targets and priors
      if (any(curr.targets == 0) && !all(curr.targets == 0)) {
        curr.targets = curr.targets[not.zero]
        priors.mu = priors.mu[,not.zero,drop = FALSE]
        if (!is.null(curr.restrictions)) {restr.mat = restr.mat[,not.zero,drop = FALSE]}
      }

      #proceed with bias correction
      x0 = curr.targets / sum(curr.targets + 1)
      opts <- list(algorithm = options$algorithm,
                   xtol_rel = options$xtol_rel,
                   xtol_abs = options$xtol_abs,
                   maxeval = options$maxeval
      )
      # check if the optimiser uses gradient or not
      if (grepl("_LD_",opts$algorithm)) {
        eval_grad_f = grad_sqr_diff.mnl
      } else {
        eval_grad_f = NULL
      }
      res.x = nloptr::nloptr(x0 = x0,
                             eval_f = sqr_diff.mnl,
                             eval_grad_f = eval_grad_f,
                             lb = rep(exp(-options$MAX_EXP),length(x0)),
                             ub = rep(exp(options$MAX_EXP),length(x0)),
                             opts=opts,
                             mu = priors.mu,areas = curr.areas,targets = curr.targets,
                             restrictions = restr.mat,cutoff = options$cutoff)
      res.x$par = res.x$solution

      out.mu = mu.mnl(res.x$solution[1:length(curr.targets)],priors.mu,curr.areas,restr.mat,options$cutoff)

      # If differences are still too large, do individual logit models boosted by grid search
      if (res.x$objective > options$max_diff) {
        # Find targets where out.mu deviates more than max_diff from the target
        error_ind = (curr.targets - colSums(out.mu))^2 > options$max_diff
        error_targets = curr.targets[error_ind]
        error_restrictions = restr.mat[,error_ind]

        # Calculate residual areas -  res_areas
        res_areas = curr.areas
        if (any(!error_ind)) {
          res_areas = res_areas - rowSums(out.mu[,!error_ind,drop=FALSE])
        }

        # Loop over remaining targets
        for (ccc in 1:length(error_targets)) {
          curr_error_target = error_targets[ccc]
          curr_error_restrictions = error_restrictions[,ccc]
          curr_error_mu = priors.mu[,names(curr_error_target),drop=FALSE]

          # Do an iterated grid search to find correct scaling coefficient for priors
          curr_scaling = iterated_grid_search(min_param = -options$MAX_EXP,
                                              max_param = options$MAX_EXP,
                                              func = sqr_diff.mnl,
                                              max_iterations = 10,
                                              precision_threshold = 1e-3,
                                              exp_transform = TRUE,
                                              mu = curr_error_mu,areas = res_areas,
                                              targets = curr_error_target,
                                              restrictions = curr_error_restrictions,
                                              cutoff = options$cutoff)

          # Re-scale prior
          curr_error_mu = curr_error_mu * curr_scaling$best_param

          # Optimize with scaled priors
          res.x = nloptr::nloptr(x0 = 1,
                                 eval_f = sqr_diff.mnl,
                                 eval_grad_f = eval_grad_f,
                                 lb = exp(-options$MAX_EXP),
                                 ub = exp(options$MAX_EXP),
                                 opts=opts,
                                 mu = curr_error_mu,areas = res_areas,targets = curr_error_target,
                                 restrictions = curr_error_restrictions,cutoff = options$cutoff)

          # Calculate areas of current target with mu.mnl
          curr_error_out.mu =
            mu.mnl(res.x$solution,
                   curr_error_mu,res_areas,curr_error_restrictions,
                   options$cutoff)

          # Add calculated areas to out.mu
          out.mu[,names(curr_error_target)] = curr_error_out.mu

          # Substract areas from res.areas
          res_areas = res_areas - curr_error_out.mu

          # Add note to res.x
          res.x$message = "INDIVIDUAL LOGIT BOOSTED BY GRID SEARCH: Standard optimization failed to converge"
        }
      }

      if (all(not.zero)) {out.res = out.mu
      } else {out.res[,not.zero] = out.mu}
      out.solver[[curr.lu.from]] = res.x
    }

    # add residual own flows in output
    out.res2 = data.frame(ns = names(curr.areas),
                          curr.areas - rowSums(out.res),out.res)
    colnames(out.res2)[2] = paste0(curr.lu.from)
    # pivot into long format
    res.agg <- out.res2 %>%
      pivot_longer(cols = -c("ns"),names_to = "lu.to") %>%
      bind_cols(lu.from = curr.lu.from)

    # aggregate results over dataframes
    if(curr.lu.from==lu.from[1]){
      full.out.res <- res.agg
    } else {
      full.out.res = bind_rows(full.out.res,res.agg)
    }
  }
  return(list(out.res = full.out.res, out.solver = out.solver))
}


#' Bias correction solver for Poisson type problems
#'
#' @param targets A dataframe with columns pop.type and value (all targets >= 0)
#' @param xmat A dataframe of explanatory variables with columns ks and value.
#' @param betas A dataframe of coefficients with columns ks, lu.from, lu.to & value
#' @param options A list with solver options. Call \code{\link{downscale_control_pop}} for default options and for more detail.
#'
#' @details Given \code{J} targets matches  the projections from a Poisson-type model.
#'
#' You should not call this functions directly, call \code{\link{downscale_pop}} instead.
#'
#' Targets should be specified in a dataframe with  a pop.type and a value column.
#' All targets must be larger or equal to zero.
#'
#' @return A list containing
#' * \code{out.res} A \code{n x p} matrix of area allocations
#' * \code{out.solver} A list of the solver output
#'
#' @export solve_biascorr.poisson
#' @import nloptr
#' @import tidyr
#' @import dplyr
#' @import tibble
#'
#' @examples
#' ## A basic example
solve_biascorr.poisson = function(targets,xmat,betas,
                                  options = downscale_control_pop()) {
  pop.type <- unique(targets$pop.type)
  ks = unique(betas$ks)
  value = NULL

  out.solver <- list()
  curr.pop.type <- pop.type[1]
  full.out.res = data.frame()
  for(curr.pop.type in pop.type){
    err.txt = paste0(curr.pop.type," ",options$err.txt)

    # Extract targets
    curr.targets = dplyr::filter(targets,pop.type == curr.pop.type)$value

    # Extract betas
    curr.betas = dplyr::filter(betas,pop.type == curr.pop.type) %>%
      tibble::column_to_rownames(var = "ks") %>%
      dplyr::select(value)
    curr.betas = as.matrix(curr.betas)

    # Extract xmat
    ## IMPORTANT CHECK ORDER OF VARIABLES FIXED
    curr.xmat = dplyr::filter(xmat,ks %in% row.names(curr.betas)) %>%
      tidyr::pivot_wider(names_from = "ks",values_from = "value",id_cols = "ns")  %>%
      tibble::column_to_rownames(var = "ns") %>%
      dplyr::select(rownames(curr.betas))
    curr.xmat = as.matrix(curr.xmat)

    n = nrow(curr.xmat)
    k = nrow(curr.betas)


    if (ncol(curr.xmat)!=k || nrow(curr.xmat)!=n) {
      stop(paste0(err.txt,"Dimensions of xmat, areas and betas do not match."))
    }

    # out.res contains downscaled estimates; priors.mu econometric & other priors for estimation
    out.res = priors.mu = matrix(0,n,1)
    colnames(out.res) = colnames(priors.mu) = names(curr.pop.type)
    # match econometric priors
    priors.mu = curr.xmat %*% curr.betas
    # make sure the priors are numerically well behaved
    priors.mu[priors.mu > options$MAX_EXP] = options$MAX_EXP
    priors.mu[priors.mu < -options$MAX_EXP] = -options$MAX_EXP
    priors.mu = exp(priors.mu)

    res.x = list()
    if (curr.targets == 0) {
      #catch case if all targets are equal zero
      res.x$par = -Inf
      out.solver[[curr.pop.type]] = res.x
      out.res = priors.mu * 0
    } else {
      res.x$par = log(curr.targets / sum(priors.mu))

      out.res = exp(res.x$par) * priors.mu
      out.solver[[curr.pop.type]] = res.x
    }

    # add residual own flows in output
    res.agg = data.frame(ns = rownames(curr.xmat),
                         pop.type = curr.pop.type, value = out.res)

    # aggregate results over dataframes
    full.out.res = bind_rows(full.out.res,res.agg)
  }
  return(list(out.res = full.out.res, out.solver = out.solver))
}


solve_biascorr.mnl_BII <- function (targets, areas, xmat, betas, priors = NULL, restrictions = NULL,
                                    options = downscale_control())
{
  lu.from <- unique(targets$lu.from)
  lu.to <- unique(targets$lu.to)
  ks = unique(betas$ks)
  out.solver <- list()
  curr.lu.from <- lu.from[1]
  full.out.res = NULL
  for (curr.lu.from in lu.from) {
    err.txt = paste0(curr.lu.from, " ", options$err.txt)
    curr.targets = dplyr::filter(targets, lu.from == curr.lu.from)$value
    names(curr.targets) <- targets$lu.to[targets$lu.from ==
                                           curr.lu.from]
    curr.lu.to = names(curr.targets)
    curr.betas = dplyr::filter(betas, lu.from == curr.lu.from &
                                 lu.to %in% curr.lu.to) %>% tidyr::pivot_wider(names_from = "lu.to",
                                                                               values_from = "value", id_cols = "ks") %>% tibble::column_to_rownames(var = "ks")
    curr.betas = as.matrix(curr.betas)
    curr.xmat = dplyr::filter(xmat, ks %in% row.names(curr.betas)) %>%
      tidyr::pivot_wider(names_from = "ks", values_from = "value",
                         id_cols = "ns") %>% tibble::column_to_rownames(var = "ns") %>%
      dplyr::select(rownames(curr.betas))
    curr.xmat = as.matrix(curr.xmat)
    curr.areas = dplyr::filter(areas, lu.from == curr.lu.from)$value
    names(curr.areas) <- areas$ns[areas$lu.from == curr.lu.from]
    if (nrow(curr.xmat) > 0) {
      curr.areas = curr.areas[match(rownames(curr.xmat),
                                    names(curr.areas))]
    }
    if (!is.null(priors) && any(priors$lu.from == curr.lu.from)) {
      curr.priors = dplyr::filter(priors, lu.from == curr.lu.from &
                                    lu.to %in% curr.lu.to) %>% tidyr::pivot_wider(names_from = lu.to,
                                                                                  values_from = "value", id_cols = "ns") %>% tibble::column_to_rownames(var = "ns")
      curr.prior_weights = dplyr::filter(priors, lu.from ==
                                           curr.lu.from & lu.to %in% curr.lu.to) %>% tidyr::pivot_wider(names_from = lu.to,
                                                                                                        values_from = "weight", id_cols = "ns") %>%
        tibble::column_to_rownames(var = "ns")
      name_match = match(names(curr.areas), row.names(curr.priors))
      curr.priors = curr.priors[name_match, , drop = FALSE]
      curr.prior_weights = curr.prior_weights[name_match,
                                              , drop = FALSE]
      mixed_priors = c()
      nonmixed_priors = colnames(curr.priors)
      if (any(colnames(curr.betas) %in% colnames(curr.priors))) {
        mixed_priors = colnames(curr.betas)[which(colnames(curr.betas) %in%
                                                    colnames(curr.priors))]
        nonmixed_priors = nonmixed_priors[!nonmixed_priors %in%
                                            mixed_priors]
      }
      curr.priors = as.matrix(curr.priors)
    }
    else {
      curr.priors = NULL
    }
    if (!is.null(restrictions) && any(restrictions$lu.from ==
                                      curr.lu.from)) {
      curr.restrictions = dplyr::filter(restrictions,
                                        lu.from == curr.lu.from) %>% tidyr::pivot_wider(names_from = lu.to,
                                                                                        values_from = "value", id_cols = "ns") %>% tibble::column_to_rownames(var = "ns")
      curr.restrictions = curr.restrictions[match(names(curr.areas),
                                                  row.names(curr.restrictions)), , drop = FALSE]
      curr.restrictions = as.matrix(curr.restrictions)
    }
    else {
      curr.restrictions = NULL
    }
    p = length(curr.targets)
    n = length(curr.areas)
    p1 = ncol(curr.betas)
    k = nrow(curr.betas)
    if (p1 > 0) {
      if (ncol(curr.xmat) != k || nrow(curr.xmat) != n) {
        stop(paste0(err.txt, "Dimensions of xmat, areas and betas do not match."))
      }
    }
    if (!is.null(curr.priors)) {
      p2 = length(nonmixed_priors)
      p2_mixed = length(mixed_priors)
      if (any(curr.priors < 0)) {
        stop(paste0(err.txt, "Priors must be strictly non-negative."))
      }
    }
    else {
      p2 = 0
      p2_mixed = 0
    }
    if (!is.null(curr.restrictions) & any(colnames(curr.restrictions) %in%
                                          names(curr.targets))) {
      restr.mat = matrix(0, n, p)
      colnames(restr.mat) = names(curr.targets)
      restr.mat[, colnames(curr.restrictions)] = curr.restrictions
    }
    else {
      restr.mat = NULL
    }
    out.res = priors.mu = matrix(0, n, p)
    colnames(out.res) = colnames(priors.mu) = names(curr.targets)
    priors.mu[, colnames(curr.betas)] = curr.xmat %*% curr.betas
    priors.mu[priors.mu > options$MAX_EXP] = options$MAX_EXP
    priors.mu[priors.mu < -options$MAX_EXP] = -options$MAX_EXP
    priors.mu[, colnames(curr.betas)] = exp(priors.mu[,
                                                      colnames(curr.betas)])
    if (p2 > 0) {
      priors.mu[, nonmixed_priors] = curr.priors[, nonmixed_priors]
    }
    if (p2_mixed > 0) {
      w1 = curr.prior_weights[, mixed_priors, drop = FALSE]
      eco.priors_min = apply(priors.mu[, mixed_priors,
                                       drop = FALSE], c(2), min)
      eco.priors_max = apply(priors.mu[, mixed_priors,
                                       drop = FALSE], c(2), max)
      exo.priors = curr.priors[, mixed_priors, drop = FALSE]
      exo.priors_min = apply(exo.priors, 2, min)
      exo.priors_max = apply(exo.priors, 2, max)
      exo.priors = t((t(exo.priors) - exo.priors_min)/(exo.priors_max -
                                                         exo.priors_min) * (eco.priors_max - eco.priors_min) +
                       eco.priors_min)
      exo.priors[is.na(exo.priors)] = 0
      priors.mu[, mixed_priors] = as.matrix((1 - w1) *
                                              priors.mu[, mixed_priors] + w1 * exo.priors)
    }
    adjusted_areas_own_flow = FALSE
    if (sum(curr.targets) > 0 && ((sum(curr.areas) - sum(curr.targets))/sum(curr.targets)) <
        options$ref_class_adjust_threshold) {
    }
    not.zero = (curr.targets != 0)
    if (all(curr.targets == 0)) {
      out.solver[[curr.lu.from]] = NULL
    }
    else {
      if (any(curr.targets == 0) && !all(curr.targets ==
                                         0)) {
        curr.targets = curr.targets[not.zero]
        priors.mu = priors.mu[, not.zero, drop = FALSE]
        if (!is.null(curr.restrictions)) {
          restr.mat = restr.mat[, not.zero, drop = FALSE]
        }
      }
      if (any(apply(priors.mu, c(2), max) < 10^-8)) {
        p.min = apply(priors.mu, c(2), min)
        p.max = apply(priors.mu, c(2), max)
        priors.mu = t((t(priors.mu) - p.min)/(p.max -
                                                p.min))
      }
      x0 = curr.targets/sum(curr.targets + 1)
      opts <- list(algorithm = options$algorithm, xtol_rel = options$xtol_rel,
                   xtol_abs = options$xtol_abs, maxeval = options$maxeval)
      redo = TRUE
      countr = 1
      while (redo) {
        res.x = nloptr::nloptr(x0, sqr_diff.mnl, lb = rep(exp(-options$MAX_EXP),
                                                          length(x0)), ub = rep(exp(options$MAX_EXP),
                                                                                length(x0)), opts = opts, mu = priors.mu,
                               areas = curr.areas, targets = curr.targets,
                               restrictions = restr.mat, cutoff = options$cutoff)
        if (res.x$objective < options$max_diff || countr >
            options$redo) {
          redo = FALSE
          res.x$par = res.x$solution
        }
        else {
          countr = countr + 1
          x0 = res.x$solution
        }
      }
      out.mu = mu.mnl(res.x$solution[1:length(curr.targets)],
                      priors.mu, curr.areas, restr.mat, options$cutoff)
      if (all(not.zero)) {
        out.res = out.mu
      }
      else {
        out.res[, not.zero] = out.mu
      }
      out.solver[[curr.lu.from]] = res.x
    }
    out.res2 = data.frame(ns = names(curr.areas), curr.areas -
                            rowSums(out.res), out.res)
    colnames(out.res2)[2] = paste0(curr.lu.from)
    res.agg <- out.res2 %>% pivot_longer(cols = -c("ns"),
                                         names_to = "lu.to") %>% bind_cols(lu.from = curr.lu.from)
    if (adjusted_areas_own_flow) {
      res.agg = dplyr::filter(res.agg, lu.from != lu.to)
    }
    if (curr.lu.from == lu.from[1]) {
      full.out.res <- res.agg
    }
    else {
      full.out.res = bind_rows(full.out.res, res.agg)
    }
  }
  return(list(out.res = full.out.res, out.solver = out.solver))
}
#' Multinomial logit mean calculation with restrictions and cutoff
#'
#' @param x Bias correction parameters
#' @param mu Means of each class
#' @param areas Vector of areas
#' @param restrictions Optional restrictions. Binary, if 1, the log-odds are set to zero
#' @param cutoff Optional, defaults to zero. If set, all log-odds below cutoff are set to zero
#'
#' @return Returns the vector of log-odds times the areas
#'
#' @keywords internal
mu.mnl = function(x,mu,areas,restrictions = NULL,cutoff = 0) {
  mu = mu * matrix(x,nrow(mu),ncol(mu),byrow = TRUE)
  if (!is.null(restrictions)) {
    mu[restrictions == 1] = 0
  }
  mu = mu / rowSums(cbind(1,mu))
  mu[mu<cutoff] = 0
  mu = mu * areas
  return(mu)
}

#' Squared differences optimization function with multinomial logit
#'
#' @param x Bias correction parameters
#' @param mu Means of each class
#' @param areas Vector of areas
#' @param targets Targets to match
#' @param restrictions Optional restrictions. Binary, if 1, the log-odds are set to zero
#' @param cutoff Optional, defaults to zero. If set, all log-odds below cutoff are set to zero
#'
#' @return Squared differences of optimized values and targets
#'
#' @keywords internal
sqr_diff.mnl = function(x,mu,areas,targets,restrictions = NULL,cutoff = 0) {
  x.mu = x[1:length(targets)]
  mu = mu.mnl(x.mu,mu,areas,restrictions,cutoff)
  return( sum((colSums(mu) - targets)^2) )
}

#' Provides analytical gradients for the squared differences optimization function with multinomial logit
#'
#' @inheritParams sqr_diff.mnl
#'
#' @return Vector of length \eqn{J} containing the gradients for each value of \code{x}.
#'
#' Does not work for \code{cutoff} \eqn{> 0} or with restrictions. Generates a warning if these are supplied.
#'
#' @keywords internal
grad_sqr_diff.mnl = function(x,mu,areas,targets,restrictions = NULL,cutoff = 0) {
  if (!is.null(restrictions) || cutoff > 0) {
    stop("Gradient optimization not yet implemented for restrictions or cutoff > 0. \n
            Consider switching to a gradient free optimiser (see the nloptr documentation for details).")
  }
  n = nrow(mu); J = length(x)
  mu = cbind(1,mu * matrix(exp(x) ,n,J,byrow = TRUE))
  theta = mu / c(rowSums(mu)); theta = theta[,-1,drop=FALSE]
  diff_targets = targets - colSums(areas * theta)

  grad_x = rep(0,J)
  for (jj in 1:J) {
    own_d = sum( areas * theta[,jj] * (1 - theta[,jj])) * diff_targets[jj]
    other_d = 0
    for (jj2 in c(1:J)[-jj]) {
      other_d = other_d + sum(areas * theta[,jj] * theta[,jj2]) * diff_targets[jj2]
    }
    grad_x[jj] = -2*own_d + 2*other_d
  }
  return(grad_x)
}

#' Bayesian logit model with Pólya Gamma prior with MCMC
#'
#' @param X An n by k matrix of explanatory variables
#' @param Y An n by p matrix of dependent variables
#' @param baseline Baseline class for estimation. Parameters will be set to zero. Defaults to the p-th column.
#' @param niter Total number of MCMC draws. Defaults to 1000.
#' @param nburn Burn-in draws for MCMC. Note: \code{nburn} has to be lower than \code{niter}. Defaults to 500.
#' @param A0 Prior variance scalar for all slope coefficients
#' @param calc_marginal_fx Should marginal effects be calculated? Defaults to \code{FALSE}.
#'
#' @details MCMC estimation of a multinomial logit model following Polson et al. (2013).
#'
#' @return A list containing
#' * \code{postb} A k x p x (niter - nburn dimensions) array containing posterior draws of the slope coefficients.
#' * \code{marginal_fx} A k x p x (niter - nburn dimensions) array containing posterior draws of marginal effects.
#' * \code{X, Y, baseline} The matrices of explanatory and dependent variables, as defined above and the baseline class.
#'
#' @references Nicholas G. Polson, James G. Scott, and Jesse Windle. Bayesian inference for
#' logistic models using Polya-Gamma latent variables.
#' Journal of the American statistical Association 108.504 (2013): 1339-1349.
#'
#' @importFrom MASS mvrnorm
#' @import BayesLogit
#' @export mnlogit
#'
#' @examples
#' n <- 100
#' p <- 3
#' k <- 2
#' X <- cbind(1, matrix(rnorm(n * (k - 1), 0, 2), n, k - 1))
#' BETA <- matrix(sample(c(-3:3), k * p, replace = TRUE), k, p)
#' BETA[, p] <- 0
#' Y <- exp(X %*% BETA) / rowSums(exp(X %*% BETA))
#' res1 <- mnlogit(X, Y)
#' print(BETA)
#' print(apply(res1$postb, c(1, 2), mean))
#'
#' require(tidyr)
#' Y = dplyr::filter(argentina_luc,lu.from == "Cropland" & Ts == 2000) %>%
#'    pivot_wider(names_from = lu.to)
#' X = argentina_df$xmat %>% tidyr::pivot_wider(names_from = "ks") %>%
#'    dplyr::arrange(match(ns,Y$ns))
#' Y = Y %>% dplyr::select(-c(lu.from,Ts,ns))
#' X = X %>% dplyr::select(-c(ns))
#' res1 <- mnlogit(as.matrix(X), as.matrix(Y),baseline = which(colnames(Y) == "Cropland"),
#'           niter = 100,nburn = 50)
#' print(apply(res1$postb, c(1, 2), mean))
mnlogit <- function(X, Y, baseline = ncol(Y),
                    niter = 1000, nburn = 500, A0 = 10^4,
                    calc_marginal_fx = FALSE) {
  if (niter<=nburn) {stop("niter has to be higher than nburn.")}

  n <- nrow(X)
  k <- ncol(X)
  p <- ncol(Y)
  pp <- (1:p)[-baseline]
  # prior setup
  nn <- matrix(1, nrow(X), ncol(Y))
  beta_prior_mean <- matrix(0, k, p)
  beta_prior_var <- diag(k) * A0

  ### set-up the gibbs sampler
  ndiscard <- nburn
  nretain <- niter - ndiscard
  # save the posterior draws here
  postb <- array(0, c(k, p, nretain))
  dimnames(postb)[[1]] <- colnames(X)
  dimnames(postb)[[2]] <- colnames(Y)

  # starting values (won't matter after sufficient draws)
  curr.beta <- matrix(0, ncol = p, nrow = k)
  curr.beta[, p] <- 0
  curr.xb <- X %*% curr.beta
  curr.om <- matrix(0, n, p)

  # pre-calculate some terms for faster draws
  beta_prior_var_inv <- solve(beta_prior_var)
  kappa <- Y - nn / 2

  ### Gibbs sampling
  pb <- utils::txtProgressBar(min = 0, max = niter, style = 3)
  for (iter in 1:niter) {
    for (j in pp) {
      A <- rowSums(exp(X %*% curr.beta[, -j]))
      c.j <- log(A)
      eta.j <- X %*% curr.beta[, j] - c.j
      # sample omega
      curr.om[, j] <- BayesLogit::rpg(n, nn[, j], eta.j)
      # draw beta
      V <- solve(beta_prior_var_inv + t(X) %*% (X * curr.om[, j]))
      b <- V %*% (beta_prior_var_inv %*% beta_prior_mean[, j] + t(X) %*% (kappa[, j] + c.j * curr.om[, j]))
      curr.beta[, j] <- MASS::mvrnorm(1, b, V)
    }
    # we are past the burn-in, save the draws
    if (iter > ndiscard) {
      s <- iter - ndiscard
      postb[, , s] <- curr.beta
      curr.xb <- X %*% curr.beta
    }
    utils::setTxtProgressBar(pb, iter)
  }
  close(pb)
  ### marginal effects calculations
  if (calc_marginal_fx) {
    marginal_fx <- array(0, c(k, p, nretain))
    dimnames(marginal_fx)[[1]] <- colnames(X)
    dimnames(marginal_fx)[[2]] <- colnames(Y)

    meanXs <- apply(X, c(2), mean)
    for (jjj in 1:nretain) {
      MU <- X %*% postb[, , jjj]
      pr <- exp(MU) / rowSums(exp(MU))
      for (ppp in 1:p) {
        bbb <- matrix(1, n, k) %*% diag(postb[, ppp, jjj])
        pr_bbb <- bbb
        for (kk in 1:k) {
          pr_bbb[, kk] <- rowSums(pr %*% diag(postb[kk, , jjj]))
        }
        partial1 <- pr[, ppp] * (bbb - pr_bbb)
        marginal_fx[, ppp, jjj] <- apply(partial1, c(2), mean)
      }
    }
  } else{marginal_fx = NULL}
  results <- list(
    postb = postb,
    marginal_fx = marginal_fx,
    X = X, Y = Y, baseline = baseline
  )
  return(results)
}


## check_BII_regional
## Yazhen, 20230814

check_BII_regional <- function(resNow=res, rrrNow=rrr,checktime=curr.time){
  res_BII_REGION <- bind_cols(REGION = rrrNow, resNow$out.res) %>% left_join(BII_SimU_data,by=c("ns")) %>%
    mutate(BII_from=NA) %>%
    mutate(BII_from=ifelse(lu.from %in% c("CrpLnd"),BII_CrpLnd_An,BII_from)) %>%
    mutate(BII_from=ifelse(lu.from %in% c("PltFor"),BII_CrpLnd_Pe,BII_from)) %>%
    mutate(BII_from=ifelse(lu.from %in% c("Grass"),BII_GrsLnd,BII_from)) %>%
    mutate(BII_from=ifelse(lu.from %in% c("Forest"),BII_MngFor,BII_from)) %>%
    mutate(BII_from=ifelse(lu.from %in% c("OthNatLnd"),BII_NatLnd,BII_from)) %>%
    mutate(BII_from=ifelse(lu.from %in% c("RstLnd"),BII_RstLnd,BII_from)) %>%
    mutate(BII_from=ifelse(lu.from %in% c("protected_priforest"),BII_PriFor,BII_from)) %>%
    mutate(BII_from=ifelse(lu.from %in% c("protected_other"),BII_NatLnd,BII_from)) %>%
    mutate(BII_from=ifelse(is.na(BII_from),0,BII_from)) %>%
    mutate(BII_to=NA) %>%
    mutate(BII_to=ifelse(lu.to %in% c("CrpLnd"),BII_CrpLnd_An,BII_to)) %>%
    mutate(BII_to=ifelse(lu.to %in% c("PltFor"),BII_CrpLnd_Pe,BII_to)) %>%
    mutate(BII_to=ifelse(lu.to %in% c("Grass"),BII_GrsLnd,BII_to)) %>%
    mutate(BII_to=ifelse(lu.to %in% c("Forest"),BII_MngFor,BII_to)) %>%
    mutate(BII_to=ifelse(lu.to %in% c("OthNatLnd"),BII_NatLnd,BII_to)) %>%
    mutate(BII_to=ifelse(lu.to %in% c("RstLnd"),BII_RstLnd,BII_to)) %>%
    mutate(BII_to=ifelse(lu.to %in% c("protected_priforest"),BII_PriFor,BII_to)) %>%
    mutate(BII_to=ifelse(lu.to %in% c("protected_other"),BII_NatLnd,BII_to)) %>%
    mutate(BII_to=ifelse(is.na(BII_to),0,BII_to)) %>%
    mutate(BII_delta=BII_to-BII_from) %>%
    mutate(times=checktime) %>%
    dplyr::select(REGION,times,ns,lu.from,lu.to,value,range_rarity,BII_from,BII_to,BII_delta) %>%
    mutate(delta_BII_ns=value*range_rarity*BII_delta) %>%
    group_by(REGION,times) %>%
    summarise(DELTA_BII=sum(delta_BII_ns))%>%
    mutate(SCEN1=curr.SCEN1,SCEN2=curr.SCEN2,SCEN3=curr.SCEN3) %>%
    relocate(DELTA_BII,.after = "SCEN3")

  ##fromGLOBIOM
  DELTABII_for_check <- REGION_DELTA_BII_COMPARE %>%
    subset(REGION == rrrNow &
             SCEN1 == curr.SCEN1 &  SCEN2 == curr.SCEN2   &  SCEN3 == curr.SCEN3  &
             year==checktime
    ) %>%
    dplyr::select(c(REGION,SCEN1,SCEN2,SCEN3, year, value)) %>%
    unique() %>% rename(times = year)

  ##Merge the two dataframes, calculate the difference, and compare
  chck.DS.BII = DELTABII_for_check %>% rename(GLOBIOM.DELTABII=value) %>%
    mutate(times=as.character(times)) %>%
    left_join(res_BII_REGION%>%
                mutate(times=as.character(times))%>%rename(downscale.DELTABII=DELTA_BII),
              by = c("REGION","SCEN1","SCEN2","SCEN3","times") ) %>%
    mutate(diff = downscale.DELTABII-GLOBIOM.DELTABII)

  curr.BII.diff=chck.DS.BII$diff[chck.DS.BII$times==checktime]
  return(curr.BII.diff)

}



update_priors_HigherBIIweight <- function(priorsNow=priors){
  priorsNow <- priorsNow %>%
    mutate(weight=ifelse((value>0)&!(
      (lu.from=="CrpLnd" & lu.to=="PltFor") |
        (lu.from=="Grass" & lu.to=="PltFor") |
        (lu.from=="OthNatLnd" & lu.to=="PltFor") |
        (lu.from=="PltFor" & lu.to=="OthNatLnd") |
        (lu.from=="CrpLnd" & lu.to=="RstLnd") |
        (lu.from=="Grass" & lu.to=="RstLnd") |
        (lu.from=="PltFor" & lu.to=="RstLnd")
      # ),0.5,weight))
    ),weight+0.2,weight))

  priorsNow <- priorsNow %>%
    mutate(weight=ifelse(weight>1,1,weight))

  return(priorsNow)

}
###### End of Script ######



