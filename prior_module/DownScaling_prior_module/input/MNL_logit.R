# ### Bayesian MN-logit model with Pólya Gamma prior ###
# ###  see Polson et al. (2013)
# rm(list=ls())
# 
# library(MASS)
# library(truncnorm)
# library(BayesLogit)
# 
#   
# ## first let's construct our logit DGP
# n=2000
# k = 4
# p = 3
# 
# X <- cbind( 1, runif(n),runif(n),runif(n) )
# 
# 
# BETA = matrix(
#   c(1,-2,2,3,
#     -3,1,3,-1,
#     rep(0,4)),
#   4,3)
# 
# nn = rep(1,n) # ´number of trials
# MU = X %*% BETA
# pr = exp(MU) / rowSums(exp(MU))
# #Y = pr
# Y = matrix(0,n,p)
# for (i in 1:n) {
#   pr2 =  sample(size=1,x = 3,prob = pr[i,])
#   Y[i,pr2] = 1
# }
# 



bayes.MNlogit <- function(X,Y,baseline=4,ntot,nburn){


### let us assign prior values
# beta mean and variance are proper, but with high variance (= low prior information)
n = nrow(X)
k = ncol(X)
p = ncol(Y)
pp = (1:p)[-baseline]

nn = matrix(1,n,p)

beta_prior_mean = matrix(0,k,p)
beta_prior_var = diag(k) * 10^4

### set-up the gibbs sampler
# total number of draws
niter = ntot
# retain only S2 and discard S1, with S = S1 + S2
ndiscard = nburn
nretain = niter - ndiscard
# save the posterior draws here
postb = array(0,c(k,p,nretain))
posty = array(0,c(n,p,nretain))
postom = array(0,c(n,p,nretain))

# starting values (won't matter after sufficient draws)
#curr.beta = mvrnorm(1,beta_prior_mean,beta_prior_var)
#curr.beta = solve(crossprod(X)) %*% crossprod(X,Y)
curr.beta <- matrix(0,ncol = p, nrow = k)
curr.beta[,p] = 0
curr.xb = X %*% curr.beta
curr.om = matrix(0,n,p)

# pre-calculate some terms for faster draws
beta_prior_var_inv = solve(beta_prior_var)
kappa = Y - nn/2

### Gibbs sampling
for (iter in 1:niter) {
  cat("iter:",iter,"\n")
  
  for (j in pp) {
    
    A = rowSums( exp( X %*% curr.beta[,-j]) );
    c.j   = log(A);
    eta.j = X %*% curr.beta[,j] - c.j;
    
    # sample omega
    curr.om[,j] = rpg.devroye(n, nn[,j], eta.j)
    
    # draw beta
    V = solve(beta_prior_var_inv + t(X) %*%  (X*curr.om[,j]) )
    b = V %*% (beta_prior_var_inv%*%beta_prior_mean[,j] +  t(X) %*% (kappa[,j] + c.j * curr.om[,j]) )
    curr.beta[,j] = mvrnorm(1,b,V)
    
    # #beta mean/var:
    # PL.j = t(X) %*% (X * curr.om[,j]);
    # bL.j = t(X) %*% (kappa[,j] + c.j * curr.om[,j]);
    # 
    # P1.j = PL.j + beta_prior_var_inv;
    # ## Can speed up using Choleksy.
    # V1.j = chol2inv(chol(P1.j));
    # m1.j = V1.j %*% (bL.j );
    # 
    # sqrtV1.j = t(chol(V1.j));
    # curr.beta[,j] = m1.j + sqrtV1.j %*% rnorm(k);
  }
  
  # we are past the burn-in, save the draws
  if (iter > ndiscard) {
    s = iter - ndiscard
    postb[,,s] = curr.beta
    curr.xb = X %*% curr.beta
    posty[,,s] = exp(curr.xb) / rowSums(exp(curr.xb))
    postom[,,s] = curr.om
  }
}

beta_mean <- apply(postb,c(1,2),mean)

results <- list(postb = postb, postbmean = beta_mean)#, posty = posty, postom = postom)
return(results)

}


















### calculate posterior mean of beta and sigma
# beta_mean_hat = apply(postb,c(1,2),mean)
# y_mean_hat = apply(posty,c(1,2),mean)
# 
# marginal_fx = array(0,c(k,p,nretain))
# dimnames(marginal_fx)[[1]] = colnames(X)
# dimnames(marginal_fx)[[2]] = colnames(Y)
# 
# meanXs = apply(X,c(2),mean)
# 
# jjj = 1
# for (jjj in 1:nretain) {
#   ppp = 1
#   
#   MU = X %*% postb[,,jjj]
#   pr = exp(MU) / rowSums(exp(MU))
#   
#   for (ppp in 1:p) {
#     
#     bbb = matrix(1,n,kk) %*% diag(postb[,ppp,jjj])  #+ vecAIW %*% t(postb[ind_WX,ppp,ind]) 
#     pr_bbb = bbb
#     for (kk in 1:k) {
#       pr_bbb[,kk] = rowSums(pr %*% diag(postb[kk,,jjj]))
#     }
#     
#     partial1 = pr[,ppp] * (bbb - pr_bbb)
#     
#     
#     marginal_fx[,ppp,jjj] = apply(partial1,c(2),mean)
#   }
# }


# 
# data.frame(
#   name = colnames(X),
#   beta = beta_mean_hat,
#   signi = beta_mean_hat / apply(postb,c(1),sd))
# 
# # Geweke convergence diagnostic
# full_chain_m = t(postb)
# mh.draws <- mcmc(full_chain_m)
# gconv = geweke.diag(mh.draws)$z ## z>3 or z<-3 indicates non-convergence!
# cat("Should be 0:",sum(abs(gconv)>3),"\n")
# 
# # plots
# par(mfrow=c(1,2))
# plot(pr,y_mean_hat,main="Posterior vs simulated probabilities",xlab="Simulated",ylab="Posterior")
# plot(MU,X %*% beta_mean_hat,
#      main="Posterior vs simulated log-odds",xlab="Simulated",ylab="Posterior")
# par(mfrow=c(1,1))
# 
