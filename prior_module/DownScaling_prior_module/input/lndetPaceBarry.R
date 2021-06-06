
lndetPaceBarry <- function(W,qq = NULL,length.out = 200){
  # require(matrixcalc)
  rmin=-1 # <---- CHANGE: perhaps rmin=1e-5, to produce results only for 0 < rho < 1
  rmax=1 # range of rho
  order=50
  iter=30 # <--- CHANGE: tuning parameters according to LeSage suggestions
  
  n=dim(W)[1]
  
  # Exact mom3ents from 1 to oexact
  td=matrix(c(0,sum(W^2)/2),length(c(0,sum(W^2)/2)),1)

  oexact=length(td)
  
  # stochastic moments
  mavmomi=matrix(0,order,iter)
  
  for(j in 1:iter)
  {
    u=matrix(rnorm(n,0,1),n,1)
    v=u
    utu=t(u)%*%u
    for (i in 1:order)
    {
      v=W%*%v
      mavmomi[i,j]=as.double(n*((t(u)%*%v)/(i*utu)))
      
    }  
  }
    mavmomi[1:oexact,]=td[,matrix(1,iter,1)]
    
  # averages across iterations
  avmomi=as.matrix(rowMeans(mavmomi))
  
  # alpha matrix
  if (is.null(qq)) {
    alpha=seq(rmin,rmax,length.out = length.out) 
  } else {
    alpha=seq(rmin,rmax,qq) 
  }
  valpha=vandermonde.matrix(alpha,length(alpha))
  alomat=-valpha[,(2:(order+1))]
  
  # Estimated ln|I-aD| using mixture of exact, stochastic moments
  # exact from 1 to oexact, stochastic from (oexact+1) to order
  
  lndetmat=alomat%*%avmomi
  
  
  return(cbind(lndetmat,alpha))
}