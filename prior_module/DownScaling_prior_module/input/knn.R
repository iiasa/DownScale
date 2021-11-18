#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(FNN,spam,Matrix)

getWknn<-function(xy,k) {
  #construct Wmatrix
  n<- NROW(xy)
  nn<-knn.index(xy,k=k)
  W<-spam(0,n,n)
  cseq = c(1:n)
  for (i in 1:k) {
    tmp1<-cbind( cseq,nn[,i],1/k)
    tmp2<-cbind( cseq,cseq,0)
    tmp3<-rbind(tmp1,tmp2)
    if (i==1) {
      W<-sparseMatrix(i=tmp3[,1],j=tmp3[,2],x=tmp3[,3])
    } else {
      W <- W + sparseMatrix(i=tmp3[,1],j=tmp3[,2],x=tmp3[,3])
    }  
  }
  diag(W) = 0
  return(W)
}

# for flexible W-matrix
constructW<-function(Ws,phi) {
  phisum = sum(phi^c(1:length(Ws)))
  W = (phi * Ws[[1]]) / phisum
  for (i in 2:length(Ws)) {
    W = W + (phi^i * Ws[[i]])/phisum
  }
  return(W)
}