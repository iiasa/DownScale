rm(list = ls())

#### define path and load relevant packages
environment(.libPaths)$.lib.loc <- c(environment(.libPaths)$.lib.loc,"H:/R/win-library/3.5")
script_dir <- ifelse(.Platform$GUI == "RStudio", dirname(rstudioapi::getActiveDocumentContext()$path), getwd())
setwd(script_dir)


args <- commandArgs(trailingOnly=TRUE)
JOBS <- ifelse(.Platform$GUI == "RStudio",88,as.integer(args[[1]]))
dir.create("output")

library(MASS)
library(truncnorm)
require(bayesm)
library(SparseM)
library(Matrix)
library(BayesLogit)
library(stringr)
library(spdep)
library(FNN)
library(spam)
library(mvtnorm)
library(matrixcalc)



# define setup variables for estimation


#models <- c("MNL", "logit", "SUR", "SAR", "SEM")
models <- "MNL"
#X_specs <- c("Xonly", "XWX")
X_specs <- "Xonly"
totit <- 2000
brunit <- 1000
threshold <- 45
W1 <- "itself"

regio <- "REGION" #"REGION"


explanatories <- c("X", "Y", "CrpLnd", "Grass", "PriFor", "MngFor", "PltFor", "OthNatLnd", "urban",
                   "MeanTemp", "MeanPrecip", "HarvWood", "HarvCost", "GRASYLD", "meanTimeToMarket",
                   "totPop", "ruralPop", "Altitude", "Slope", "Soil2_Medium", "Soil3_Heavy", "Soil4_Stony", "Soil5_Peats",
                   "Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott",  "Gnut", "Mill", "Pota","Rape", "Rice", "Soya", 
                   "Srgh", "SugC", "Sunf", "SwPo", "Whea", "gdp_base")
#excluded for non singularity of XpXi "Soil2_Medium", "Soil3_Heavy", "Soil4_Stony", "Soil5_Peats", tot_permSnowIce
#load data
load("./input/xy_dat_May18.RData")
load("./input/W_region37.RData")

#define estimation curr.classes (names must match names of data)
est_class <- c("CrpLnd","Forest", "Grass", "OthNatLnd")
est_class_from <- c("CrpLnd","Forest", "Grass", "OthNatLnd")

regions <- as.character(unique(X.raw[,regio]))
regions <- regions[!regions=="0"]


data <- cbind(SIMUID=X.raw$SIMUID, REGION=X.raw[,regio],X.raw[,explanatories],Y.raw[,-1])


#load relevant functions
source("./input/MNL_logit.R")
source("./input/lndetPaceBarry.R")
source("./input/knn.R")
# source("./input/logit.R")
# source("./input/SUR.R")
# source("./input/SUR_spatial.R")
# source("./input/SUR_SEM.R")

clustergrid <- expand.grid(regions,est_class_from)
clustergrid <- apply(clustergrid,2,as.character)

#define Job specs
region <- clustergrid[JOBS,1]
model <- models
X_spec <- X_specs
curr.class <- clustergrid[JOBS,2]



#subset data for relevant job
tempdata <- subset(data, data$REGION==region)

transitions <- paste0(curr.class,".",est_class)

columns <- seq(1,4,1)
for(jjj in transitions){
  columns <- c(columns,grep(jjj, colnames(tempdata)))
}
tempdata <- tempdata[,columns]




#correction if number of transitions smaller than threshold

tempdata.class <- tempdata
colnames(tempdata.class) <- sub("\\..*", "", colnames(tempdata.class))
faultclass <- t(rowsum(as.numeric(colSums(tempdata.class!=0)<=threshold), group=names(tempdata.class)))


faultclass <- c(faultclass)[1]


indic <- 1
while(faultclass!=0){
  indic <- indic+1
  neighbours <- W.new[region,indic]
  neighbours <- row.names(W.new)[neighbours]
  
  W1 <- c(W1, neighbours)
  
  tempdata2 <- subset(data, data$REGION%in%neighbours)
  tempdata2 <- tempdata2[,columns]
  
  
  tempdata.class <- rbind(tempdata,tempdata2)
  colnames(tempdata.class) <- sub("\\..*", "", colnames(tempdata.class))
  faultclass <- t(rowsum(as.numeric(colSums(tempdata.class!=0)<=threshold), group=names(tempdata.class)))
  
  
  faultclass <- c(faultclass)[1]
}


all.sim <- tempdata.class$SIMUID




#collecting y variables for estimation
y <- Y.raw[Y.raw$SIMUID%in%all.sim, c("SIMUID",transitions)]

y <- y[rowSums(y[,2:ncol(y)])!=0,]

est.sim <- y$SIMUID
y$SIMUID <- NULL


#extract simu spatial data
longlat <- as.matrix(X.raw[X.raw$SIMUID%in%est.sim, c("X","Y")])
W <- getWknn(longlat,15)





#realtive changes
y[is.na(y)] <- 0
y <- y/rowSums(y)




#### choose baseline
baseline <- which(est_class_from%in%curr.class)



X <- as.matrix(X.raw[X.raw$SIMUID%in%est.sim, explanatories])
intercept <- rep(1,nrow(X))

# if(X_spec=="Xonly"){
X <- as.matrix(cbind(intercept,X))
# } else if(X_spec=="XWX"){
#   X <- as.matrix(cbind(intercept,X, W %*% X))
# }


if(X_spec=="Xonly"){
  X <- as.matrix(X)
} else if(X_spec=="XWX"){
  X <- as.matrix(cbind(X, W %*% X))
}


y <- as.matrix(y)
if(model=="MNL"){
  est_res <- bayes.MNlogit(X,y,baseline,totit,brunit)
} else if(model=="SUR") {
  est_res <- bayes.SUR.simple(X,y,totit,brunit)
} else if(model=="SEM") {
  W.sem <- getWknn(longlat,7)
  est_res <- bayes.SUR.SAR(X,y,totit,brunit,W.sem)
} else if(model=="SAR") {
  W.sar <- getWknn(longlat,7)
  est_res <- bayes.SUR.SAR(X,y,totit,brunit,W.sar)
} else if(model=="logit") {
  est_res <- list()
  for(i in 1:ncol(y)){
    est_res[[i]] <- bayes.logit(X,as.numeric(y[,i]),totit,brunit)
    names(est_res)[i] <- paste0(est_class_from[i])
  }
}

est_res[["region"]] <- region
est_res[["est.sim"]] <- est.sim
est_res[["all.sim"]] <- all.sim
est_res[["class"]] <- curr.class
est_res[["model"]] <- model
est_res[["X_spec"]] <- X_spec
est_res[["neighbours"]] <- W1

save(est_res, file="output/output.Rdata")
