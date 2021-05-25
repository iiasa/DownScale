rm(list = ls())

script_dir <- ifelse(.Platform$GUI == "RStudio", dirname(rstudioapi::getActiveDocumentContext()$path), getwd())
setwd(script_dir)

library(stringr)
library(withr)
library(raster)
require(progress)
library(spdep)
library(Matrix)
library(gdxrrw)
library(tidyr)

##################################################################################
model.nr <- 1
est_class <- c("CrpLnd","Forest", "Grass", "OthNatLnd")
spec.nr <- 1
ex.name <- "DownScaling"
cluster.nr.reg30 <- 1615  #1615 bei reg30, 1622 bei reg 37
cluster.nr.reg37 <- cluster.nr.reg30+7  #1615 bei reg30, 1622 bei reg 37
mean.save <- TRUE

class.nr <- length(est_class)
run.nr <- model.nr*class.nr*30*spec.nr

#if in mac os x
if (Sys.info()[1]!="Windows") {
  igdx("/Applications/GAMS24.7/sysdir")
} else {
  igdx("C:/GAMS/win64/29.1")
}



explanatories <- c("X", "Y", "CrpLnd", "Grass", "PriFor", "MngFor", "PltFor", "OthNatLnd", "urban",
                   "MeanTemp", "MeanPrecip", "HarvWood", "HarvCost", "GRASYLD", "meanTimeToMarket",
                   "totPop", "ruralPop", "Altitude", "Slope", "Soil2_Medium", "Soil3_Heavy", "Soil4_Stony", "Soil5_Peats",
                   "Barl", "BeaD", "Cass", "ChkP", "Corn", "Cott",  "Gnut", "Mill", "Pota","Rape", "Rice", "Soya", 
                   "Srgh", "SugC", "Sunf", "SwPo", "Whea", "gdp_base")

explanatories <- c("Intercept", explanatories)



pb2 <- progress_bar$new(
  format = "  [:bar] Loading Data :percent in :eta",
  total = run.nr, clear = FALSE, width= 60)


########



for (i in 1:run.nr){
  load(paste0("./output/output_",ex.name,"_",cluster.nr.reg30,".",with_options(
    c(scipen = 999), 
    str_pad(i, 6, pad = "0")
  ),".RData"))
  
  betas <- data.frame(est_res$postbmean)
  colnames(betas) <- est_class
  results <- data.frame(region=rep(est_res$region, nrow(betas)), lufrom=rep(est_res$class, nrow(betas)), variable=explanatories, betas) 
  results
  
  if(i==1){
    results.big <- results
  } else {
    results.big <- rbind(results.big,results)
  }
  pb2$tick()
}
results.big.30 <- results.big



run.nr <- model.nr*class.nr*37*spec.nr

pb2 <- progress_bar$new(
  format = "  [:bar] Loading Data :percent in :eta",
  total = run.nr, clear = FALSE, width= 60)

for (i in 1:run.nr){
  load(paste0("./output/output_",ex.name,"_",cluster.nr.reg37,".",with_options(
    c(scipen = 999), 
    str_pad(i, 6, pad = "0")
  ),".RData"))
  
  betas <- data.frame(est_res$postbmean)
  colnames(betas) <- est_class
  results <- data.frame(region=rep(est_res$region, nrow(betas)), lufrom=rep(est_res$class, nrow(betas)), variable=explanatories, betas) 
  results
  
  if(i==1){
    results.big <- results
  } else {
    results.big <- rbind(results.big,results)
  }
  pb2$tick()
}  

results.big.37 <- results.big

data <- rbind(results.big.37, results.big.30[!results.big.30$region%in%results.big.37$region,])
colnames(data)[c(1,2,3)] <- c("ANYREGION", "LC_TYPE_EPIC_FROM", "coeff_variable")
attr(data, "symName") <- "luc_downscl_coeff"




##################################################################################


symDim <- 4
lst <- wgdx.reshape(data, symDim, symName = "luc_downscl_coeff", tName = "LC_TYPE_EPIC_TO")
lst[[5]] <- lst[[5]][,c(1,2,4,3,5)]
attr(lst[[5]], "symName") <- "luc_downscl_coeff"
wgdx.lst(paste0("../../source/betas.gdx"), lst)
