# main downscaling script for the GLOBIOM-G4M-Link pipeline
# Last update: 2025 Apr 13

# 1. Load packages (from /renv/) -----

environment(.libPaths)$.lib.loc = c(
  # "renv/library/R-4.0/x86_64-w64-mingw32",
  "renv/library/R-4.2/x86_64-w64-mingw32", # YW 20251230: now use R-4.2 (as limpopo is also R-4.2) and downscalr pkg version 2023 August, which is more updated than the one in R-4.0 (= a 2022 version of downscalr)
  environment(.libPaths)$.lib.loc)

require("gdxrrw")
require(tidyr)
require(dplyr)
require(downscalr)
require(tidyverse)

print("==============Session Info====================")
sessionInfo()
print("==============End Session Info====================")

# 2. Read downscaling parameters -----
parameters <- readRDS("downscaling_pars.RData")

## parameter for starting maps
ISIMIP <- parameters[[1]] #to be kept as FALSE except for ISIMIP scens // TRUE -> changes starting maps
## parameter for running locally or in limpopo
cluster <- parameters[[2]] #if TRUE run and scengrid must be passed on from GAMS
## parameter for scenario mapping
scenario_mapping <- parameters[[5]]
## parameter for region resolution
REGION_RESOLUTION <- parameters[[6]]
## parameter for BTC scenarios
THERE_ARE_BTC_Scenarios <- parameters[[7]]
if(THERE_ARE_BTC_Scenarios){
  SCENARIOS_BTC <- parameters[[8]]
}else{
  SCENARIOS_BTC <- NULL
}

# run <- run.nr #from GAMS
# scengrid <- AllscenLoop #from GAMS

if(cluster){
  #=================#
  # to be changed by the modeler based on current project
  project <- parameters[[3]] #as in GAMS
  lab <- parameters[[4]] #as in GAMS
  # GAMSPath = c("C:/GAMS/win64/32.2")
  GAMSPath <- c("C:/GAMS/42/") #According to the current (updated) GAMS path in limpopo
  igdx(GAMSPath)
  gdx_path <- "./gdx/downscaled.gdx" #output location of downscaling result (Land_Cover_SU_Region_SCEN)
  #=================#
  args <- commandArgs(trailingOnly=TRUE)
  run <- as.integer(args[[1]])
#  dir.create("output")

} else {
  #=================#
  # to be changed by the modeler based on current project
  project <- parameters[[3]] #as in GAMS
  lab <- parameters[[4]] #as in GAMS
#  GAMSPath = c("C:/GAMS/win64/29.1") #if run on cluster/ need limpopo GAMS path
  gdx_path <- "./gdx/downscaled.gdx" #output location of downscaling result (Land_Cover_SU_Region_SCEN)
  #=================#
}



# 3. Read shared inputs GDX/GMS/csv -----

## (1) LC, LUC, and spatial information -----

### reg 59 mapping generated with new file create_mapping in DownScale git
### full.map | SimUID - ALLCOUNTRY - REGION59 - REGION(37)
full.map <- readRDS("./source/simu_region_country.rds")
## Note that in region37 version, as in GLOBIOM/G4M, UK should still be in EU_North instead of ROWE. So renaming the REGION(37) for UK to be in line with GLOBIOM and G4M
full.map$REGION[full.map$ALLCOUNTRY=="UK"] = "EU_North"


### initial LU level, currently not directly used in DS function, but is used for checking DS result
LANDCOVER_COMPARE_SCEN <-
  rgdx.param(file.path(paste0(
    "input/output_landcover_", project, "_", lab
  )), "LANDCOVER_COMPARE_SCEN") %>%
  setNames(c(
    "REGION",
    "lu.class",
    "SCEN1",
    "SCEN2",
    "SCEN3",
    "year",
    "value"
  )) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

LUC_COMPARE_SCEN0 <-
  rgdx.param(file.path(paste0(
    "input/output_landcover_", project, "_", lab
  )), "LUC_COMPARE_SCEN0") %>%
  setNames(
    c(
      "REGION",
      "lu.from",
      "lu.to",
      "SCEN1",
      "SCEN2",
      "SCEN3",
      "year",
      "value"
    )
  ) %>%  mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year))) %>%
  mutate(SCEN1=toupper(SCEN1),SCEN2=toupper(SCEN2),SCEN3=toupper(SCEN3))

Price_Compare2 <-
  rgdx.param(file.path(paste0(
    "input/output_landcover_", project, "_", lab
  )), "Price_Compare2") %>%
  setNames(c(
    "CROP",
    "REGION",
    "SCEN1",
    "SCEN2",
    "SCEN3",
    "year",
    "value"
  )) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

## (2) Socioeconomic data (independent vars) -----
transportation <-
  read.csv(file = "./source/acc_mean_travel_minutes_simu.csv")

grasyield <- read_table("source/data_GrasYield_X.gms")
grasyield <-
  grasyield[, c(1, 2)] %>% `colnames<-`(c("SimUID", "grasyield"))
grasyield$SimUID <- gsub("\\..*", "", grasyield$SimUID)
grasyield$SimUID <- gsub("\\/24", "24", grasyield$SimUID) # Address the 1st line when reading in grasyield
grasyield <- na.omit(grasyield)

MngForest_Param <-
  rgdx.param(file.path(paste0("source/Forestparameters")), "MngForest_Param") %>%
  setNames(c("SimUID", "junk" , "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

pop1 <- rgdx.param(file.path(paste0("source/pop_SSP1")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop2 <- rgdx.param(file.path(paste0("source/pop_SSP2")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop3 <- rgdx.param(file.path(paste0("source/pop_SSP3")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop4 <- rgdx.param(file.path(paste0("source/pop_SSP4")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop5 <- rgdx.param(file.path(paste0("source/pop_SSP5")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

gdp <- rgdx.param(file.path(paste0("source/gdp")), "gdp") %>%
  setNames(c("SimUID", "SCEN1" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

YLD_SSP_STAT <-
  rgdx.param(file.path(paste0(
    "input/output_landcover_", project, "_", lab
  )), "YLD_SSP_STAT") %>%
  setNames(c("SCEN1" , "REGION", "CROP", "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

YLD_SSP_STAT <-
  expand.grid(SCEN1 = unique(YLD_SSP_STAT$SCEN1), year = unique(YLD_SSP_STAT$year), REGION = unique(YLD_SSP_STAT$REGION), CROP=unique(YLD_SSP_STAT$CROP)) %>% left_join(YLD_SSP_STAT) %>% mutate(value=ifelse(is.na(value),1,value))


## (3) init_xmat ------

init_xmat <-
  rgdx.param(file.path(paste0("source/Xmat")), "xmat") %>%
  setNames(c("SimUID" , "REGION", "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

if(REGION_RESOLUTION==59){
 ### Read in Xmat.gdx and remap SimuID to REGION59
 init_xmat <-  init_xmat %>% 
   ungroup() %>% dplyr::select(-REGION) %>% left_join(full.map %>% ungroup() %>% dplyr::select(SimUID, REGION59) %>% rename("REGION"="REGION59"))
}else{ 
 #if(REGION_RESOLUTION==37)
 ### Read in Xmat.gdx and remap SimuID to REGION37
  init_xmat <-  init_xmat %>% 
    ungroup() %>% dplyr::select(-REGION) %>% left_join(full.map %>% ungroup() %>% dplyr::select(SimUID, REGION))
  }

### Remove duplications in init_xmat
init_xmat <- init_xmat %>%
  distinct(.keep_all = TRUE)


## (4) LUC_Fin, SRP_Suit, trans_factors, luc_downscl_coeff, AREA, Yield_Simu  ------
LUC_Fin <-
  rgdx.param(file.path(paste0("source/LUC_Fin_Write_SSP2_msg07Ukraine37R")), "LUC_Fin") %>%
  setNames(c("SimUID", "lu.class", "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value)) %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())


SRP_Suit <-
  rgdx.param(file.path(paste0("source/X_4Tatiana")), "SRP_suit") %>%
  setNames(c("SimUID", "country", "SRP_class", "value")) %>% subset(SRP_class ==
                                                                      "SRP_NPP") %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))  %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())

trans_factors <-
  rgdx.param(file.path(paste0("source/Xmat")), "trans_factors") %>%
  setNames(c("variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

luc_downscl_coeff <-
  rgdx.param(file.path(paste0("source/betas")), "luc_downscl_coeff") %>%
  setNames(c("REGION", "lu.from", "lu.to", "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))
########### reg 59 mapping adapted here
if(REGION_RESOLUTION==59){
luc_downscl_coeff <- unique(full.map %>% dplyr::select(REGION, REGION59)) %>% full_join(luc_downscl_coeff) %>% dplyr::select(-REGION) %>% rename("REGION"="REGION59")
}

AREA <-
  rgdx.param(file.path(paste0("source/X_4Tatiana")), "Area") %>%
  setNames(c("SimUID", "country", "CROP", "mgmt_sys", "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value)) %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())

Yield_Simu <-
  rgdx.param(file.path(paste0("source/yields")), "Yield_Simu") %>%
  setNames(c("SimUID", "country", "CROP", "mgmt_sys" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))  %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())


# 4. Scenario-specific DownScaling: incl. prepartaion of projections/restrictions & other for DS for each scenario-----
REGION <- unique(LUC_COMPARE_SCEN0$REGION)[!unique(LUC_COMPARE_SCEN0$REGION)=="World"]
scengrid <- parameters[[5]] %>% mutate(REGION=rep(REGION,length(unique(parameters[[5]]$ScenLoop))))

if(!cluster){
  scenarios <- which(scengrid$SCEN1=="SSP1" & scengrid$SCEN2=="SPA0" & scengrid$SCEN3=="scenRCPref")
} else {
  scenarios <- run
}

## (1) initialize res -----
res <- list()
res[["out.res"]] <- NULL

## (2) starting scenario loop -----
for(scen in scenarios){
  idx <- which(scengrid$ScenNr==scen)
  curr.SCEN1 <- scengrid$SCEN1[idx]
  curr.SCEN2 <- scengrid$SCEN2[idx]
  curr.SCEN3 <- scengrid$SCEN3[idx]
  rrr <- scengrid$REGION[idx]

### (i) crop area and starting maps -----
  ## crop area map
  area_crop_unit_input = subset(AREA, REGION == rrr) %>% rename(area = value) %>%
    left_join(
      subset(Yield_Simu, REGION == rrr) %>%
        dplyr::select(SimUID, CROP, mgmt_sys, value) %>%
        rename(yield = value),
      by = c("SimUID", "mgmt_sys", "CROP")
    )
  sum_crop_area = area_crop_unit_input %>% group_by(SimUID) %>% summarise(area = sum(area))


  if(ISIMIP==FALSE){
    init.areas <-
      LUC_Fin %>% subset(REGION == rrr) %>% dplyr::select(!REGION) %>% subset(lu.class != "SimUarea") %>%
      rename(ns = SimUID, lu.from = lu.class)  %>% mutate(value = ifelse(value < 0.00001, 0, value))

  } else {
    init.areas <-
      read.csv(file="source/SimU_LU_biodiv_G4M_jan19.csv") %>%
      mutate(SimUID=row.names(.)) %>% subset(SimUID%in%unique((LUC_Fin %>% subset(REGION == rrr))$SimUID))

    cropsys <-
      read.csv(file="source/cropsys.csv") %>%
      mutate(SS_area=rowSums(across(ends_with("SS"))), glob_crop=rowSums(across(ends_with(c("HI","LI","IR"))))) %>%
      dplyr::select(SIMUID,SS_area,glob_crop) %>% rename(SimUID=SIMUID) %>% mutate(SimUID=as.character(SimUID))

    init.areas <- init.areas %>% left_join(cropsys) %>%
      mutate(arable=cropland-SS_area-glob_crop) %>%
      mutate(arable=ifelse(arable<0,0,arable)) %>%
      mutate(cropland=glob_crop) %>%
      dplyr::select(!c(glob_crop, restored))  %>%
      pivot_longer(cols = c(!SimUID), names_to = "lu.from", values_to = "value") %>%
      rename(ns=SimUID) %>% mutate(lu.from=recode(lu.from,"cropland"="CrpLnd",
                                                  "grassland"="Grass",
                                                  "other"="OthNatLnd",
                                                  "priforest"="PriFor",
                                                  "SRP"="PltFor",
                                                  "mngforest"="MngFor"))


  }

  #YW 20251230 update: try using BTC layer for all scenarios, so that to ensure the downscaled results are the same until 2020.
  # Further update the initial LC map: will use the updated init.areas.CSV (not the default LUC.Fin above) and further process it depending on BTC activated or not
  USE_New_Initial_Map_for_BTC_compatible <- TRUE
  if(USE_New_Initial_Map_for_BTC_compatible){
    init.areas.CSV <-
      read.csv(file="source/SimU_LU_biodiv_G4M_jan19_YWaddRst0.csv") %>%
      mutate(SimUID=row.names(.)) %>% subset(SimUID%in%unique((LUC_Fin %>% subset(REGION == rrr))$SimUID))

    ## filter all BTC scenarios in scenario mapping:
    scenario_name_DS_wBTC <- scenario_mapping %>%
      filter(ScenLoop %in% SCENARIOS_BTC) %>%
      dplyr::select(-c(RegionName, ScenNr,ScenLoop)) %>%
      distinct() %>%
      mutate(Scenario=paste0(SCEN1,"_",SCEN2,"_",SCEN3))

      init.areas <- init.areas.CSV %>%
        dplyr::select(!c( restored, urban))  %>%
        mutate(Forest=priforest+mngforest) %>%
        dplyr::select(!c( priforest, mngforest))  %>%
        pivot_longer(cols = c(!SimUID), names_to = "lu.from", values_to = "value") %>%
        mutate(value=value*0.001) %>%
        rename(ns=SimUID) %>% mutate(lu.from=recode(lu.from,"cropland"="CrpLnd",
                                                    "grassland"="Grass",
                                                    "other"="OthNatLnd",
                                                    "priforest"="PriFor",
                                                    "SRP"="PltFor",
                                                    "mngforest"="MngFor"))
  }

  ### (ii) xmat -----
  xmat <- init_xmat %>% subset(REGION == rrr) %>%
    rename(ns = SimUID, ks = variable) %>% dplyr::select(-REGION)#explanatory variables

 ###complete xmat: filling na with 0
  xmat <- unique(merge(unique(init.areas$ns), unique(xmat$ks)) %>% rename("ns"="x", "ks"="y") %>% left_join(xmat) %>% mutate(value=ifelse(is.na(value),0,value)))

  ### (iii) curr.SRP_Suit (exogenous priors for PltFor, RstLnd) -----
  curr.SRP_Suit <- SRP_Suit %>% subset(REGION == rrr) %>%
    mutate(value = ifelse(value == 0, .0001, value)) %>%
    mutate(value = value / max(value)) %>%
    rename(ns = SimUID) %>% dplyr::select(ns, value)
  curr.SRP_Suit = bind_cols(lu.from = "CrpLnd", lu.to = "PltFor", curr.SRP_Suit) %>%
    bind_rows(bind_cols(lu.from = "Grass", lu.to = "PltFor", curr.SRP_Suit)) %>%
    bind_rows(bind_cols(lu.from = "OthNatLnd", lu.to = "PltFor", curr.SRP_Suit)) %>%
    bind_rows(
      bind_cols(lu.from = "PltFor", lu.to = "OthNatLnd", curr.SRP_Suit) %>%
        mutate(value = 1 / value) %>% mutate(value = value / max(value))
    )
  
  if(REGION_RESOLUTION==37){
    prior_RstLnd <- read.csv(file="source/RstLnd_prior_10Dec2018_YWaddPltFor.csv") %>% left_join(full.map %>% mutate(SimUID=as.integer(SimUID))) %>% select(-c(ALLCOUNTRY,REGION59)) %>% subset(REGION == rrr) %>% rename(ns=SimUID)
  }
  if(REGION_RESOLUTION==59){
    prior_RstLnd <- read.csv(file="source/RstLnd_prior_10Dec2018_YWaddPltFor.csv") %>% left_join(full.map %>% mutate(SimUID=as.integer(SimUID))) %>% select(-c(ALLCOUNTRY,REGION)) %>% dplyr::rename(REGION=REGION59) %>% subset(REGION == rrr) %>% rename(ns=SimUID)
  }
  
  prior_RstLnd1 <- prior_RstLnd %>%
    dplyr::select(c(ns,prior_CrpLnd_RstLnd))%>% mutate(lu.from="CrpLnd") %>% mutate(lu.to="RstLnd") %>% rename(value=prior_CrpLnd_RstLnd) %>%
    dplyr::select(lu.from,lu.to,ns,value)
  
  prior_RstLnd2 <- prior_RstLnd %>%
    dplyr::select(c(ns,prior_GrsLnd_RstLnd))%>% mutate(lu.from="Grass") %>% mutate(lu.to="RstLnd") %>% rename(value=prior_GrsLnd_RstLnd) %>%
    dplyr::select(lu.from,lu.to,ns,value)
  
  prior_RstLnd3 <- prior_RstLnd %>%
    dplyr::select(c(ns,prior_GrsLnd_RstLnd))%>% mutate(lu.from="PltFor") %>% mutate(lu.to="RstLnd") %>% rename(value=prior_GrsLnd_RstLnd) %>%
    dplyr::select(lu.from,lu.to,ns,value)
  
  curr.SRP_Suit <- rbind(curr.SRP_Suit,prior_RstLnd1,prior_RstLnd2,prior_RstLnd3)
  
  ###complete curr.SRP_Suit: filling na with 0 (otherwise err.check.input will report error)
  curr.SRP_Suit <- merge(unique(xmat$ns), unique(curr.SRP_Suit[,c(1,2)])) %>% rename("ns"="x") %>% left_join(curr.SRP_Suit %>% mutate(ns=as.character(ns))) %>% mutate(weight=ifelse(is.na(value),0,1), value=ifelse(is.na(value),0,value))
  
  
  ### (iv) DDelta (downscling targets) -----
  DDelta <-
    LUC_COMPARE_SCEN0 %>% filter(lu.from != "MngFor" &
                                   lu.to != "PriFor") %>%
    mutate(
      value = ifelse(value < 0.00001, 0, value),
      lu.from = recode(
        lu.from,
        "NatLnd"= "OthNatLnd",
        "GrsLnd"="Grass",
        "PltArt"="PltFor"),
      lu.to = recode(
        lu.to,
        "NatLnd"= "OthNatLnd",
        "GrsLnd"="Grass",
        "PltArt"="PltFor")) %>%
    subset(
      REGION == rrr &
        SCEN1 == curr.SCEN1 &
        SCEN2 == curr.SCEN2 & SCEN3 == curr.SCEN3
    ) %>%
    dplyr::select(c(lu.from, lu.to, year, value)) %>% unique() %>% rename(times = year) %>%
    ungroup() %>% group_by(lu.from, lu.to, times) %>% summarise(value=sum(value)) %>% ungroup() #targets %>%
  # mutate(value= value*1000)

  #### temporary bug fix, creates dummy transitions to represent all years (as otherwise because some regions may have zero LUC in some years, there will be missing land cover results in this year in GLOBIOM_LC):
  missing.year <- seq(2010,2100,10)[which(!seq(2010,2100,10) %in% unique(DDelta$times))]
  if(length(missing.year)!=0){
    for(yy in missing.year)
      DDelta <- DDelta %>% bind_rows(data.frame(lu.from="OthNatLnd", lu.to="Forest", times=yy, value=0.00001))
  }

  ### (v) betas, restrictions, curr.crop_projections -----
  betas <- luc_downscl_coeff %>% subset(REGION == rrr) %>%
    rename(ks = variable) %>% dplyr::select(-REGION)  %>% subset(ks %in% unique(xmat$ks))

  lu.from <- unique(DDelta$lu.from)
  lu.to <- unique(DDelta$lu.to)

  restrictions = sum_crop_area %>%
    mutate(area = ifelse(area > 0, 0, 1)) %>%
    rename(ns = SimUID, value = area) %>%
    right_join(init.areas %>% dplyr::select(ns) %>% distinct()) %>%
    tidyr::replace_na(list(value = 1)) %>%
    expand_grid(
      DDelta %>%
        distinct(lu.from) %>%
        bind_cols(lu.to = "CrpLnd") %>% filter(lu.to != lu.from)
    )

  # YW below two lines can be used in the future to complete the curr.crop_projections, when yield for specific crop-mngmnt doesn't exist for an ns in YLD_SSP_STAT, then use the country average for this crop-mngmnt for this ns. (but not implemented yet)
  Yield_Simu_average <- Yield_Simu %>% 
    group_by(REGION,country, CROP, mgmt_sys) %>% 
    summarise(value_countryAve=mean(value,na.rm=TRUE)) %>% 
    ungroup()
  Yield_Simu0 <- Yield_Simu
  
  curr.crop_projections = subset(AREA, REGION == rrr) %>% rename(area = value) %>%
    left_join(
      subset(Yield_Simu, REGION == rrr) %>% dplyr::select(SimUID, CROP, mgmt_sys, value) %>%
        rename(yield = value),
      by = c("SimUID", "mgmt_sys", "CROP")
    ) %>%
    left_join(
      subset(YLD_SSP_STAT, REGION == rrr &
               SCEN1 == curr.SCEN1) %>%
        bind_rows(
          subset(YLD_SSP_STAT, REGION == rrr &
                   SCEN1 == curr.SCEN1) %>% expand(CROP, year =
                                                             2000, value = 1) # YW this causes NA, maybe need to add 'REGION=rrr' in expand()
        ) %>%
        rename(shifters = value),
      by = c("CROP", "REGION")
    ) %>%
    left_join(
      subset(
        Price_Compare2,
        REGION == rrr & SCEN1 == curr.SCEN1 &
          SCEN2 == curr.SCEN2 &
          SCEN3 == curr.SCEN3
      ) %>% rename(price = value)
    ) %>%
    left_join(trans_factors %>% rename(transfact = value, CROP = variable)) %>%
    mutate(value = area * yield * shifters * price * transfact) %>%
    group_by(SimUID, CROP, year) %>% summarise(value = sum(value))  %>%
    rename(ns = SimUID, ks = CROP, times = year)

  curr.gdp_projections = subset(gdp,
                                SimUID %in% unique(init.areas$ns) &
                                  SCEN1 == curr.SCEN1) %>%
    mutate(
      value = value * subset(trans_factors, variable == "gdp_base")$value,
      ks = "gdp_base"
    ) %>%
    dplyr::select(-SCEN1) %>% rename(ns = SimUID, times = year)
  if (curr.SCEN1 == "SSP1") {
    curr.pop_projections = pop1 %>%  dplyr::select(-SCEN1)
  } else if (curr.SCEN1 == "SSP3") {
    curr.pop_projections = pop3 %>%  dplyr::select(-SCEN1)
  } else if (curr.SCEN1 == "SSP4") {
    curr.pop_projections = pop4 %>%  dplyr::select(-SCEN1)
  } else if (curr.SCEN1 == "SSP5") {
    curr.pop_projections = pop5 %>%  dplyr::select(-SCEN1)
  } else {
    curr.pop_projections = pop2 %>%  dplyr::select(-SCEN1)
  }
  curr.pop_projections = subset(curr.pop_projections, SimUID %in% unique(init.areas$ns)) %>%
    mutate(variable = recode(variable, "Rural" = "ruralPop", "Total" = "totPop")) %>%
    left_join(trans_factors %>% rename(transfact = value)) %>%
    mutate(value = value * transfact) %>%
    dplyr::select(-transfact) %>% rename(ns = SimUID, times = year, ks = variable)

  curr.projections = bind_rows(curr.crop_projections,
                               curr.gdp_projections,
                               curr.pop_projections)
  curr.projections = curr.projections  %>% right_join(expand_grid(
    ns = unique(init.areas$ns),
    ks = unique(curr.projections$ks),
    times = unique(curr.projections$times)
  )) %>%
    tidyr::replace_na(list(value = 0))


  xmat.coltypes = data.frame(ks = c("CrpLnd", "Grass", "Forest", "OthNatLnd"),
                             value = "dynamic")
  xmat.coltypes = xmat.coltypes %>%
    bind_rows(data.frame(ks = unique(xmat$ks)[which(unique(xmat$ks) %in% unique(curr.projections$ks))], value = "projected"))

  
## (3) downscale() here: -----
  
cat("start downscale() process ------")
  temp.res <-
    downscale(
      targets = DDelta %>% mutate(lu.from=recode(lu.from,"PriFor"="Forest"),
                                  lu.to  =recode(lu.to  ,"MngFor"="Forest")),
      start.areas = init.areas,
      xmat = xmat,
      betas = betas,
      priors = curr.SRP_Suit,
      areas.update.fun = areas.sum_to,
      xmat.coltypes = xmat.coltypes,
      xmat.proj = curr.projections,
      restrictions = restrictions
    )

  
  ## write output to res 
  cat("downscale() process finished. Process output ------\n")

  # res$out.res <- res$out.res %>% bind_rows(temp.res$out.res)
  res$out.res <- res$out.res %>% bind_rows(temp.res$out.res %>% mutate(REGION=rrr))

  # chck.targets.tot =   DDelta %>%
  #   left_join( res$out.res %>%
  #                group_by(lu.from,lu.to,times) %>%
  #                summarize(downscale.value = sum(value, na.rm=T),.groups = "keep"),by = c("lu.from", "lu.to","times") ) %>%
  #   mutate(diff = value - downscale.value) %>% filter(!is.na(diff))
  #
  # chck.targets.tot = chck.targets.tot %>%
  #   group_by(lu.from,lu.to,times) %>%
  #   summarise(value=sum(value),
  #             downscale.value=sum(downscale.value),
  #             diff=sum(diff))
  # if (scen==scenarios[1]) {
  #   check.targets = bind_cols(REGION = rrr, chck.targets.tot)
  # } else {
  #   check.targets = bind_rows(check.targets,bind_cols(REGION = rrr, chck.targets.tot))
  # }
  
} ## end: LOOP for scen (scenarios)


# 5. Check Downscaled results -----

## (1) Check LUC -----
## compare target and sum downscaled LUC
target_check = DDelta %>% mutate(lu.from=recode(lu.from,"PriFor"="Forest"),lu.to  =recode(lu.to  ,"MngFor"="Forest")) %>%
  bind_cols(REGION = rrr)  %>%
  mutate(SCEN1=curr.SCEN1,SCEN2=curr.SCEN2,SCEN3=curr.SCEN3) %>%
  # relocate("REGION",.before = "lu.from")
  relocate("value",.after = "SCEN3") %>%
  rename(GLOBIOOM.value=value)

ds.result.sum <- res$out.res %>%
  group_by(lu.from,lu.to,times) %>%
  summarize(downscale.value = sum(value),.groups = "keep") %>%
  bind_cols(REGION = rrr)  %>%
  mutate(SCEN1=curr.SCEN1,SCEN2=curr.SCEN2,SCEN3=curr.SCEN3) %>%
  relocate(downscale.value,.after = "SCEN3")

chck.DS.targets =   target_check %>%
  left_join(ds.result.sum,by = c("REGION","lu.from","lu.to","times","SCEN1","SCEN2","SCEN3") ) %>%
  relocate(REGION,.before = "lu.from") %>%
  mutate(diff = downscale.value - GLOBIOOM.value)

## (2) Check LC -----
## compare GLOBIOM LC and sum LC, end of period
LC_for_check <- LANDCOVER_COMPARE_SCEN%>% filter(lu.class!= "TotLnd") %>%
  mutate(
    value = ifelse(value < 0.00001, 0, value),
    lu.class = recode(
      lu.class,
      "ArbLndPls"= "CrpLnd",
      "GrsLndTot"="Grass",
      # "PltArtTot"="PltFor",
      "PltForTot"="PltFor",
      "ForLndTot"="Forest",
      "OthLndTot"="OthNatLnd" ##temporary solution; OthLndTot actually contains "NotRel"
    )) %>%
  mutate(SCEN1=toupper(SCEN1),SCEN2=toupper(SCEN2),SCEN3=toupper(SCEN3)) %>%
  subset(
    REGION == rrr &
      SCEN1 == curr.SCEN1 &
      SCEN2 == curr.SCEN2 & SCEN3 == curr.SCEN3
  ) %>%
  dplyr::select(c(REGION,SCEN1,SCEN2,SCEN3,lu.class, year, value)) %>% unique() %>% rename(times = year)

ds.resultLC.sum1 <- res$out.res %>% group_by(times,lu.to) %>%
  summarize(downscale.value = sum(value),.groups = "keep") %>%
  spread(key=times,value = downscale.value) %>%
  rename(LC=lu.to)

ds.resultLC.sum2 <- res$out.res %>%
  mutate(times=as.numeric(times)) %>%
  mutate(times=times-10) %>%
  group_by(times,lu.from) %>%
  summarize(downscale.value = sum(value),.groups = "keep") %>%
  spread(key=times,value = downscale.value) %>%
  rename(LC=lu.from)

df.init.sum <- init.areas %>% group_by(lu.from) %>%
  summarize(value = sum(value),.groups = "keep") %>%
  rename(LC=lu.from,init_area_2000=value)

ds.resultLC.sum <- ds.resultLC.sum1 %>%
  left_join(dplyr::select(ds.resultLC.sum2,LC,`2000`),by = c("LC")) %>%
  relocate("2000",.before = "2010") %>%
  left_join(df.init.sum,by=c("LC")) %>%
  relocate("init_area_2000",.before = "2000")

ds.resultLC.sumLong <- ds.resultLC.sum %>%
  pivot_longer(cols = !c(LC),names_to = "times")%>%
  mutate(REGION=rrr) %>%
  mutate(SCEN1=curr.SCEN1,SCEN2=curr.SCEN2,SCEN3=curr.SCEN3) %>%
  relocate(value,.after = "SCEN3")

##Merge the two dataframes, calculate the difference, and compare
chck.DS.LC = LC_for_check %>% rename(LC=lu.class,GLOBIOM.value=value) %>%
  mutate(times=as.character(times)) %>%
  left_join(ds.resultLC.sumLong%>%rename(downscale.value=value),
            by = c("REGION","LC","SCEN1","SCEN2","SCEN3","times") ) %>%
  mutate(diff = downscale.value - GLOBIOM.value)


## (3) Calc BII impact ------
## To be added (temporarily skipped because we haven't ported the BII related downscaling, and because it is more complicated to check BII impact in GLOBIOM and Downscaled results). Relative checking codes can be found in GLOBIOM-G4M-Link-SSP1 pipeline or MOEJ pipeline version.


## (4) merge checking results (only if cluster=FALSE) ------
if (scen==scenarios[1]) {
  chck.DS.targets_merge=chck.DS.targets
  chck.DS.LC_merge=chck.DS.LC
} else {
  chck.DS.targets_merge=rbind(chck.DS.targets_merge,chck.DS.targets)
  chck.DS.LC_merge=rbind(chck.DS.LC_merge,chck.DS.LC)
}


# 6. Conduct G4M_link to calc LC for G4M ------

data <- res$out.res %>% group_by(ns,lu.from,times, REGION) %>% summarise(value=sum(value)) %>%
  bind_cols(SCEN1=curr.SCEN1,SCEN2=curr.SCEN2, SCEN3=curr.SCEN3) %>%
  pivot_wider(names_from = times, values_from = value) %>% rename(SimUID=ns, LC_TYPES_EPIC=lu.from) %>% ungroup() %>%
  mutate(across(starts_with("2"), ~ifelse(is.na(.),0,.))) %>% left_join(init.areas %>%
  rename(SimUID=ns,LC_TYPES_EPIC=lu.from,"2000"=value)) %>%
  relocate("2000",.before = "2010") %>% replace(is.na(.),0)

# Load in G4MID and SIMUID mappings
g4m_mapping <- readRDS("source/g4m_mapping.RData")
g4m_simu_map <- g4m_mapping[[1]]
fao_landtype <- g4m_mapping[[2]]
fao_landtype_g4m <- g4m_mapping[[3]]
landtype_map <- g4m_mapping[[4]]

#  data_for_g4m <- data %>% left_join(landtype_map) %>% left_join(g4m_simu_map) %>%
#    #filter(LandTypeFAO=="Reserved") %>%
#    group_by(g4m_05_id,SCEN1,SCEN2,SCEN3,LandTypeFAO) %>%
#    summarise_at(.vars = colnames(.)[which(colnames(.)=="2000"):which(colnames(.)==max(LUC_COMPARE_SCEN0$year))], sum) %>%
#    drop_na()

cat("execute G4M_link_func.R ------")

source("G4M_link_func.R")

data_link_ds_g4m <- G4M_link(res,curr.SCEN1, curr.SCEN2, curr.SCEN3)
data_for_g4m <- data_link_ds_g4m[[2]]

cat(" G4M_link_func.R finished ------")


#  attr(data, "symName") <- "Land_Cover_SU_Region_SCEN"
#  symDim <- 7
#  LC <- wgdx.reshape(data, symDim, symName = "Land_Cover_SU_Region_SCEN", tName = "ScenYear")

#  attr(data_for_g4m, "symName") <- "LandCover_G4MID"
#  symDim <- 6
#  G4M <- wgdx.reshape(data_for_g4m, symDim, symName = "LandCover_G4MID", tName = "ScenYears")

## Not run:
# send the data to GDX
#  wgdx.lst(paste0(gdx_path), LC,G4M[[7]])

# 7. Write final output to RData -----

cat("Write output.RData ------")

# default: in older version (without checking)
# cons_data <- list()
# cons_data[[1]] <- data
# cons_data[[2]] <- data_for_g4m
# cons_data[[3]] <- res

# updated: add two more checking items
cons_data <- list()
cons_data[[1]] <- data
cons_data[[2]] <- data_for_g4m
cons_data[[3]] <- res
cons_data[[4]] <- chck.DS.targets
cons_data[[5]] <- chck.DS.LC
cons_data[[6]] <- data_link_ds_g4m[[3]] ## for outputting 

# saveRDS(cons_data,"gdx/output.RData")
saveRDS(cons_data,paste0("gdx/output_",project,"_",lab,".RData")) # YW 20251030 update output naming to include project and lab

cat("Write output finished. ------")


# 8. Process RstLnd to RstLnd_NoAffor vs. RstLnd_YesAffor
## This was decided not to be done in DS, but in G4M_DS_Merge (now in get_biodiversity.R).


