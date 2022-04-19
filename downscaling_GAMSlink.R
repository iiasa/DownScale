rm(list = ls())

GAMSPath = c("C:/GAMS/win64/29.1")
require("gdxrrw")
require(tidyverse)
require(downscalr)
require(tidyr)
igdx(GAMSPath)

options(stringsAsFactors = FALSE)

project <- "SSPxRCP"
lab <- "16042021"
gdx_path <- "./gdx/downscaled_test.gdx"
ISIMIP <- FALSE
cluster <- FALSE


#################################### read in GDX/GMS/csv ####



LANDCOVER_COMPARE_SCEN <-
  rgdx.param(file.path(paste0(
    "input/output_landcover_", project, "_", lab
  )), "LANDCOVER_COMPARE_SCEN") %>%
  setNames(c(
    "REGION",
    "lu.class",
    "MacroScen",
    "BioenScen",
    "IEA_SCEN",
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
      "MacroScen",
      "BioenScen",
      "IEA_SCEN",
      "year",
      "value"
    )
  ) %>%  mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

Price_Compare2 <-
  rgdx.param(file.path(paste0(
    "input/output_landcover_", project, "_", lab
  )), "Price_Compare2") %>%
  setNames(c(
    "CROP",
    "REGION",
    "MacroScen",
    "BioenScen",
    "IEA_SCEN",
    "year",
    "value"
  )) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

transportation <-
  read.csv(file = "./source/acc_mean_travel_minutes_simu.csv")

grasyield <- read_table("source/data_GrasYield_X.gms")
grasyield <-
  grasyield[, c(1, 2)] %>% `colnames<-`(c("SimUID", "grasyield"))
grasyield$SimUID <- gsub("\\..*", "", grasyield$SimUID)

MngForest_Param <-
  rgdx.param(file.path(paste0("source/Forestparameters")), "MngForest_Param") %>%
  setNames(c("SimUID", "junk" , "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

pop1 <- rgdx.param(file.path(paste0("source/pop_SSP1")), "pop") %>%
  setNames(c("SimUID", "MacroScen" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop2 <- rgdx.param(file.path(paste0("source/pop_SSP2")), "pop") %>%
  setNames(c("SimUID", "MacroScen" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop3 <- rgdx.param(file.path(paste0("source/pop_SSP3")), "pop") %>%
  setNames(c("SimUID", "MacroScen" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop4 <- rgdx.param(file.path(paste0("source/pop_SSP4")), "pop") %>%
  setNames(c("SimUID", "MacroScen" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop5 <- rgdx.param(file.path(paste0("source/pop_SSP5")), "pop") %>%
  setNames(c("SimUID", "MacroScen" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))


gdp <- rgdx.param(file.path(paste0("source/gdp")), "gdp") %>%
  setNames(c("SimUID", "MacroScen" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

YLD_SSP_STAT <-
  rgdx.param(file.path(paste0("source/YLD_SSP_STATandDYN_regions37")), "YLD_SSP_STAT") %>%
  setNames(c("MacroScen" , "REGION", "CROP", "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))


YLD_SSP_STAT <-
  expand.grid(MacroScen = unique(YLD_SSP_STAT$MacroScen), year = unique(YLD_SSP_STAT$year), REGION = unique(YLD_SSP_STAT$REGION), CROP=unique(YLD_SSP_STAT$CROP)) %>% left_join(YLD_SSP_STAT) %>% mutate(value=ifelse(is.na(value),1,value))


init_xmat <-
  rgdx.param(file.path(paste0("source/Xmat")), "xmat") %>%
  setNames(c("SimUID" , "REGION", "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

LUC_Fin <-
  rgdx.param(file.path(paste0("source/LUC_Fin_Write_SSP2_msg07")), "LUC_Fin") %>%
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

AREA <-
  rgdx.param(file.path(paste0("source/X_4Tatiana")), "Area") %>%
  setNames(c("SimUID", "country", "CROP", "mgmt_sys", "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value)) %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())

Yield_Simu <-
  rgdx.param(file.path(paste0("source/yields")), "Yield_Simu") %>%
  setNames(c("SimUID", "country", "CROP", "mgmt_sys" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))  %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())


#################################### prepartaion of projections/restrictions & other for DS ####
scengrid <-
  expand.grid(
    REGION = unique(LUC_COMPARE_SCEN0$REGION)[unique(LUC_COMPARE_SCEN0$REGION) !=
                                                "World"],
    MacroScen = unique(LUC_COMPARE_SCEN0$MacroScen),
    BioenScen = unique(LUC_COMPARE_SCEN0$BioenScen),
    IEA_SCEN = unique(LUC_COMPARE_SCEN0$IEA_SCEN)
  )

if(!cluster){
  scenarios <- which(scengrid$MacroScen=="SSP1" & scengrid$BioenScen=="SPA0" & scengrid$IEA_SCEN=="scenRCPref")
} else {
  scenarios <- run
}

for(scen in scenarios){
  curr.MacroScen <- scengrid$MacroScen[scen]
  curr.BioenScen <- scengrid$BioenScen[scen]
  curr.IEA_SCEN <- scengrid$IEA_SCEN[scen]
  rrr <- scengrid$REGION[scen]
  
  lu.from <- lu.to <- unique(LANDCOVER_COMPARE_SCEN$lu.class)
  
  
  area_crop_unit_input = subset(AREA, REGION == rrr) %>% rename(area = value) %>%
    left_join(
      subset(Yield_Simu, REGION == rrr) %>%
        dplyr::select(SimUID, CROP, mgmt_sys, value) %>%
        rename(yield = value),
      by = c("SimUID", "mgmt_sys", "CROP")
    )
  sum_crop_area = area_crop_unit_input %>% group_by(SimUID) %>% summarise(area = sum(area))
  
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
  
  
  if(ISIMIP==FALSE){
    init.areas <-
      LUC_Fin %>% subset(REGION == rrr) %>% dplyr::select(!REGION) %>% subset(lu.class != "SimUarea") %>%
      rename(ns = SimUID, lu.from = lu.class)  %>% mutate(value = ifelse(value < 0.00001, 0, value))
  } else {
    init.areas <- 
      read.csv(file="source2/SimU_LU_biodiv_G4M_jan19.csv") %>%
      mutate(SimUID=row.names(.)) %>% subset(SimUID%in%unique((LUC_Fin %>% subset(REGION == rrr))$SimUID))
    
    cropsys <- 
      read.csv(file="source2/cropsys.csv") %>%
      mutate(SS_area=rowSums(across(ends_with("SS"))), glob_crop=rowSums(across(ends_with(c("HI","LI","IR"))))) %>%
      dplyr::select(SIMUID,SS_area,glob_crop) %>% rename(SimUID=SIMUID) %>% mutate(SimUID=as.character(SimUID))
    
    init.areas <- init.areas2 %>% left_join(cropsys) %>%
      mutate(arable=cropland-SS_area-glob_crop) %>%
      mutate(arable=ifelse(arable<0,0,arable)) %>%
      mutate(cropland=glob_crop) %>%
      dplyr::select(!c(glob_crop, restored))  %>%
      pivot_longer(cols = c(!SimUID), names_to = "lu.from", values_to = "value") %>%
      rename(ns=SimUID) %>% mutate(lu.from=recode(lu.from,"cropland"="CrpLnd",
                                                  "grassland"="Grass",
                                                  "other"="OthNatLnd",
                                                  "priforest"="Forest")) 
    
    
  }
  
  
  
  
  
  
  xmat <- init_xmat %>% subset(REGION == rrr) %>%
    rename(ns = SimUID, ks = variable) %>% dplyr::select(-REGION)#explanatory variables
  DDelta <-
    LUC_COMPARE_SCEN0 %>% filter(lu.from != "MngFor" &
                                   lu.to != "MngFor") %>%
    mutate(
      value = ifelse(value < 0.00001, 0, value),
      lu.from = as.character(
        fct_recode(
          lu.from,
          OthNatLnd = "NatLnd",
          Forest = "PriFor",
          Grass = "GrsLnd"
        )
      ),
      lu.to = as.character(
        fct_recode(
          lu.to,
          OthNatLnd = "NatLnd",
          Forest = "PriFor",
          Grass = "GrsLnd"
        )
      )
    ) %>%
    subset(
      REGION == rrr &
        MacroScen == curr.MacroScen &
        BioenScen == curr.BioenScen & IEA_SCEN == curr.IEA_SCEN
    ) %>%
    dplyr::select(c(lu.from, lu.to, year, value)) %>% unique() %>% rename(times = year) #targets
  betas <- luc_downscl_coeff %>% subset(REGION == rrr) %>%
    rename(ks = variable) %>% dplyr::select(-REGION)  %>% subset(ks %in% unique(xmat$ks))
  
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
  
  curr.crop_projections = subset(AREA, REGION == rrr) %>% rename(area = value) %>%
    left_join(
      subset(Yield_Simu, REGION == rrr) %>% dplyr::select(SimUID, CROP, mgmt_sys, value) %>%
        rename(yield = value),
      by = c("SimUID", "mgmt_sys", "CROP")
    ) %>%
    left_join(
      subset(YLD_SSP_STAT, REGION == rrr &
               MacroScen == curr.MacroScen) %>%
        bind_rows(
          subset(YLD_SSP_STAT, REGION == rrr &
                   MacroScen == curr.MacroScen) %>% expand(CROP, year =
                                                             2000, value = 1)
        ) %>%
        rename(shifters = value),
      by = c("CROP", "REGION")
    ) %>%
    left_join(
      subset(
        Price_Compare2,
        REGION == rrr & MacroScen == curr.MacroScen &
          BioenScen == curr.BioenScen &
          IEA_SCEN == curr.IEA_SCEN
      ) %>% rename(price = value)
    ) %>%
    left_join(trans_factors %>% rename(transfact = value, CROP = variable)) %>%
    mutate(value = area * yield * shifters * price * transfact) %>%
    group_by(SimUID, CROP, year) %>% summarise(value = sum(value))  %>%
    rename(ns = SimUID, ks = CROP, times = year)
  
  curr.gdp_projections = subset(gdp,
                                SimUID %in% unique(init.areas$ns) &
                                  MacroScen == curr.MacroScen) %>%
    mutate(
      value = value * subset(trans_factors, variable == "gdp_base")$value,
      ks = "gdp_base"
    ) %>%
    dplyr::select(-MacroScen) %>% rename(ns = SimUID, times = year)
  if (curr.MacroScen == "SSP1") {
    curr.pop_projections = pop1 %>%  dplyr::select(-MacroScen)
  } else if (curr.MacroScen == "SSP3") {
    curr.pop_projections = pop3 %>%  dplyr::select(-MacroScen)
  } else if (curr.MacroScen == "SSP4") {
    curr.pop_projections = pop4 %>%  dplyr::select(-MacroScen)
  } else if (curr.MacroScen == "SSP5") {
    curr.pop_projections = pop5 %>%  dplyr::select(-MacroScen)
  } else {
    curr.pop_projections = pop2 %>%  dplyr::select(-MacroScen)
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
  xmat.coltypes = xmat.coltypes %>% bind_rows(data.frame(ks = unique(xmat$ks)[which(unique(xmat$ks) %in% unique(curr.projections$ks))], value = "projected"))
  
  res <-
    downscale(
      targets = DDelta,
      init.areas,
      xmat,
      betas = betas,
      priors = curr.SRP_Suit,
      areas.update.fun = areas.sum_to,
      xmat.coltypes = xmat.coltypes,
      xmat.proj = curr.projections,
      restrictions = restrictions
    )
  
  
  
  if(!cluster){
    if (scen==scenarios[1]) {
      out.res = bind_cols(REGION = rrr, res$out.res)
    } else {
      out.res = bind_rows(out.res,bind_cols(REGION = rrr, res$out.res))
    }
  } 
  
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
  
  
  
}

#################################### write gdx ####

data <- res$out.res %>% group_by(ns,lu.from,times, REGION) %>% summarise(value=sum(value)) %>% bind_cols(REGION=rrr,MacroScen=curr.MacroScen,BioScen=curr.BioenScen, IEA_SCEN=curr.IEA_SCEN) %>% pivot_wider(names_from = times, values_from = value) %>% rename(SimUID=ns, LC_TYPES_EPIC=lu.from)
attr(data, "symName") <- "Land_Cover_SU_Region_SCEN"

symDim <- 7
wgdx.reshape(data, symDim, symName = "Land_Cover_SU_Region_SCEN", tName = "ScenYear", gdxName=paste0(gdx_path))


