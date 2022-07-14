## package loading

environment(.libPaths)$.lib.loc = c("renv/library/R-4.0/x86_64-w64-mingw32",environment(.libPaths)$.lib.loc)

require("gdxrrw")
require(tidyr)
require(dplyr)
require(downscalr)
require(tidyverse)

set.seed(12457)

parameters <- readRDS("downscaling_pars.RData")

ISIMIP <- parameters[[1]] #to be kept as FALSE except for ISIMIP scens // TRUE -> changes starting maps
cluster <- parameters[[2]] #if TRUE run and scengrid must be passed on from GAMS

# run <- run.nr #from GAMS
# scengrid <- AllscenLoop #from GAMS

if(cluster){
  ####################
  # to be changed by the modeler based on current project
  project <- parameters[[3]] #as in GAMS
  lab <- parameters[[4]] #as in GAMS
  GAMSPath = c("C:/GAMS/win64/32.2")
  igdx(GAMSPath)
  gdx_path <- "./gdx/downscaled.gdx" #output location of downscaling result (Land_Cover_SU_Region_SCEN)
  ####################
  args <- commandArgs(trailingOnly=TRUE)
  run <- as.integer(args[[1]])
#  dir.create("output")

} else {
  ####################
  # to be changed by the modeler based on current project
  project <- "isimip" #as in GAMS
  lab <- 27042022 #as in GAMS
#  GAMSPath = c("C:/GAMS/win64/29.1") #if run on cluster/ need limpopo GAMS path
  gdx_path <- "./gdx/downscaled.gdx" #output location of downscaling result (Land_Cover_SU_Region_SCEN)
  ####################
}



#################################### read in GDX/GMS/csv ####


## initial LU level, currently not used in DS
# LANDCOVER_COMPARE_SCEN <-
#   rgdx.param(file.path(paste0(
#     "input/output_landcover_", project, "_", lab
#   )), "LANDCOVER_COMPARE_SCEN") %>%
#   setNames(c(
#     "REGION",
#     "lu.class",
#     "SCEN1",
#     "SCEN2",
#     "SCEN3",
#     "year",
#     "value"
#   )) %>% mutate(across(everything(), as.character)) %>%
#   mutate(value = as.numeric(value),
#          year = as.integer(as.character(year)))

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
         year = as.integer(as.character(year)))

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

transportation <-
  read.csv(file = "./input/acc_mean_travel_minutes_simu.csv")

grasyield <- read_table("input/data_GrasYield_X.gms")
grasyield <-
  grasyield[, c(1, 2)] %>% `colnames<-`(c("SimUID", "grasyield"))
grasyield$SimUID <- gsub("\\..*", "", grasyield$SimUID)

MngForest_Param <-
  rgdx.param(file.path(paste0("input/Forestparameters")), "MngForest_Param") %>%
  setNames(c("SimUID", "junk" , "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

pop1 <- rgdx.param(file.path(paste0("input/pop_SSP1")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop2 <- rgdx.param(file.path(paste0("input/pop_SSP2")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop3 <- rgdx.param(file.path(paste0("input/pop_SSP3")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop4 <- rgdx.param(file.path(paste0("input/pop_SSP4")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

pop5 <- rgdx.param(file.path(paste0("input/pop_SSP5")), "pop") %>%
  setNames(c("SimUID", "SCEN1" , "variable" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))


gdp <- rgdx.param(file.path(paste0("input/gdp")), "gdp") %>%
  setNames(c("SimUID", "SCEN1" , "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))

YLD_SSP_STAT <-
  rgdx.param(file.path(paste0("input/YLD_SSP_STATandDYN_regions37")), "YLD_SSP_STAT") %>%
  setNames(c("SCEN1" , "REGION", "CROP", "year" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value),
         year = as.integer(as.character(year)))


YLD_SSP_STAT <-
  expand.grid(SCEN1 = unique(YLD_SSP_STAT$SCEN1), year = unique(YLD_SSP_STAT$year), REGION = unique(YLD_SSP_STAT$REGION), CROP=unique(YLD_SSP_STAT$CROP)) %>% left_join(YLD_SSP_STAT) %>% mutate(value=ifelse(is.na(value),1,value))


init_xmat <-
  rgdx.param(file.path(paste0("input/Xmat")), "xmat") %>%
  setNames(c("SimUID" , "REGION", "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

LUC_Fin <-
  rgdx.param(file.path(paste0("input/LUC_Fin_Write_SSP2_msg07Ukraine37R")), "LUC_Fin") %>%
  setNames(c("SimUID", "lu.class", "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value)) %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())


SRP_Suit <-
  rgdx.param(file.path(paste0("input/X_4Tatiana")), "SRP_suit") %>%
  setNames(c("SimUID", "country", "SRP_class", "value")) %>% subset(SRP_class ==
                                                                      "SRP_NPP") %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))  %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())

trans_factors <-
  rgdx.param(file.path(paste0("input/Xmat")), "trans_factors") %>%
  setNames(c("variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

luc_downscl_coeff <-
  rgdx.param(file.path(paste0("input/betas")), "luc_downscl_coeff") %>%
  setNames(c("REGION", "lu.from", "lu.to", "variable" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))

AREA <-
  rgdx.param(file.path(paste0("input/X_4Tatiana")), "Area") %>%
  setNames(c("SimUID", "country", "CROP", "mgmt_sys", "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value)) %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())

Yield_Simu <-
  rgdx.param(file.path(paste0("input/yields")), "Yield_Simu") %>%
  setNames(c("SimUID", "country", "CROP", "mgmt_sys" , "value")) %>% mutate(across(everything(), as.character)) %>%
  mutate(value = as.numeric(value))  %>% left_join(init_xmat %>% dplyr::select(SimUID, REGION) %>% unique())


#################################### prepartaion of projections/restrictions & other for DS ####
REGION <- unique(LUC_COMPARE_SCEN0$REGION)[!unique(LUC_COMPARE_SCEN0$REGION)=="World"]
scengrid <- parameters[[5]] %>% mutate(REGION=rep(REGION,length(unique(parameters[[5]]$ScenLoop))))
#scengrid <- readRDS("ISIMIP_scengrid.RData") %>% mutate(ScenLoop=rep(seq(1:22),37)) %>%
#  arrange(ScenLoop) %>% mutate(ScenNr=1:(22*37))
# scengrid <- readRDS("scengrid.RData")

if(!cluster){
  scenarios <- which(scengrid$SCEN1=="SSP1" & scengrid$SCEN2=="SPA0" & scengrid$SCEN3=="scenRCPref")
} else {
  scenarios <- run
}

for(scen in scenarios){
  idx <- which(scengrid$ScenNr==scen)
  curr.SCEN1 <- scengrid$SCEN1[idx]
  curr.SCEN2 <- scengrid$SCEN2[idx]
  curr.SCEN3 <- scengrid$SCEN3[idx]
  rrr <- scengrid$REGION[idx]

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
                                                  "mngforest"="MngFor",))


  }






  xmat <- init_xmat %>% subset(REGION == rrr) %>%
    rename(ns = SimUID, ks = variable) %>% dplyr::select(-REGION)#explanatory variables

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
    ungroup() %>% group_by(lu.from, lu.to, times) %>% summarise(value=sum(value)) %>% ungroup() #targets

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
                                                             2000, value = 1)
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


  xmat.coltypes = data.frame(ks = c("CrpLnd", "Grass", "PriFor","MngFor", "OthNatLnd"),
                             value = "dynamic")
  xmat.coltypes = xmat.coltypes %>% bind_rows(data.frame(ks = unique(xmat$ks)[which(unique(xmat$ks) %in% unique(curr.projections$ks))], value = "projected"))

  res <-
    downscale(
      targets = DDelta %>% subset(!(lu.from=="PriFor"&lu.to=="MngFor")),
      init.areas,
      xmat,
      betas = betas %>% mutate(lu.from=recode(lu.from,"Forest"="PriFor"),
                               lu.to  =recode(lu.to  ,"Forest"="MngFor")),
      priors = curr.SRP_Suit,
      areas.update.fun = areas.sum_to,
      xmat.coltypes = xmat.coltypes,
      xmat.proj = curr.projections,
      restrictions = restrictions
    )



#  if(!cluster){
    if (scen==scenarios[1]) {
      res$out.res = bind_cols(REGION = rrr, res$out.res)
    } else {
      res$out.res = bind_rows(out.res,bind_cols(REGION = rrr, res$out.res))
    }
#  }

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

  source("G4M_link_func.R")

  data_for_g4m <- G4M_link(res,curr.SCEN1, curr.SCEN2, curr.SCEN3)

#  attr(data, "symName") <- "Land_Cover_SU_Region_SCEN"
#  symDim <- 7
#  LC <- wgdx.reshape(data, symDim, symName = "Land_Cover_SU_Region_SCEN", tName = "ScenYear")

#  attr(data_for_g4m, "symName") <- "LandCover_G4MID"
#  symDim <- 6
#  G4M <- wgdx.reshape(data_for_g4m, symDim, symName = "LandCover_G4MID", tName = "ScenYears")

  ## Not run:
  # send the data to GDX
#  wgdx.lst(paste0(gdx_path), LC,G4M[[7]])

  cons_data <- list()
  cons_data[[1]] <- data
  cons_data[[2]] <- data_for_g4m
  cons_data[[3]] <- res
  saveRDS(cons_data,"gdx/output.RData")



