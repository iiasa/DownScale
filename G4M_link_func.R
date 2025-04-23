G4M_link <- function(downscalr_res, curr.SCEN1, curr.SCEN2, curr.SCEN3){

  mapping <- readRDS(file='source/g4m_mapping.RData')[[1]]
  mapping <- data.frame(apply(mapping, 2, as.numeric))

  dat <- downscalr_res$out.res
  dat$ns <- as.numeric(dat$ns)

  # map to g4m 0.5degree grids
  dat1 <- dat %>% left_join(mapping, by=c("ns"="SimUID")) %>% group_by(g4m_05_id, lu.from, times) %>%
    summarise(value=sum(value)) %>% rename("ScenYear"="times", "LC_TYPES_EPIC"= "lu.from")

  # Assign RstLnd to afforestable or non-afforestable
  mapping_g4mid_xy <- read.csv(file="source/oecd_ssp2_decc2016_reg31.csv") %>%
    dplyr::select(x,y,simuid) %>% rename("g4m_05_id"="simuid")
  maplayer_isforest0 <- read.csv(file="source/forest_nonforest_LUH2_halfdeg.csv")
  maplayer_isforest <- maplayer_isforest0 %>%
    rename("IsForest"=colnames(maplayer_isforest0)[3]) %>%
    left_join(mapping_g4mid_xy) %>% na.omit() %>%
    dplyr::select(g4m_05_id,IsForest)
  maplayer_isforest$IsForest <- as.numeric(maplayer_isforest$IsForest)

  dat2 <- dat1 %>% left_join(maplayer_isforest) %>%
    mutate(IsForest=ifelse(is.na(IsForest),0,IsForest))# %>%
    # mutate(LC_TYPES_EPIC=ifelse(IsForest==1,recode(LC_TYPES_EPIC,"RstLnd"="RstLnd_YesAffor"),recode(LC_TYPES_EPIC,"RstLnd"="RstLnd_NoAffor")))
  # YW 20240308: mapping is quicker to recode

  df.aux.1 <- data.frame(IsForest=c(1,0),Rst_Type=c("RstLnd_YesAffor","RstLnd_NoAffor"))
  dat2 <- dat2 %>%  left_join(df.aux.1)
  dat2$LC_TYPES_EPIC[dat2$LC_TYPES_EPIC=="RstLnd"] = dat2$Rst_Type[dat2$LC_TYPES_EPIC=="RstLnd"]
  dat2 <- dat2 %>% select(-c(Rst_Type))

  #test:
  # dat2_Rstcategory <- dat1 %>% left_join(maplayer_isforest) %>%
  #   mutate(IsForest=ifelse(is.na(IsForest),0,IsForest)) %>% left_join(df.aux.1)
  # dat2_Rstcategory$LC_TYPES_EPIC[dat2_Rstcategory$LC_TYPES_EPIC=="RstLnd"] = dat2_Rstcategory$Rst_Type[dat2_Rstcategory$LC_TYPES_EPIC=="RstLnd"]
  # dat2_Rstcategory <- dat2_Rstcategory %>% select(-c(Rst_Type))
  # sum(dat2 != dat2_Rstcategory,na.rm = TRUE)


  # Mark the unreserved LC types
  # YW updated 20230921: put protected land out of "unreserved" [this needs to be examined again in the next]
  # protected_priforest: should be counted in unreserved, but just should not go to G4Mland in the G4M_DS_Link step
  # protected_other: should go to "reserved"

    dat3 <- dat2 %>%
    mutate(LC_TYPES_EPIC=recode(LC_TYPES_EPIC, "Forest"="unreserved",
                                "OthNatLnd"="unreserved",
                                "PriFor"="unreserved",
                                "MngFor"="unreserved",
                                "protected_priforest"="unreserved",
                                # "protected_other"="unreserved", # YW updated 20230921
                                "RstLnd_YesAffor"="unreserved"))

  # Eventually (inversely) select the other "unreserved" LC types (that are potentially forest)

  dat4 <- dat3 %>%
    subset(LC_TYPES_EPIC!="unreserved") %>% ungroup() %>% group_by(g4m_05_id, ScenYear) %>%
    summarise(value=sum(value)) %>% na.omit()


  dat_final <- bind_cols(g4m_05_id=dat4$g4m_05_id, SCEN1=curr.SCEN1, SCEN3=curr.SCEN3,SCEN2=curr.SCEN2,
                   LC_TYPES_EPIC="Reserved", ScenYear=dat4$ScenYear, value=dat4$value) %>%
    pivot_wider(id_cols=c(g4m_05_id, SCEN1, SCEN3, SCEN2, LC_TYPES_EPIC),
                names_from = "ScenYear", values_from = "value")


  # return(dat_final)

  #=======YW 20240308: Adding this part to output an additional table for G4M: ordered affor=====
  df.aux.2 <- data.frame(IsForest=c(1,0),Nat_Type=c("NatLnd_forestBiome","NatLnd_grassBiome"))

  dat2_category <- dat2 %>% left_join(df.aux.2)
  dat2_category$LC_TYPES_EPIC[dat2_category$LC_TYPES_EPIC=="OthNatLnd"] = dat2_category$Nat_Type[dat2_category$LC_TYPES_EPIC=="OthNatLnd"]
  dat2_category <- dat2_category %>% select(-c(IsForest,Nat_Type))

  Array_NewAffor_LC <- c("RstLnd_YesAffor","NatLnd_forestBiome","NatLnd_grassBiome")
  dat2_category_select <- dat2_category %>%
    subset(LC_TYPES_EPIC %in% c("RstLnd_YesAffor","NatLnd_forestBiome","NatLnd_grassBiome")) %>%
    mutate(SCEN1=curr.SCEN1, SCEN3=curr.SCEN3, SCEN2=curr.SCEN2) %>%
    pivot_wider(id_cols=c(g4m_05_id, SCEN1, SCEN3, SCEN2, ScenYear),
                names_from = "LC_TYPES_EPIC", values_from = "value")

  for(k in 1:length(Array_NewAffor_LC)){
    if(!(Array_NewAffor_LC[k] %in% colnames(dat2_category_select))){
      dat2_category_select$NewCol <- 0
      names(dat2_category_select)[names(dat2_category_select)=="NewCol"] <- Array_NewAffor_LC[k]
    }
  }


  ## final processing, and data output
 if(!nrow(dat_final)==0){

  dat_final_column <- dat_final %>%
    pivot_longer(cols = -c(g4m_05_id,SCEN1,SCEN3,SCEN2,LC_TYPES_EPIC),names_to = "ScenYear",values_to = "value") %>%
    mutate(ScenYear=as.integer(ScenYear)) %>%
    pivot_wider(names_from = "LC_TYPES_EPIC",values_from = "value")

  dat2_output_for_g4m <- dat2_category_select %>%
    mutate(RstLnd_YesAffor=ifelse(is.na(RstLnd_YesAffor),0,RstLnd_YesAffor)) %>%
    mutate(NatLnd_forestBiome=ifelse(is.na(NatLnd_forestBiome),0,NatLnd_forestBiome)) %>%
    mutate(NatLnd_grassBiome=ifelse(is.na(NatLnd_grassBiome),0,NatLnd_grassBiome)) %>%
    mutate(Total_avail_newAffor=RstLnd_YesAffor+NatLnd_forestBiome+NatLnd_grassBiome) %>%
    left_join(dat_final_column,by = c("g4m_05_id", "SCEN1", "SCEN3", "SCEN2","ScenYear")) %>%
    mutate(Reserved=ifelse(is.na(Reserved),0,Reserved)) %>%
    select(g4m_05_id, SCEN1, SCEN3, SCEN2, ScenYear, Reserved, RstLnd_YesAffor, NatLnd_forestBiome, NatLnd_grassBiome, Total_avail_newAffor) # reorder the columns

  dat2_output_for_g4m2 <- dat2_output_for_g4m %>%
    pivot_longer(cols=c(RstLnd_YesAffor, NatLnd_forestBiome, NatLnd_grassBiome, Total_avail_newAffor),names_to = "LC_TYPES",values_to = "value") %>%
    pivot_wider(names_from = "ScenYear",values_from = "value")

  #=======END YW 20240308========

  dat_return <- list()
  dat_return[[1]] <- dat1
  dat_return[[2]] <- dat_final
  dat_return[[3]] <- dat2_output_for_g4m

 }else{
   ##if no data in data_final, then return a null list
   dat_return <- list()
   dat_return[[1]] <- dat1
   dat_return[[2]] <- dat_final
   dat_return[[3]] <- dat_final
 }

  return(dat_return)
}
