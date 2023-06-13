library(tidyverse)
# curr.SCEN1 <- "SSP1"
# curr.SCEN2 <- "SPA1"
# curr.SCEN3 <- "RCP2p6_UKESM1"
# project <- "isimip"
# lab <- "13072022"

mysum <- function(x){y <- sum(x, na.rm = TRUE); return(y)}

g4mid_to_simuid <- function(DS.res = out.res,
         #G4M.res.path,
         lab,
         project,
         curr.SCEN1,
         curr.SCEN2,
         curr.SCEN3) {

  ### create a mapping between G4M and SIMUS

  # DS.res=readRDS(paste0('../Isimip/13072022/postproc/new/output_15002.000002.RData'))[[3]]$out.res

  mapping <- readRDS(file = 'input/G4M_mapping.RData')[[1]]
  mapping <- apply(mapping, 2, as.character)
  mapping <- data.frame(mapping)
  mapping <- mapping %>% rename("g4m_id" = "g4m_05_id", "ns" = "SimUID")

  #save current region
  curr.region <- unique(DS.res$REGION)

  # G4M.res <- read.csv(paste0("../Isimip/13072022/G4M/new/area_harvest_map_",project,"_",lab,"_",curr.SCEN1,"_",curr.SCEN3,"_",curr.SCEN2,".csv"))
  # G4M.res <-
  #   read.csv(paste0("./input/ds_test/area_harvest_map_",project,"_",lab,"_",curr.SCEN1,"_",curr.SCEN3,"_",curr.SCEN2,".csv"))
  G4M.res <- read.csv(path(str_glue("area_harvest_map_",project,"_",lab,"_",curr.SCEN1,"_",curr.SCEN3,"_",curr.SCEN2,".csv")))

  G4M.res <-
    G4M.res %>% dplyr::select(g4m_id, year, forest_old_ha, forest_new_ha) %>%
    group_by(g4m_id) %>% mutate(
      deforestation = forest_old_ha - lag(forest_old_ha, n = 2),
      afforestation = forest_new_ha - lag(forest_new_ha, n =
                                            2),
      g4m_id = as.character(g4m_id)
    ) %>%
    filter(year %% 10 == 0) %>% na.omit()

  DS.res.g4mland <-
    DS.res %>% mutate(
      lu.from = recode(
        lu.from,
        "OthNatLnd" = "G4Mland",
        "PriFor" = "G4Mland",
        "MngFor" = "G4Mland",
        "protected_other" =
          "G4Mland",
        "protected_priforest" =
          "G4Mland",
      ),
      lu.to = recode(
        lu.to,
        "OthNatLnd" = "G4Mland",
        "PriFor" = "G4Mland",
        "MngFor" = "G4Mland",
        "protected_other" = "G4Mland",
        "protected_priforest" =
          "G4Mland",
      )
    ) %>%
    group_by(REGION, times, ns, lu.from, lu.to) %>% summarise(value = mysum(value))

  DS.init.g4mland <-
    DS.res.g4mland %>%
    filter(times == 2010) %>%
    group_by(REGION, ns, lu.from, times) %>%
    summarise(value = mysum(value)) %>%
    subset(lu.from == 'G4Mland') %>%
    mutate(times = times - 10)

  DS.init.grass <-
    DS.res.g4mland %>%
    filter(times == 2010) %>%
    group_by(REGION, ns, lu.from, times) %>%
    summarise(value = mysum(value)) %>%
    subset(lu.from == 'Grass') %>%
    mutate(times = times - 10)

  DS.init.crop <-
    DS.res.g4mland %>%
    filter(times == 2010) %>%
    group_by(REGION, ns, lu.from, times) %>%
    summarise(value = mysum(value)) %>%
    subset(lu.from == 'CrpLnd') %>%
    mutate(times = times - 10)

  DS.init.plt <-
    DS.res.g4mland %>%
    filter(times == 2010) %>%
    group_by(REGION, ns, lu.from, times) %>%
    summarise(value = mysum(value)) %>%
    subset(lu.from == 'PltFor') %>%
    mutate(times = times - 10)

  DS.init <- DS.res.g4mland %>%
    filter(times==2010) %>%
    group_by(REGION,ns, lu.from, times) %>%
    summarise(value=mysum(value)) %>%
    subset(lu.from!='G4Mland') %>%
    mutate(times=times-10)



  forest.weighting <- mapping %>%
    left_join((DS.init.g4mland %>% ungroup() %>% dplyr::select(ns, value))) %>%
    na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)

  grass.weighting <- mapping %>%
    left_join((DS.init.grass %>% ungroup() %>% dplyr::select(ns, value))) %>%
    na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)

  crop.weighting <- mapping %>%
    left_join((DS.init.crop %>% ungroup() %>% dplyr::select(ns, value))) %>%
    na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)

  plt.weighting <- mapping %>%
    left_join((DS.init.plt %>% ungroup() %>% dplyr::select(ns, value))) %>%
    na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)


  simu.forest <- forest.weighting %>%
    left_join(
      G4M.res %>% dplyr::select(g4m_id, year, forest_old_ha, forest_new_ha) %>%
        subset(year == 2000)
    ) %>%
    mutate(forest_old_ha = forest_old_ha * weight,
           forest_new_ha = forest_new_ha * weight) %>%
    na.omit() %>%
    dplyr::select(ns, forest_old_ha, forest_new_ha)


  DS.init.g4mland.forest <-
    DS.init.g4mland %>%
    left_join(simu.forest %>%
                mutate(ns = as.character(ns))) %>%
    mutate(across(starts_with("forest"), ~ifelse(is.na(.), 0, .))) %>%
    mutate(OthNatLnd = value - forest_old_ha - forest_new_ha)


  excess.forest <- DS.init.g4mland.forest %>% dplyr::select(ns, forest_old_ha, OthNatLnd) %>% left_join(mapping) %>% group_by(g4m_id) %>%  filter(OthNatLnd<0) %>% summarise(forest_old_ha=mysum(forest_old_ha),excess.f= mysum(OthNatLnd)) %>%
    mutate(percentage = (forest_old_ha-abs(excess.f))/forest_old_ha)





  if (!all(DS.init.g4mland.forest$OthNatLnd > 0)) {
    warning(
      "For some simus there is more G4M forest than G4Mland \n
            (sum of PriFor, MngFor, OthNatland + protected! \n
            Forest reduced to G4Mland levels"
    )
    DS.init.g4mland.forest <- DS.init.g4mland.forest %>%
      mutate(
        forest_old_ha = ifelse(
          OthNatLnd < 0,
          forest_old_ha + (forest_old_ha / (forest_old_ha + forest_new_ha)) * OthNatLnd,
          forest_old_ha
        ),
        forest_new_ha = ifelse(
          OthNatLnd < 0,
          forest_new_ha + (forest_new_ha / (forest_old_ha + forest_new_ha)) * OthNatLnd,
          forest_new_ha
        ),
        OthNatLnd = ifelse(OthNatLnd < 0, 0, OthNatLnd)
      )
  }

  final.forest.alloc <-
    DS.init.g4mland.forest %>% ungroup() %>% dplyr::select(REGION, times, ns, forest_old_ha, forest_new_ha, OthNatLnd) %>%
    pivot_longer(
      cols = c(forest_old_ha, forest_new_ha, OthNatLnd),
      names_to = "lu.from",
      values_to = "value"
    )

  DS.init.forest <- bind_rows(DS.init, final.forest.alloc)

  curr.init.DS <- DS.init.forest



  DS.trans.g4mland.g4mid <-
    DS.res.g4mland %>% left_join(mapping) %>%
    group_by(REGION, times, g4m_id, lu.from, lu.to) %>%
    summarise(value = mysum(value)) %>% filter(lu.from == "G4Mland" |
                                               lu.to == "G4Mland") %>%
    filter(value != 0)


  years <- unique(DS.res$times)

  iii <- 2020
  for (iii in years) {

    DS.res.nog4m <- DS.res.g4mland %>% filter(lu.from!="G4Mland" & lu.to!="G4Mland" & times==iii)


    deforestation <-
      G4M.res %>% dplyr::select(g4m_id, year, deforestation, afforestation) %>%
      # filter(deforestation!=0 | afforestation!=0) %>%
      filter(year == iii) %>% mutate(deforestation = abs(deforestation)) %>% left_join(excess.forest) %>%
      mutate(deforestation=ifelse(is.na(percentage),deforestation,deforestation*percentage),
             afforestation=ifelse(is.na(percentage),afforestation,afforestation*percentage)) %>%
      dplyr::select(g4m_id,year,deforestation,afforestation)





    deforestation <- deforestation %>% left_join((curr.init.DS %>% filter(lu.from=="forest_old_ha") %>% left_join(mapping) %>% group_by(g4m_id) %>% summarise(forest=mysum(value)))) %>% mutate(deforestation=ifelse(is.na(forest),deforestation,min(deforestation,forest))) %>%  dplyr::select(g4m_id,year,deforestation,afforestation)


    DS.trans.g4mland.g4mid.temp <-
      DS.trans.g4mland.g4mid %>% filter(times == iii) %>% left_join(deforestation)






    #### first case: no forest according G4M ----- all transitions only with other

    updated.transitions.oth <- DS.trans.g4mland.g4mid.temp %>%
      filter(is.na(deforestation) & is.na(afforestation)) %>%
      mutate(
        lu.from = recode(lu.from, "G4Mland" = "OthNatLnd"),
        lu.to = recode(lu.to, "G4Mland" = "OthNatLnd")
      ) %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)


    DS.trans.g4mland.g4mid.temp <-
      DS.trans.g4mland.g4mid.temp %>% filter(!(is.na(deforestation) &
                                                 is.na(afforestation)))


    #### second case: afforest where no afforestation is happening, and same for deforestation

    #afforestation
    updated.transitions.aff <- DS.trans.g4mland.g4mid.temp %>%
      filter(lu.from != "G4Mland" & afforestation == 0) %>%
      left_join((
        curr.init.DS %>% filter(lu.from == "OthNatLnd") %>%
          left_join(mapping) %>% group_by(g4m_id) %>% summarise(value.oth =
                                                                  mysum(value))
      )) %>%
      mutate(
        value.oth = ifelse(is.na(value.oth), 0, value.oth),
        lu.to = ifelse(
          value.oth == 0,
          recode(lu.to, "G4Mland" = "forest_new_ha"),
          recode(lu.to, "G4Mland" = "OthNatLnd")
        )
      ) %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)



    #deforestation
    updated.transitions.def <- DS.trans.g4mland.g4mid.temp %>%
      filter(lu.to != "G4Mland" & deforestation == 0) %>%
      #need to check if from othnatlnd is possible otherwise forced deforestation
      left_join((
        curr.init.DS %>% filter(lu.from == "OthNatLnd") %>%
          left_join(mapping) %>% group_by(g4m_id) %>% summarise(value.oth =
                                                                  mysum(value))
      )) %>%
      mutate(
        value.oth = ifelse(is.na(value.oth), 0, value.oth),
        lu.from = ifelse(
          value.oth == 0,
          recode(lu.from, "G4Mland" = "forest_old_ha"),
          recode(lu.from, "G4Mland" = "OthNatLnd")
        )
      ) %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)







    ###### maybe changes needed to updated aff and def



    DS.trans.g4mland.g4mid.temp <- DS.trans.g4mland.g4mid.temp %>%
      filter(!(lu.to != "G4Mland" & deforestation == 0)) %>%
      filter(!(lu.from != "G4Mland" & afforestation == 0))


    #### third case: afforest where afforestation is happening, and same for deforestation

    #afforestation
    afforest <- DS.trans.g4mland.g4mid.temp %>%
      filter(lu.from != "G4Mland" & afforestation > 0)


    grouped.aff <- afforest %>%
      group_by(REGION, times, g4m_id) %>%
      summarise(value = mysum(value),
                afforestation = mean(afforestation))

    allocateable <-
      grouped.aff %>% mutate(afforestation.updated = afforestation - value) %>%
      filter(afforestation.updated >= 0) %>% dplyr::select(-value)

    updated.afforest <-
      afforest %>% left_join(allocateable) %>% na.omit() %>%
      mutate(lu.to = recode(lu.to, "G4Mland" = "forest_new_ha")) %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)


    unallocateable <-
      grouped.aff %>% mutate(afforestation.updated = afforestation - value) %>%
      filter(afforestation.updated < 0) %>% dplyr::select(-value)
    if (nrow(unallocateable) != 0) {
      updated.afforest2 <-
        afforest %>% left_join(unallocateable) %>% na.omit() %>%
        mutate(count = 2) %>% uncount(count) %>% ungroup() %>% mutate(identifier = rep(seq(1,2,1),nrow(.)/2))

      A <- updated.afforest2 %>% filter(identifier==1) %>% mutate(value=value*(afforestation/(afforestation+abs(afforestation.updated))))
      B <- updated.afforest2 %>% filter(identifier==2) %>% mutate(value=value*(abs(afforestation.updated)/(afforestation+abs(afforestation.updated))))

      updated.afforest2 <- A %>% bind_rows(B)

      updated.afforest2$lu.to[updated.afforest2$identifier == 1] <-
        "forest_new_ha"

      updated.afforest2$lu.to[updated.afforest2$identifier == 2] <-
        "OthNatLnd"


      updated.afforest2 <- updated.afforest2 %>%
        dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)


      updated.afforest <-
        updated.afforest %>% bind_rows(updated.afforest2)
    }

    #deforestation
    deforest <- DS.trans.g4mland.g4mid.temp %>%
      filter(lu.to != "G4Mland" & deforestation > 0)


    grouped.def <- deforest %>%
      group_by(REGION, times, g4m_id) %>%
      summarise(value = mysum(value),
                deforestation = mean(deforestation))

    allocateable <-
      grouped.def %>% mutate(deforestation.updated = deforestation - value) %>%
      filter(deforestation.updated >= 0) %>% dplyr::select(-value)

    updated.deforest <-
      deforest %>% left_join(allocateable) %>% na.omit() %>%
      mutate(lu.from = recode(lu.from, "G4Mland" = "forest_old_ha")) %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)


    unallocateable <-
      grouped.def %>% mutate(deforestation.updated = deforestation - value) %>%
      filter(deforestation.updated < 0) %>% dplyr::select(-value)
    if (nrow(unallocateable) != 0) {
      updated.deforest2 <-
        deforest %>% left_join(unallocateable) %>% na.omit() %>%
        mutate(count = 2) %>% uncount(count) %>% ungroup() %>% mutate(identifier = rep(seq(1,2,1),nrow(.)/2))


      A <- updated.deforest2 %>% filter(identifier==1) %>% mutate(value=value*(deforestation/(deforestation+abs(deforestation.updated))))
      B <- updated.deforest2 %>% filter(identifier==2) %>% mutate(value=value*(abs(deforestation.updated)/(deforestation+abs(deforestation.updated))))

      updated.deforest2 <- A %>% bind_rows(B)


      updated.deforest2$lu.from[updated.deforest2$identifier == 1] <-
        "forest_old_ha"

      updated.deforest2$lu.from[updated.deforest2$identifier == 2] <-
        "OthNatLnd"


      updated.deforest2 <- updated.deforest2 %>%
        dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)


      updated.deforest <-
        updated.deforest %>% bind_rows(updated.deforest2)
    }



    afforestation.updates <-
      grouped.aff %>% mutate(afforestation.updated = afforestation - value) %>%
      mutate(afforestation.updated = ifelse(afforestation.updated < 0, 0, afforestation.updated))
    deforestation.updates <-
      grouped.def %>% mutate(deforestation.updated = deforestation - value) %>%
      mutate(deforestation = ifelse(deforestation.updated < 0, 0, deforestation.updated))


    DS.trans.g4mland.g4mid.temp <-  DS.trans.g4mland.g4mid.temp %>%
      left_join(afforestation.updates %>% dplyr::select(g4m_id, afforestation.updated)) %>%
      left_join(deforestation.updates %>% dplyr::select(g4m_id, deforestation.updated)) %>%
      mutate(
        afforestation = ifelse(
          is.na(afforestation.updated),
          afforestation,
          afforestation.updated
        ),
        deforestation = ifelse(
          is.na(deforestation.updated),
          deforestation,
          deforestation.updated
        )
      ) %>%
      dplyr::select(REGION,
                    times,
                    g4m_id,
                    lu.from,
                    lu.to,
                    deforestation,
                    afforestation)


    #### forth case: remainder between affor and deforest
    ## negative remainder=deforestation, positive remainder = afforestation

    DS.trans.g4mland.g4mid.temp <- DS.trans.g4mland.g4mid.temp %>%
      filter(lu.from == lu.to) %>% mutate(remainder = afforestation - deforestation)

    ### afforestation
    remainder.aff <-
      DS.trans.g4mland.g4mid.temp %>% filter(remainder >= 0) %>%
      mutate(lu.to = "forest_new_ha") %>%
      left_join((
        curr.init.DS %>% filter(lu.from == "OthNatLnd") %>%
          left_join(mapping) %>% group_by(g4m_id) %>% summarise(value.oth =
                                                                  mysum(value))
      ))

    if(ncol(remainder.aff %>% filter(deforestation == 0 & value.oth == 0)) == 0) {
      warning("unallocateable afforestation!!!")
      remainder.aff <-
        remainder.aff %>% filter(!(deforestation == 0 & value.oth == 0))
    }

    afforest.transitions <- remainder.aff %>%
      mutate(value = abs(deforestation), lu.from = "forest_old_ha") %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value) %>% filter(value !=0)

    afforestation.transitions2 <- remainder.aff %>%
      mutate(max.trans = ifelse(value.oth > remainder, remainder, value.oth))

    if (ncol(afforestation.transitions2 %>% filter(max.trans != abs(remainder))) != 0) {
      warning("unallocateable afforestation!!!")
      afforestation.transitions2 <-
        afforestation.transitions2 %>% filter(!(max.trans == 0))
    }

    afforestation.transitions2 <- afforestation.transitions2 %>%
      mutate(value = max.trans, lu.from = "OthNatLnd") %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)

    total.afforest.transitions <-
      afforest.transitions %>% bind_rows(afforestation.transitions2)









    ### deforestation
    remainder.def <-
      DS.trans.g4mland.g4mid.temp %>% filter(remainder < 0) %>%
      mutate(lu.from = "forest_old_ha")


    remaining <- updated.deforest %>% filter(lu.from=="forest_old_ha") %>%
      group_by(g4m_id) %>% summarise(value=sum(value))

    remaining <- curr.init.DS %>% filter(lu.from == "forest_old_ha") %>%
      left_join(mapping) %>% group_by(g4m_id) %>% summarise(forest = sum(value)) %>% left_join(remaining) %>%
      mutate(value=ifelse(is.na(value),0,value), forest=forest-value)



    remainder.def <- remainder.def  %>% left_join(remaining)


    if(ncol(remainder.def %>% filter(forest==0)) == 0) {
      warning("unallocateable afforestation!!!")
      remainder.def <-
        remainder.def %>% filter(!(forest == 0))
    }

    deforest.transitions <- remainder.def %>%
      mutate(value = afforestation, lu.to = "forest_new_ha") %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value) %>% filter(value !=0)

    deforestation.transitions2 <- remainder.def %>%
      mutate(max.trans = ifelse(forest > abs(remainder), abs(remainder), forest))

    if (ncol(deforestation.transitions2 %>% filter(max.trans != abs(remainder))) != 0) {
      warning("unallocateable deforestation!!!")
      deforestation.transitions2 <-
        deforestation.transitions2 %>% filter(!(max.trans == 0))
    }

    deforestation.transitions2 <- deforestation.transitions2 %>%
      mutate(value = max.trans, lu.to = "OthNatLnd") %>%
      dplyr::select(REGION, times, g4m_id, lu.from, lu.to, value)

    total.deforest.transitions <-
      deforest.transitions %>% bind_rows(deforestation.transitions2)



    #### fifth case: fill changes with remaining self inducing changes

    final.trans.before.selfinducing <- updated.transitions.oth

    if(nrow(updated.transitions.aff)!=0){
      final.trans.before.selfinducing <- final.trans.before.selfinducing %>% bind_rows(updated.transitions.aff)
    }
    if(nrow(updated.transitions.def)!=0){
      final.trans.before.selfinducing <- final.trans.before.selfinducing %>% bind_rows(updated.transitions.def)
    }
    if(nrow(total.afforest.transitions)!=0){
      final.trans.before.selfinducing <- final.trans.before.selfinducing %>% bind_rows(total.afforest.transitions)
    }
    if(nrow(total.deforest.transitions)!=0){
      final.trans.before.selfinducing <- final.trans.before.selfinducing %>% bind_rows(total.deforest.transitions)
    }
    if(nrow(updated.afforest)!=0){
      final.trans.before.selfinducing <- final.trans.before.selfinducing %>% bind_rows(updated.afforest)
    }
    if(nrow(updated.deforest)!=0){
      final.trans.before.selfinducing <- final.trans.before.selfinducing %>% bind_rows(updated.deforest)
    }


    final.trans.before.selfinducing <- final.trans.before.selfinducing %>%filter(!is.na(g4m_id))

    #map back to ns from G4M_id

    final.trans.before.nong4m.ns.g4m <-
      forest.weighting %>%
      left_join(final.trans.before.selfinducing %>% filter(lu.from!="Grass" & lu.from!="CrpLnd" & lu.from!="PltFor")) %>%
      mutate(value = value * weight) %>%
      dplyr::select(REGION, times, ns, lu.from,  value, lu.to)

    final.trans.before.nong4m.ns.grass <-
      grass.weighting %>%
      left_join(final.trans.before.selfinducing %>% filter(lu.from=="Grass"))  %>%
      mutate(value = value * weight) %>%
      dplyr::select(REGION, times, ns, lu.from,  value, lu.to) %>% na.omit()

    final.trans.before.nong4m.ns.crop <-
      crop.weighting %>%
      left_join(final.trans.before.selfinducing %>% filter(lu.from=="CrpLnd"))  %>%
      mutate(value = value * weight) %>%
      dplyr::select(REGION, times, ns, lu.from,  value, lu.to) %>% na.omit()

    final.trans.before.nong4m.ns.plt <-
      plt.weighting %>%
      left_join(final.trans.before.selfinducing %>% filter(lu.from=="PltFor"))  %>%
      mutate(value = value * weight) %>%
      dplyr::select(REGION, times, ns, lu.from,  value, lu.to) %>% na.omit()


    final.trans.before.nong4m.ns <- final.trans.before.nong4m.ns.g4m %>%
      bind_rows(final.trans.before.nong4m.ns.grass) %>%
      bind_rows(final.trans.before.nong4m.ns.crop) %>%
      bind_rows(final.trans.before.nong4m.ns.plt) %>%
      na.omit()








    if (nrow(DS.res.nog4m) != 0) {
      final.trans.ns <- final.trans.before.nong4m.ns %>% bind_rows(DS.res.nog4m %>% filter(lu.from!=lu.to))
    }






    test <- final.trans.ns %>% ungroup() %>%
      group_by(REGION, times, ns, lu.from) %>%
      summarise(total.out =mysum(value)) %>%
      left_join(curr.init.DS %>% dplyr::select(-times)) %>%
      mutate(plsbepositive = value - total.out)


    ####### fix
    corrected.trans <- final.trans.ns %>% filter(lu.from=="forest_old_ha" & lu.to=="OthNatLnd") %>%
      left_join(test %>% filter(plsbepositive<0) %>% ungroup() %>% dplyr::select(ns, plsbepositive)) %>%
      mutate(value=ifelse(is.na(plsbepositive),value,value+plsbepositive)) %>% dplyr::select(-plsbepositive)

    final.trans.ns <- final.trans.ns %>% filter(!(lu.from=="forest_old_ha"&lu.to=="OthNatLnd")) %>% bind_rows(corrected.trans)




    test.transitions_change <-final.trans.ns %>% ungroup() %>%
                  group_by(REGION, ns, lu.from) %>%
                  summarise(total.out =mysum(value)) %>% full_join(curr.init.DS %>% dplyr::select(-times)) %>% mutate(total.out=ifelse(is.na(total.out),0,total.out)) %>%
      mutate(plsbepositive = value - total.out) %>% filter(is.na(value)) %>%
      dplyr::select(REGION, ns, lu.from) %>% mutate(change=1)


    if(nrow(test.transitions_change)!=0){
      final.trans.ns <-  final.trans.ns %>% left_join(test.transitions_change) %>%
        mutate(lu.from=ifelse(!is.na(change)&lu.from=="OthNatLnd","forest_old_ha",
                              ifelse(!is.na(change)&lu.from=="forest_old_ha","OthNatLnd",lu.from)))
    }


    test.transitions_ <- final.trans.ns %>% group_by(REGION, lu.from, ns) %>% summarise(outflow=mysum(value)) %>%
      left_join(curr.init.DS) %>%
      mutate(plsbepositive=value-outflow)


    if(any(is.na(test.transitions_))) {
      warning("Problem with transitions, there are NAs")



      final.trans.ns <- final.trans.ns %>% left_join(test.transitions_ %>% filter(is.na(value)) %>% dplyr::select(REGION, lu.from, ns) %>% mutate(kick=1)) %>% filter(is.na(kick)) %>% dplyr::select(-kick,-change)
    }



    test.transitions <- curr.init.DS %>% dplyr::select(-times) %>%
      left_join(final.trans.ns %>% ungroup() %>%
                  group_by(REGION, ns, lu.from) %>%
                  summarise(total.out =mysum(value))) %>% mutate(total.out=ifelse(is.na(total.out),0,total.out)) %>%
      mutate(plsbepositive = value - total.out) %>%
      na.omit()


    if (any(test.transitions$plsbepositive < 0)) {
      warning("Problem with transitions, there is more area allocated than available.")



      final.trans.ns <- final.trans.ns %>% left_join(test.transitions %>% filter(plsbepositive<0) %>%
                                                       mutate(factor=value/total.out) %>%
                                                       dplyr::select(ns, lu.from, factor)) %>%
        mutate(value=ifelse(is.na(factor),value, value*factor)) %>% dplyr::select(-factor)

    }



    test.transitions <- curr.init.DS %>% dplyr::select(-times) %>%
      full_join(final.trans.ns %>%
                  group_by(REGION, ns, lu.from) %>%
                  summarise(total.out =mysum(value))) %>% mutate(total.out=ifelse(is.na(total.out),0,total.out)) %>%
      mutate(plsbepositive = value - total.out) %>%
      na.omit()
















    #fill transitions with curr.init.ds missing areas
    remaining.self.transitions <- test.transitions %>%
      filter(plsbepositive > 0) %>%
      mutate(lu.to = lu.from, value = plsbepositive, times=iii) %>%
      dplyr::select(REGION, times, ns, lu.from, lu.to, value)



    final.trans <-
      final.trans.ns %>% bind_rows(remaining.self.transitions)







    #update curr.init.ds by summarising over lu.to

    curr.init.DS <- final.trans %>%
      group_by(REGION, ns, lu.to, times) %>%
      summarise(value = mysum(value)) %>%
      rename("lu.from" = "lu.to") %>%
      mutate(times=iii+10)








    #### update the weightings



    DS.init.g4mland <-
      curr.init.DS %>%  mutate(
        lu.from = recode(
          lu.from,
          "forest_old_ha" = "G4Mland",
          "forest_new_ha" = "G4Mland",
          "OthNatLnd" = "G4Mland")) %>%
      group_by(REGION, ns, lu.from, times) %>%
      summarise(value = mysum(value))%>%
      subset(lu.from == 'G4Mland')%>%
      mutate(times = times - 10)



    DS.init.grass <-
      curr.init.DS %>%
      group_by(REGION, ns, lu.from, times) %>%
      summarise(value = mysum(value)) %>%
      subset(lu.from == 'Grass')%>%
      mutate(times = times - 10)


    DS.init.crop <-
      curr.init.DS %>%
      group_by(REGION, ns, lu.from, times) %>%
      summarise(value = mysum(value)) %>%
      subset(lu.from == 'CrpLnd')%>%
      mutate(times = times - 10)

    DS.init.plt <-
      curr.init.DS %>%
      group_by(REGION, ns, lu.from, times) %>%
      summarise(value = mysum(value)) %>%
      subset(lu.from == 'PltFor')%>%
      mutate(times = times - 10)



    forest.weighting <- mapping %>%
      left_join((DS.init.g4mland %>% ungroup() %>% dplyr::select(ns, value))) %>%
      na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)

    grass.weighting <- mapping %>%
      left_join((DS.init.grass %>% ungroup() %>% dplyr::select(ns, value))) %>%
      na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)

    crop.weighting <- mapping %>%
      left_join((DS.init.crop %>% ungroup() %>% dplyr::select(ns, value))) %>%
      na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)

    plt.weighting <- mapping %>%
      left_join((DS.init.plt %>% ungroup() %>% dplyr::select(ns, value))) %>%
      na.omit() %>% group_by(g4m_id) %>% mutate(weight = value / mysum(value)) %>% ungroup() %>% dplyr::select(g4m_id, ns, weight)









    if (iii == years[1]) {
      DS.res.G4M <- final.trans
    } else {
      DS.res.G4M <- DS.res.G4M %>% bind_rows(final.trans)
    }


  }

  return(DS.res.G4M)
}

