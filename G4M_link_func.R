G4M_link <- function(downscalr_res, curr.SCEN1, curr.SCEN2, curr.SCEN3){

  mapping <- readRDS(file='source/g4m_mapping.RData')[[1]]
  mapping <- data.frame(apply(mapping, 2, as.numeric))

  dat <- downscalr_res$out.res
  dat$ns <- as.numeric(dat$ns)
  dat <- dat %>% left_join(mapping, by=c("ns"="SimUID")) %>% group_by(g4m_05_id, lu.from, times) %>%
    summarise(value=sum(value)) %>% rename("ScenYear"="times", "LC_TYPES_EPIC"= "lu.from") %>%
    mutate(LC_TYPES_EPIC=recode(LC_TYPES_EPIC, "Forest"="unreserved",
                                "OthNatLnd"="unreserved",
                                "PriFor"="unreserved",
                                "MngFor"="unreserved",
                                "protected_priforest"="unreserved",
                                "protected_other"="unreserved",)) %>%
    subset(LC_TYPES_EPIC!="unreserved") %>% ungroup() %>% group_by(g4m_05_id, ScenYear) %>%
    summarise(value=sum(value)) %>% na.omit()

  dat <- bind_cols(g4m_05_id=dat$g4m_05_id, SCEN1=curr.SCEN1, SCEN3=curr.SCEN3,SCEN2=curr.SCEN2,
                   LC_TYPES_EPIC="Reserved", ScenYear=dat$ScenYear, value=dat$value) %>%
    pivot_wider(id_cols=c(g4m_05_id, SCEN1, SCEN3, SCEN2, LC_TYPES_EPIC),
                names_from = "ScenYear", values_from = "value")


  return(dat)

}
