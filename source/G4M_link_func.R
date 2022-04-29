library(tidyverse)

load(file='source/SimU_G4M_mapping.Rdata')
# test <- data.frame(SimUID=rep(mapping$SimUID,2), value=runif(length(mapping$SimUID)*2), lu.class=rep("Forest",length(mapping$SimUID)*2), year=c(rep(2000, length(mapping$SimUID)),rep(2010,length(mapping$SimUID))))

G4M_link <- function(dat, simu.label="SimUID", lu.label="lu.class", year.label="year"){
  dat <- dat %>% rename("SimUID" = all_of(simu.label), "lu.class"=all_of(lu.label), "year"=all_of(year.label))
  dat <- dat %>% left_join(mapping) %>% group_by(G4MID, lu.class, year) %>%
    summarise(value=sum(value))
  return(dat)
}

# G4M_link(test)

