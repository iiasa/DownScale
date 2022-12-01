
args <- commandArgs(trailing = TRUE)

library(tidyverse)
library(fs)

if (!dir_exists(path("gdx"))) dir_create(path("gdx"))

config <- readRDS("config.RData")

# Get scenario mapping and indices
scenario_mapping <- config[[1]]
DOWNSCALING_TYPE <- config[[2]]
cluster_nr_downscaling <- config[[3]]
PROJECT <- config[[4]]
DATE_LABEL <- config[[5]]

# scenario counter
scen_cnt <- as.integer(args[1])
f_nr <- scen_cnt + 1

  scenarios_idx <- scenario_mapping$ScenNr[which(scenario_mapping$ScenLoop %in% scen_cnt)] %>% sort()

  # Extract land cover table for G4M
  for (i in 1:length(scenarios_idx)){

    # Get scenario number
    s_list <-  sprintf("%06d", scenarios_idx[i])

    # Read data for G4M
    if (!DOWNSCALING_TYPE=="downscalr"){
      downs_files <- rgdx.param(path("g4m_merge",str_glue("downscaled_{cluster_nr_downscaling}.",
                                                                       s_list,".gdx")),"LandCover_G4MID")
      # Select data for G4M
      if (dim(downs_files)[2] == 8) downs_files <- downs_files[,-1]
      names(downs_files) <- c("g4m_05_id","SCEN1","SCEN3","SCEN2","LC","Year","value")

      downs_files <- downs_files %>% filter(LC == "Reserved") %>% dplyr::select(-LC)

      names(downs_files) <- c("g4m_05_id","SCEN1","SCEN3","SCEN2","Year","value")

      # Remap years to columns
      downs2 <- downs_files %>% spread(Year, value, fill = 0, convert = FALSE)

      # Construct file path
      f <- path("gdx",
                str_glue("GLOBIOM2G4M_output_LC_abs_{PROJECT}_{DATE_LABEL}.csv"))

      if (i==1) {
        # Write csv file
        write_csv(downs2, f, col_names = T)
      } else {
        # Append to csv file
        write_csv(downs2, f, append = T)
      }

    } else {

      downs_files <- readRDS(path("g4m_merge",str_glue("output_{cluster_nr_downscaling}.",
                                                                    s_list,".RData")))[[2]] %>% select(-LC_TYPES_EPIC)

      f <- path("gdx",
                str_glue("GLOBIOM2G4M_output_LC_abs_{PROJECT}_{DATE_LABEL}.csv"))

      if (i==1) {
        # Write csv file
        write_csv(downs_files, f, col_names = T)
      } else {
        # Append to csv file
        write_csv(downs_files, f, append = T)
      }

    }


  }



