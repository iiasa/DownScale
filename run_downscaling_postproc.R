args <- commandArgs(trailing = TRUE)

library(tidyverse)
library(dplyr)
library(fs)


# Load G4M output conversion function
source("G4M_DS_to_simu_link_final_limpopo.R")

# Get scenario
# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied", call.=FALSE)
} else {
  # default output file
  scen <- as.integer(args[1])
}


# Get downscaling mapping
downs_map <- readRDS("input/scenario_grid.RData")
additional_config <- readRDS("input/config.RData")
PROJECT <- additional_config[1]
DATE_LABEL <- additional_config[2]
cluster_nr_downscaling <- additional_config[3]

# Define loading function
rfunc <- function(x) readRDS(x)[[3]]$out.res

# Define scenarios
dscens <- downs_map %>% filter(ScenNr==scen)

# Load in downscalr output
downscalr_out <- rfunc(path(str_glue("postproc","output_",PROJECT,"_",cluster_nr_downscaling,".",
                                       sprintf("%06d",dscens$ScenNr),".RData")))

# Get output
g4m_simu_out <- g4mid_to_simuid(downscalr_out,DATE_LABEL,PROJECT,dscens$SCEN1,
                                  dscens$SCEN2,dscens$SCEN3)

# Write output
saveRDS(g4m_simu_out,path("gdx","g4m_simu_out.RData"))

