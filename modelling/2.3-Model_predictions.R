library(rSCOPE)
library(gtools)
library(tidyverse)
library(bigleaf)
library(lubridate)
library(scales)
library(REddyProc)
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
# get predictions
data("outputs_var")
unique(outputs_var$simulation_file)

outputs_var %>%
  filter(simulation_file == "fluxes.csv")

outputs_var %>%
  filter(simulation_file == "vegetation.csv")
#####################################################################################################

####################################################################################
# Berlin 2020
####################################################################################
# get prediction
Berlin2020_pred <- get_predictions(Simulation_Name = "ET_Berlin2020_")

Berlin2020_pred$timestamp <-REddyProc::BerkeleyJulianDateToPOSIXct(Inputs_Berlin_20$t, "UTC")
Berlin2020_pred$id_pixel <- Inputs_Berlin_20$id_pixel

Berlin2020_pred$ET <- bigleaf::LE.to.ET(Berlin2020_pred$lEtot, Inputs_Berlin_20$Ta)*3600
Berlin2020_pred$ET_soil = bigleaf::LE.to.ET(Berlin2020_pred$lEstot, Inputs_Berlin_20$Ta)*3600
Berlin2020_pred$ET_canopy = bigleaf::LE.to.ET(Berlin2020_pred$lEctot, Inputs_Berlin_20$Ta)*3600

saveRDS(Berlin2020_pred, "Berlin2020_pred.rds")
readRDS("Berlin2020_pred.rds")
Berlin2020_pred

# check the input parameters and model setting used to simulate in SCOPE
SCOPE_parameters_Berlin2020 <- get_parameters(
  SCOPE_dir = "D:/SCOPE-master/",
  Simulation_Name = "Berlin2020_",
  LAI = TRUE,
  SMC = TRUE,
  hc = TRUE,
  hc_c = TRUE,
  z_c = TRUE)

SCOPE_parameters_Berlin2020[[1]]
length(SCOPE_parameters_Berlin2020)
####################################################################################
# Berlin 2019
####################################################################################
# get prediction
Outputs_files_2019 <- list.files(paste0(path=grep("ET_Berlin2019_",
                                                  list.dirs(path=paste0("D:/SCOPE-master/","output"),
                                                            full.names = TRUE,
                                                            recursive = FALSE),
                                                  value = TRUE),
                                        "/", collapse = NULL, recycle0 = FALSE),
                                 pattern= "fluxes.csv",
                                 full.names = TRUE)

# the order is wrong 1 10 11 ... 2 20 21 ..

# correct the order 1 2 3 4 ... 55
Outputs_files_2019 <- gtools::mixedsort(sort(Outputs_files_2019))

# read all in a list
Outputs_list_2019 <- lapply(1:length(Outputs_files_2019), 
                            function(i)  readr::read_csv(Outputs_files_2019[i],
                                                         col_names = T,
                                                         readr::cols(.default = readr::col_double()))[-1,])

# stack all the 55 subsets # A tibble: 9,663,928 × 17
Berlin2019_pred <- dplyr::bind_rows(Outputs_list_2019)

Berlin2019_pred$timestamp <- BerkeleyJulianDateToPOSIXct(Inputs_Berlin_20$t, "UTC")
Berlin2019_pred$id_pixel <- Inputs_Berlin_20$id_pixel

Berlin2019_pred$ET <- bigleaf::LE.to.ET(Berlin2019_pred$lEtot, Inputs_Berlin_20$Ta)*3600
Berlin2019_pred$ET_soil = bigleaf::LE.to.ET(Berlin2019_pred$lEstot, Inputs_Berlin_20$Ta)*3600
Berlin2019_pred$ET_canopy = bigleaf::LE.to.ET(Berlin2019_pred$lEctot, Inputs_Berlin_20$Ta)*3600

saveRDS(Berlin2019_pred, "Berlin2019_pred.rds")
write.csv(Berlin2020_pred, "Berlin2020_pred.csv", row.names = FALSE)
readRDS("Berlin2019_pred.rds")
