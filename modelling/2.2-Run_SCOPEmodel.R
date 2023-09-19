library(rSCOPE)
library(svMisc)
library(REddyProc)
#####################################################################################################
## Examples of uses of the run_SCOPE function
#####################################################################################################
# check the possible parameters (variables and constant) and model setting to be changed

# inputs used as timeseries 
filter(model_inputs(SCOPE_dir = "D:/SCOPE-master/")[[1]], is.na(X2)==FALSE)

# inputs used as constant 
model_inputs(SCOPE_dir = "D:/SCOPE-master/")[[2]]

# model settings
model_inputs(SCOPE_dir = "D:/SCOPE-master/")[[3]]

######################################################################################################

####################################################################################################
## split entire Berlin by group of pixels and with time delay in seconds
######################################################################################################
# Inputs_Berlin <- readRDS("Inputs_Berlin.rds")
# summary(Inputs_Berlin)
# tail(Inputs_Berlin)

Inputs_Berlin_19 <- filter(Inputs_Berlin, year(BerkeleyJulianDateToPOSIXct(Inputs_Berlin$t, "UTC"))==2019)
Inputs_Berlin_20 <- filter(Inputs_Berlin, year(BerkeleyJulianDateToPOSIXct(Inputs_Berlin$t, "UTC"))==2020)

# A tibble: 9,636,048 × 17
names(Inputs_Berlin_20)

# saveRDS(Inputs_Berlin_19, "Inputs_Berlin_2019.rds")
# saveRDS(Inputs_Berlin_20, "Inputs_Berlin_2020.rds")
# Inputs_Berlin_19 <- readRDS("Inputs_Berlin_2019.rds")
# Inputs_Berlin_20 <- readRDS("Inputs_Berlin_2020.rds")
# write.csv(Inputs_Berlin_19, "Inputs_Berlin_2019.csv")
# write.csv(Inputs_Berlin_20, "Inputs_Berlin_2020.csv")

#  obs = 9609720
#  pixels = 1097

# 2020
#  timestamp = 8760
9609720 - (20*8760)*54
Inputs_Berlin_19$split_55 <- c(rep(seq(1:54), each = 20*8760), rep(55, 148920))
split_19pixels <- 1:55
# 2020
9636048 - (20*8784)*54
Inputs_Berlin_20$split_55 <- c(rep(seq(1:54), each = 20*8784), rep(55, 149328))
split_20pixels <- 1:55

# number of splits
n_split = 55

####################################################################################################
# run SCOPE for entire Berlin in 2020 (divided in 55 runs)
for (i in 1:55) {
  run_SCOPE(csv_inputs = Inputs_Berlin_20,
            Simulation_Name = paste0("ET_Berlin2020_", i),
            SCOPE_dir = "D:/SCOPE-master/",
            split = TRUE,
            col_split = "split_55", #dataset_for_verification
            split_values = split_20pixels[i],
            t = "t", # time BerkeleyJulianDate  
            Rin = "Rin", Rli = "Rli", 
            p = "p", Ta = "Ta", RH = "RH", ea = NA, 
            u = "ws", # atmosphere conditions
            tts = "tts", tto = NA, psi = NA,
            SMC = "SMC60",
            LAI = "LAI", 
            hc = "hc_vh",
            # constants
            z_c = 40,
            startDOY = 20181201, endDOY = 20200130,  # timestamp period
            LAT = 52.5, LON = 13.32, timezn = 1,   # Lat/long and time zone
            # model settings
            lite = 1,
            soilspectrum = 1, 
            applTcorr = 1, 
            soil_heat_method = 1, 
            calc_rss_rbs = 0, 
            MoninObukhov = 0)
  progress(i, 55, progress.bar = TRUE, init = T)
  Sys.sleep(2400) # time delay in seconds - 1 hour = 60*60
  if (i == 55) message("Done!")
}
####################################################################################################

####################################################################################################
### run SCOPE with the DWD data - one pixel

Inputs_Berlin_20_p882 <- filter(Inputs_Berlin_20, id_pixel==882)

run_SCOPE(csv_inputs = Inputs_Berlin_20_p882,
          SCOPE_dir = "D:/SCOPE-master/",
          Simulation_Name = "DWD_882",
          split = FALSE,
          # variable names
          t = "t", Rin = "Rin", Rli = "Rli", p = "p",
          Ta = "Ta", RH = "RH_DWD", ea = NA, u = "ws",
          tts = "tts", tto = NA, psi = NA, 
          SMC = "SMC60", LAI = "LAI", hc = "hc_vh",
          ### constants values (non-default)
          # hc_c = 20, LAI_c = 3, SMC_c = 50, Ca_c = 417,
          z_c = 56, # measurements height
          LAT = 52.51, # Lat/long
          LON = 13.33,
          timezn = 1,  # time zone 0 or 1 is the same if tts is provided
          startDOY = 20181201, 
          endDOY =20210130,  # timestamp period
          ### settings values (non-default)
          lite = 1, # faster
          simulation = 1, # time series
          soilspectrum = 1, 
          soil_heat_method = 1, # soil options
          applTcorr = 1,  
          MoninObukhov = 0, 
          calc_rss_rbs = 0, # corrections
          save_spectral = 0,
          calc_fluor = 0,
          calc_planck = 0,
          calc_xanthophyllabs = 0,
          Fluorescence_model = 0,
          calc_directional = 0, # very time demanding
          calc_vert_profiles = 0)

####################################################################################################
####################################################################################################
# stress scenarios 
####################################################################################################
####################################################################################################
# simulate different kinds of forest and grassland
# Inputs_Berlin <- readRDS("Inputs_Berlin.rds")

Inputs_Berlin %>%
  group_by(t) %>%
  summarise_all(mean) -> Inputs_vegetation

Inputs_Berlin %>%
  filter(id_pixel == 351) %>% # 1127
  select(LAI) -> LAI_maxhc

Inputs_Berlin %>%
  filter(id_pixel == 1128) %>% # 
  select(LAI) -> LAI_Grass

Inputs_vegetation$LAI_grass <- LAI_Grass$LAI
Inputs_vegetation$LAI_hcmax <- LAI_maxhc$LAI

summary(Inputs_vegetation)

# ## run SCOPE with the DWD data - forest 
Inputs_vegetation$Ta_plus1 <- Inputs_vegetation$Ta + 1
Inputs_vegetation$Ta_plus2 <- Inputs_vegetation$Ta + 2
Inputs_vegetation$Ta_plus3 <- Inputs_vegetation$Ta + 3
Inputs_vegetation$Ta_plus4 <- Inputs_vegetation$Ta + 4

Inputs_vegetation$RH_60p <- Inputs_vegetation$RH * 0.60
Inputs_vegetation$RH_75p <- Inputs_vegetation$RH * 0.75
Inputs_vegetation$RH_90p <- Inputs_vegetation$RH * 0.90
Inputs_vegetation$RH_95p <- Inputs_vegetation$RH * 0.95

# stressfactor = 1 (default), 2, 3, 5 - reduce Vcmax25 as SMC or leaf age
# BallBerrySlope = 8 (default), 9, 10, 7, 6
# SMC_c = 25 (default), 20, 15, 10, 5 and SMC = "SMC60_dry_3"
# Ta_plus4 and RH_75p
# Ta_plus4 and RH_75p and SMC60_dry_2

# Scenarios
#####################################################################################################
# Forest mean and SMC, Stress factor, BallBerrySlope_c, Ta and RH
run_SCOPE(csv_inputs = Inputs_vegetation,
          Simulation_Name = "forest_hc_17m", 
          split = FALSE,
          t = "t", # time BerkeleyJulianDate  
          Rin = "Rin", Rli = "Rli", 
          hc_c = 17.7, hc = NA, LAI = "LAI_hcmax",
          p = "p", ea = NA, u = "ws", # atmosphere conditions
          tts = "tts", tto = NA, psi = NA,
          Ta = "Ta", RH = "RH",  
          SMC =  NA, #"SMC60", 
          SMC_c = 0.25,
          stressfactor = 1,
          BallBerrySlope_c = 8,
          z_c = 40,
          startDOY = 20181201, endDOY = 20210130,  # timestamp period
          LAT = 52.5, LON = 13.32, timezn = 0,   # Lat/long and time zone
          lite = 1,
          soilspectrum = 0, 
          applTcorr = 1, 
          soil_heat_method = 1, 
          calc_rss_rbs = 0,
          MoninObukhov = 0)

# grassland 
run_SCOPE(csv_inputs = Inputs_vegetation,
          Simulation_Name = "grassland_hc_10cm", 
          split = FALSE,
          t = "t", Rin = "Rin", Rli = "Rli", p = "p", ea = NA, 
          u = "ws", tts = "tts", tto = NA, psi = NA,
          hc_c = 0.10, hc = NA, LAI = "LAI_grass",
          Ta = "Ta", RH = "RH",  
          SMC =  NA, #"SMC60", # 
          SMC_c = 0.01,
          stressfactor = 1,
          BallBerrySlope_c = 8,
          z_c = 40,
          startDOY = 20181201, endDOY = 20210130,  # timestamp period
          LAT = 52.5, LON = 13.32, timezn = 0,   # Lat/long and time zone
          lite = 1,
          soilspectrum = 1, 
          applTcorr = 1, 
          soil_heat_method = 1, 
          calc_rss_rbs = 0,
          MoninObukhov = 0)
####################################################################################################

####################################################################################################
1 # Simulation_Name = "Forest_average", hc_c = 25, LAI = "LAI_max",
2 # Simulation_Name = "Forest_average_LAIav", hc_c = 4.8, LAI = "LAI",
3 # Simulation_Name = "Forest_LAI_forest_max", hc_c = 33.1, LAI = "max_forest",
4 # Simulation_Name = "Forest_LAI_forest_mean", hc_c = 17.7, LAI = "mean_forest",
5 # Simulation_Name = "Forest_LAI_raster_max", hc_c = 40, LAI = "max_raster",
6 # Simulation_Name = "Forest_LAI_raster_mean", hc_c = 14, LAI = "mean_raster",
7 # Simulation_Name = "Forest_LAI_beech_max", hc_c = 29.7, LAI = "max_Beech",
8 # Simulation_Name = "Forest_LAI_beech_mean", hc_c = 19.6, LAI = "mean_Beech",
9 # Simulation_Name = "Forest_LAI_oak_max", hc_c = 26.3, LAI = "max_oak",
10 # Simulation_Name = "Forest_LAI_oak_mean", hc_c = 16.8, LAI = "mean_oak",
11 # Simulation_Name = "Forest_LAI_pine_max", hc_c = 23.5, LAI = "max_Pine",
12 # Simulation_Name = "Forest_LAI_pine_mean", hc_c = 14.2, LAI = "mean_Pine",
####################################################################################################

