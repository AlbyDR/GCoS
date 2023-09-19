library(rSCOPE) # devtools::install_github("AlbyDR/rSCOPE")
library(leaflet)
library(sf)
library(tidyverse)
library(lubridate)

#############################################################
############ ERA5 reanalysis ##############
#############################################################

# request Berlin "reanalysis-era5-single-levels",
request_Berlin_Single_2019 <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c('surface_solar_radiation_downwards',
               'surface_thermal_radiation_downwards',
               '10m_u_component_of_wind', 
               '2m_temperature',
               'surface_pressure',
               'total_precipitation'),
  year = "2019",
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(53, 12, 52, 14),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "Berlin_single_2019.nc")

ecmwfr::wf_request(user = user_cds,                    # user ID (for authentication)
                   request  = request_Berlin_Single_2019,      # the request object
                   transfer = TRUE,                            # download the file
                   path = "D:/Data-Modelling/Meteo/") # store data in current working directory


##################################
# forcing ERA data 2019 - example
##################################
names(nc_open(paste0(dest_file_Meteo, "Berlin_single_2019.nc"))$var)

Berlin_Rin_2019 <- raster::brick(paste0(dest_file_Meteo, "Berlin_single_2019.nc"), varname = "ssrd")
Berlin_Rin_2019 <- raster::crop(Berlin_Rin_2019, extent(c(12.8,13.8,52,52.9)))

plot(Berlin_Rin_2019[[1]])
plot(Berlin_border, add = TRUE)

cellFromXY(Berlin_Rin_2019, ECtower_coord) # validation point (ECtower)
Berlin_Rin_2019 <- as.vector(raster::extract(Berlin_Rin_2019, 6))

Berlin_Rli_2019 <- raster::brick(paste0(dest_file_Meteo, "Berlin_single_2019.nc"), varname = "strd")
Berlin_Rli_2019 <- raster::crop(Berlin_Rli_2019, extent(c(12.8,13.8,52,52.9)))
Berlin_Rli_2019 <- as.vector(raster::extract(Berlin_Rli_2019, 6))

Berlin_Ta_2019 <- raster::brick(paste0(dest_file_Meteo, "Berlin_single_2019.nc"), varname = "t2m")
Berlin_Ta_2019 <- raster::crop(Berlin_Ta_2019, extent(c(12.8,13.8,52,52.9)))
Berlin_Ta_2019 <- as.vector(raster::extract(Berlin_Ta_2019, 6))

Berlin_ws_2019 <- raster::brick(paste0(dest_file_Meteo, "Berlin_single_2019.nc"), varname = "u10")
Berlin_ws_2019 <- raster::crop(Berlin_ws_2019, extent(c(12.8,13.8,52,52.9)))
Berlin_ws_2019 <- as.vector(raster::extract(Berlin_ws_2019, 6))

Berlin_p_2019 <- raster::brick(paste0(dest_file_Meteo, "Berlin_single_2019.nc"), varname = "sp")
Berlin_p_2019 <- raster::crop(Berlin_p_2019, extent(c(12.8,13.8,52,52.9)))
Berlin_p_2019 <- as.vector(raster::extract(Berlin_p_2019, 6))

Berlin_prec_2019 <- raster::brick(paste0(dest_file_Meteo, "Berlin_single_2019.nc"), varname = "tp")
Berlin_prec_2019 <- raster::crop(Berlin_prec_2019, extent(c(12.8,13.8,52,52.9)))
Berlin_prec_2019 <- as.vector(raster::extract(Berlin_prec_2019, 6))

# Request Berlim "reanalysis-era5-pressure-levels"
request_Berlin_RH_2020 <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "relative_humidity",
  pressure_level = "1000",
  year = "2020",
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(53, 12, 52, 14),
  dataset_short_name = "reanalysis-era5-pressure-levels",
  target = "Berlin_RH_2020.nc"
)

wf_request(user = user_cds,   # user ID (for authentification)
           request  = request_Berlin_RH_2020,  # the request
           transfer = TRUE,     # download the file
           path = "D:/Data-Modelling/Meteo/")# store data in current working directory

############
# 2019
############
names(nc_open(paste0(dest_file_Meteo, "Berlin_RH_2019.nc"))$var)

Berlin_RH_2019 <- raster::brick(paste0(dest_file_Meteo, "Berlin_RH_2019.nc"), varname = "r") 
Berlin_RH_2019 <- raster::crop(Berlin_RH_2019, extent(c(12.8, 13.8, 52, 52.9)))

plot(Berlin_RH_2019[[1]])
plot(Berlin_border, add = T , col = "transparent")

cellFromXY(Berlin_RH_2019, ECtower_coord) # validation point (ECtower)

Berlin_RH_2019 <- as.vector(raster::extract(Berlin_RH_2019, 6))

summary(Berlin_RH_2019)

########################################################################
# vectors and unit conversion
########################################################################

### 
summary(Berlin_RH_2019)
Berlin_RH_2019[Berlin_RH_2019 > 100] <- 100

# single
summary(Berlin_Ta_2019)
Berlin_Ta_2019 <- Berlin_Ta_2019-273 # kelvin to °C

summary(Berlin_p_2019)
Berlin_p_2019 = Berlin_p_2019/100

summary(Berlin_ws_2019)
Berlin_ws_2019[Berlin_ws_2019 <= 0] <- 0

summary(Berlin_Rin_2019)
Berlin_Rin_2019 <- Berlin_Rin_2019/3600

summary(Berlin_Rli_2019)
Berlin_Rli_2019 <- Berlin_Rli_2019/3600

# precipitation
summary(Berlin_prec_2019)
Berlin_prec_2019 <- Berlin_prec_2019*(1000)
