library(tidyverse)
library(lubridate)
library(REddyProc) # POSIXctToBerkeleyJulianDate
library(raster)
library(ggplot2)
library(rSCOPE)
library(photobiology) # photobiology::sun_zenith_angle
####################################################################################
####################################################################################
# create a timestamp for 2019 and 2020
ts19_20 <- seq(as.POSIXct("2019-01-01", tz = "UTC"),
               as.POSIXct("2021-01-01", tz = "UTC"),
               by = "hour")[-17545] #"30 min"

ts19_20 <- force_tz(ts19_20, tz = "UTC")

head(ts19_20)
tail(ts19_20)

n_time <- ncell(ts19_20) # number of timestamps

# raster template based on the final interpolation cropped
plot(krg_grid) # see DWD interpolation

n_pixels <- ncell(krg_grid) # number of pixels ( raster cells)
####################################################################################

####################################################################################
# create a data frame based on the raster cells and timestamp
####################################################################################
# id with the number of rows
Input_Berlin_grid <- tibble::tibble(id = 1:(n_time*n_pixels))

# row order by timestmp and pixel
Input_Berlin_grid$id_time <- rep(seq(1:n_time), each = n_pixels)

Input_Berlin_grid$id_pixel <- c(sapply(1:n_time, FUN = function(i) 
  raster::values(krg_grid[[1]])))

# timestamp as BerkeleyJ required by SCOPE
Input_Berlin_grid$t <- as.character(sapply(1:n_time, FUN = function(i) 
  rep(REddyProc::POSIXctToBerkeleyJulianDate(ts19_20[i]), n_pixels)))
####################################################################################

####################################################################################
# include the SCOPE inputs from the interpolation - meteorological DWD data
####################################################################################
# air temperature
Input_Berlin_grid$Ta <- c(sapply(1:n_time, FUN = function(i) values(krg_Ta[[i]])))

# relative humidity
Input_Berlin_grid$RH <- c(sapply(1:n_time, FUN = function(i) values(krg_RH[[i]])))

summary(Input_Berlin_grid$RH)
Input_Berlin_grid$RH[which(Input_Berlin_grid$RH >= 100)] <- 99.9

# air pressure
Input_Berlin_grid$p <- c(sapply(1:n_time, FUN = function(i) values(krg_p[[i]])))

# atm vapour pressure
Input_Berlin_grid$ea <- c(sapply(1:n_time, FUN = function(i) values(krg_ea[[i]])))

# wind speed
Input_Berlin_grid$ws <- c(sapply(1:n_time, FUN = function(i) values(krg_ws[[i]])))

summary(Input_Berlin_grid$ws)
Input_Berlin_grid$ws[which(Input_Berlin_grid$ws <= 0.5)] <- 0.5 # SCOPE requires

# shortwave solar radiation
Input_Berlin_grid$Rin <- c(sapply(1:n_time, FUN = function(i) values(krg_Rin[[i]])))
summary(Input_Berlin_grid$Rin)
Input_Berlin_grid$Rin[which(Input_Berlin_grid$Rin < 0)] <- 0

# longwave solar radiation
summary(solar_radiation_3987)
Input_Berlin_grid$Rli <- c(sapply(1:n_time, FUN = function(i) rep(solar_radiation_3987$Rli[i], 
                                                                  n_pixels)))
summary(Input_Berlin_grid$Rli)
Input_Berlin_grid$Rli[which(Input_Berlin_grid$Rli < 205)] <- 205 #minimum expected

# SMC 60cm
Input_Berlin_grid$SMC60 <- c(sapply(1:n_time, FUN = function(i) krg_SMC60[[i]]))
summary(Input_Berlin_grid$SMC60)
Input_Berlin_grid$SMC60[Input_Berlin_grid$SMC60 <= 0] <- 0.01

# calculation of sun geometry
####################################################################################
# tts
Input_Berlin_grid$tts <- c(sapply(1:n_time, FUN = function(i) 
  rep(photobiology::sun_zenith_angle(lubridate::ymd_hms(ts19_20[i] + minutes(30)),
                                     geocode = data.frame(lat = 52.5, lon = 13.4)), n_pixels)))

summary(Input_Berlin_grid$tts)
Input_Berlin_grid$tts[Input_Berlin_grid$tts >= 85] <- 85 # SCOPE limit

# psi
# Input_Berlin_grid$psi <- c(sapply(1:n_time, FUN = function(i) rep(abs((
#   photobiology::sun_azimuth(lubridate::ymd_hms(ts19_20[i] + minutes(30)))-0)-180), n_pixels)))
# 
# summary(Input_Berlin_grid$psi)
####################################################################################

####################################################################################
# vegetation parameters
####################################################################################
# LAI
# plot(LAI_1km[[4000]])
Input_Berlin_grid$LAI <- c(sapply(1:n_time, FUN = function(i) 
  raster::values(LAI_1km[[i]])))

summary(Input_Berlin_grid$LAI)
Input_Berlin_grid$LAI[which(is.na(Input_Berlin_grid$LAI) == T)] <- 0.101
Input_Berlin_grid$LAI[Input_Berlin_grid$LAI < 0.1] <- 0.101

# vegetation height
Input_Berlin_grid$hc_vh <- rep(raster::values(VH_1km), n_time)

summary(Input_Berlin_grid$hc_vh)
Input_Berlin_grid$hc_vh[which(is.na(Input_Berlin_grid$hc_vh) == T)] <- 0.0202
Input_Berlin_grid$hc_vh[which(Input_Berlin_grid$hc_vh <= 0.02)] <- 0.0202

###################################################################################
# masked pixels out side of Berlin borders
####################################################################################
krg_mask <- raster::mask(krg_grid, Berlin_buffer_utm)
cellNA <- ifelse(values(krg_mask) >= 0, 1, NA)
Input_Berlin_grid$cellNA <- rep(cellNA, n_time)

###################################################################################
# assess and save
###################################################################################
summary(Input_Berlin_grid)

Input_Berlin_grid %>%
  dplyr::filter(id_pixel == 1169 | id_pixel == 1217 | # ROTH
         id_pixel == 882 | id_pixel == 930 |   # TUCC
         id_pixel == 263 | id_pixel == 786 ) %>%   # Higher and lower vegetated pixels
  ggplot(aes(x = t, group = id_pixel)) +
  geom_line(aes(y = LAI, colour = factor(id_pixel)), size = 1)

unique(rep(c(sapply(1:n_time, FUN = function(i) raster::values(krg_grid[[1]])) == Input_Berlin_grid$id_pixel)))
unique(rep(raster::values(krg_grid), n_time) == Input_Berlin_grid$id_pixel)

# saveRDS(Input_Berlin_grid, file = "Input_Berlin_grid.rds")
# Input_Berlin_grid <- readRDS("Input_Berlin_grid.rds")
####################################################################################

###################################################################################
# change from data organized by timestamp (for all pixels) to by pixel (for all timestamps)
##############################################################################
Inputs_Berlin_pixel <- Input_Berlin_grid[with(Input_Berlin_grid, 
                                              order(id_pixel, id_time)),]

# saveRDS(Inputs_Berlin_pixel, file = "Inputs_Berlin_pixel.rds")
# Inputs_Berlin_pixel <- readRDS("Inputs_Berlin_pixel.rds")
##############################################################################

# take out the NA out of the Berlin borders (inputs organized by pixel)
##############################################################################
# table(is.na(cellNA))

Inputs_Berlin_pixel %>% dplyr::filter(!is.na(cellNA)) -> Inputs_Berlin

summary(Inputs_Berlin)

# saveRDS(Inputs_Berlin, file = "Inputs_Berlin.rds")
write.csv(Inputs_Berlin, file = "Inputs_Berlin.csv")
# Inputs_Berlin_pixel <- readRDS("Inputs_Berlin_pixel.rds")
#################################################################################

