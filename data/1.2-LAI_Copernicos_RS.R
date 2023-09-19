library(rSCOPE)
library(RCurl)
library(ncdf4)
library(terra)
library(raster)
library(sf)
library(tidyverse)

# Download LAI images
##################################################################################
# generate the links
LAI_links <- get_LAI_links(user_VITO = "my_user_name", # user name
                           pass_VITO = "my_passord", # password
                           destfile = "D:/Data-Modelling/EUcities/",
                           start_date = "2017-12-01",
                           end_date = "2022-09-20")

# download the images
for (i in 1:length(LAI_links)) {
  download.file(LAI_links[[1]][i], 
                destfile = paste0("D:/Data-Modelling/LAI/", 
                                  "LAI_", LAI_links[[2]][i],"_", i,".nc"),  
                method="curl")
}
##################################################################################

##################################################################################
# processing the EU time series 2017-2022
##################################################################################
# file list
file_names <- list.files(path = paste0("D:/Data-Modelling/", "LAI/"),
                         pattern = "*.nc", recursive = TRUE)

file_names <- file_names[1:173]

file_names_patch <- sapply(1:length(file_names), FUN = function(i) 
  paste0("D:/Data-Modelling/", "LAI/", file_names[i]))

# check var name
names(nc_open("D:/Data-Modelling//LAI/2020/LAI_20201231.nc")$var)

# open global images
LAI_copernicus_globe <- terra::rast(file_names_patch, "LAI")
# crop to EU
LAI_EU <- terra::crop(LAI_copernicus_globe, terra::ext(-10.125, 30.125, 29.875, 65.125))
plot(LAI_EU[[121]])

rm(LAI_copernicus_globe)

# layers name and time
names(LAI_EU) <- rev(LAI_links[[2]])
terra::time(LAI_EU) <- lubridate::ymd(rev(LAI_links[[2]]))

# save
terra::writeRaster(LAI_EU, "LAI_EU.tif", overwrite = TRUE)

# open
# LAI_EU <- terra::rast("D:/Research topics/Data-Modelling/EUcities/lai/LAI_EU.tif")

time(LAI_EU)
names(LAI_EU)
##################################################################################
# run for different cities
##################################################################################
LAI_Berlin <-  LAI_daily(star_data = as.Date("2018-01-10", tz="UTC"),
                         end_data = as.Date("2022-09-10", tz="UTC"),
                         LAI_rast = LAI_EU,
                         crop_area = obj_locations_cities$DE_Berlin_TUCC$latlon$buffer_dist)

LAI_Berlin

plot(LAI_Berlin[[c(2,10,30,120)]])
plot(obj_locations_cities$DE_Berlin_TUCC$latlon$buffer_dist, add = T)
plot(obj_locations_cities$DE_Berlin_TUCC$latlon$buffer_fetch, add = T)
plot(obj_locations_cities$DE_Berlin_TUCC$latlon$border, add = T)

terra::writeRaster(LAI_Berlin, "D:/Data-Modelling/LAI/LAI_Berlin.tif", overwrite=TRUE)

prj = "+proj=longlat +datum=WGS84"
prj_utm = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"

LAI_Berlin_utm <- terra::project(LAI_Berlin, prj_utm)
terra::writeRaster(LAI_Berlin_utm, "D:/Data-Modelling/LAI/LAI_Berlin_utm.tif", overwrite=TRUE)
