library(raster)
library(rgdal) # for spTransform
library(ncdf4) # read .nc data
library(sf)
library(lubridate)
library(ggplot2)
library(REddyProc) #POSIXctToBerkeleyJulianDate
library(spatialEco)
##########################################################################
##### LAI - Copernicus 300m product
##########################################################################
# set login and password in the portal https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home
#### download LAI 300m from the Copernicus product portal - https://land.copernicus.eu/global/products/lai
#### Portal Link - https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Browse;Root=512260;Collection=1000062;Time=NORMAL,NORMAL,-1,,,-1,,
#### Example of file c_gls_LAI300_202009300000_GLOBE_PROBAV_V1.0.1.nc
# List all the files downloaded in the folder 

##########################################################################
# library(RCGLS) # this package can be used to download
# data <- download_CGLS_data(username="login", password="******", 
#                            timeframe=seq(as.Date("2019-06-01"), as.Date("2019-06-15"), by="days"),
#                            product="lai", resolution="300m", version="v1")
##########################################################################
# downloaded files with spatialEco
# lai <- cgls_urls(resolution = 300, product = "lai") # for all available files
# head(basename(lai))

# Create date string for query
d <- seq(as.Date("2017-05-01"), as.Date("2021-06-01"), by="day")

# Search for 300m (333m) LAI within specified date range 
all.urls <- cgls_urls(dates = d, resolution = "300", product = "lai")	

for(i in 1:length(all.urls)){
 if(i > 1){ Sys.sleep(3) }
 file.url <- paste0("https://", paste("login_username", "password****", sep=":"),
                    "@", sub(".*//", "", all.urls[i]))  
    download.file(file.url, file.path(getwd(), 
 	              basename(all.urls[i])), mode = 'wb') 
   }

##########################################################################
# List all the files downloaded in the folder 
file_names <- list.files(path = "D:/Research topics/Data-Modelling/data/LAI300m/",
                         pattern = "*.nc", full.names = TRUE)

LAI_copernicus <- sapply(1:length(file_names), 
                         FUN = function(i) raster::raster(file_names[i]))
LAI_copernicus <- raster::stack(LAI_copernicus)

plot(LAI_copernicus[[1]])

krg_grid_latlon <- raster::projectRaster(krg_grid, crs = crs(LAI_copernicus))

LAI_grid_lat <- raster::crop(LAI_copernicus, raster::extent(krg_grid_latlon))
plot(LAI_grid_lat[[10]])
plot(Berlin_border_longlat, add = T , col="transparent")
summary(LAI_grid_lat[[10]])

LAI_grid <- raster::projectRaster(LAI_grid_lat, crs = crs(krg_grid), method = "ngb")
plot(LAI_grid[[10]])
plot(Berlin_border_utm, add = T , col="transparent")
summary(LAI_grid[[10]])

LAI_grid <- raster::mask(LAI_grid, Berlin_buffer_utm)
plot(LAI_grid[[10]])
plot(Berlin_border_utm, add = T , col = "transparent")

# raster::writeRaster(LAI_grid, filename = "LAI_grid", overwrite = TRUE)
# LAI_grid <- raster::brick("LAI_grid")

#####################################################################################################
# get the image date from the file names
file_date <- sapply(1:length(file_names), FUN=function(i) substring(file_names[i], 61, 71)) # year_month_day

names(LAI_grid) <- sapply(1:length(file_names), FUN = function(i) 
  paste0("date_", substring(file_date[i], 1, 4), "-",
         substring(file_date[i], 5, 6), "-",
         substring(file_date[i], 7, 8), "" ))

#####################################################################################################
# check for min negative values
for (i in 1:length(file_names)) {
  print(names(LAI_grid[[i]]))
  print(summary(LAI_grid[[i]])[1])
}

# exclude negative values
for (i in 1:nlayers(LAI_grid)) {
  LAI_grid[[i]][LAI_grid[[i]] <= 0] <- NA
}

#####################################################################################################
# fill gaps and NA
LAI_fill <- raster::approxNA(LAI_grid, rule = 2)
plot(LAI_fill[[1:4]])
plot(LAI_fill[[34]])
plot(LAI_fill[[84]])

LAI_fill[[39]]
LAI_fill[[111]]
#####################################################################################################
library(lubridate)
# create a timestamp
tsLAI <- seq(as.POSIXct("2019-01-01  00:00:00 UTC", tz="UTC"),
             as.POSIXct("2020-12-31  23:00:00 UTC", tz="UTC"),
             by = "hour") #"30 min"

tsLAI <- force_tz(tsLAI, tz = "UTC")

head(tsLAI)
tail(tsLAI)
length(tsLAI)

# give the name like the image for all timestamps
tsLAI_names <- sapply(1:length(tsLAI), FUN=function(i) 
  paste0("date_", 
         substring(lubridate::date(tsLAI[i]), 1, 4),".",
         substring(lubridate::date(tsLAI[i]), 6, 7),".",
         substring(lubridate::date(tsLAI[i]), 9, 10),"_",
         lubridate::hour(tsLAI[i])))

names(LAI_fill[[39]]) <- "date_2019-01-01"
names(LAI_fill) <- paste0(names(LAI_fill),"_12")

#####################################################################################################
# keep images from 2019 and 2020
LAI_19_20 <- LAI_fill[[39:111]]
plot(LAI_19_20[[1:4]])
plot(LAI_19_20[[73]])
#####################################################################################################
# create a null raster 
LAI_raster_null <- LAI_19_20[[1]]
values(LAI_raster_null) <- NA

# replicate per hour for the entire timestamp
LAI_hour <- c(replicate(length(tsLAI_names), LAI_raster_null))
LAI_hour <- stack(LAI_hour)
LAI_hour[[13]]
names(LAI_hour) <- tsLAI_names # include the names

# include the real images at midday of the date of the image
for (i in 1:nlayers(LAI_19_20)) {
  LAI_hour[[names(LAI_19_20)[i]]] <- LAI_19_20[[i]]
}

LAI_hour
plot(LAI_hour[[1]])
plot(LAI_hour[[10]])
plot(LAI_hour[[13]])
plot(Berlin_border_utm, add = T , col="transparent")
plot(LAI_hour[[17533]])

length(tsLAI_names)
tail(tsLAI_names)
head(tsLAI_names)
#####################################################################################################
# interpolate for all hours
LAI_Berlin <- approxNA(LAI_hour, rule = 2)
names(LAI_Berlin) <- tsLAI_names

plot(LAI_Berlin[[130]])
plot(Berlin_border_utm, add = T , col="transparent")
plot(LAI_Berlin[[1]])
plot(LAI_Berlin[[17544]])

# writeRaster(LAI_Berlin, "LAI_Berlin", overwrite = TRUE)
# LAI_Berlin <- raster::brick("LAI_Berlin")

plot(LAI_Berlin[[23]])
plot(Berlin_border_utm, col="transparent", add=T, lwd=2)

####################################################################################
# mask water bodies before rescale to 1km
# impervious cover map
Impreviousmap <- get_fisbroker_map(url = "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/sach_nutz2015_nutzsa")

# extract water bodies polygons
water_polygons <- Impreviousmap[Impreviousmap$typklar == 'Gewässer',]["typklar"]
plot(water_polygons)
#write_sf(water_polygons, paste0("D:/Research topics/Data-Modelling/data/Images", "/", "water_polygons.shp"), delete_layer = TRUE)

water_polygons$typklar <- rep(NA,697)
water_raster <- fasterize::fasterize(water_polygons, field = "typklar", LAI_Berlin[[1]], 
                                     background = 0, fun = "any")
plot(water_raster)

# mask with the water bodies 
LAI_Berlin_WaterMask <- raster::mask(LAI_Berlin, water_polygons, inverse = T)
plot(LAI_Berlin_WaterMask[[5222]])
plot(Berlin_border_utm, add = T , col = "transparent")

summary(LAI_Berlin_WaterMask[[5222]])
####################################################################################

# resample to 1 km resolution
############################################################################
LAI_1km <- raster::resample(LAI_Berlin_WaterMask, krg_grid)
plot(LAI_1km[[23]])
plot(Berlin_border_utm, add = T , col="transparent")

summary(LAI_1km[[23]])

# writeRaster(LAI_1km, "LAI_1km", overwrite = TRUE)
# LAI_1km <- raster::brick("LAI_1km")

# if 300m was used instead of 1km grid
LAI_Berlin_crop <- raster::crop(LAI_1km$date_2019.06.28_12, Berlin_border_utm)
plot(LAI_Berlin_crop)
plot(Berlin_border_utm, add = T , col="transparent")

LAI_Berlin_crop_mask <- raster::mask(LAI_Berlin_crop, Berlin_buffer_utm)
plot(LAI_Berlin_crop_mask)
plot(Berlin_border_utm, col="transparent", add=T, lwd=2)

cellNA_LAI <- ifelse(values(LAI_Berlin_crop_mask) >= 0, 1, NA)
which(!is.na(cellNA_LAI))

# 13.322 pixels against 1097 from the 1km 
###############################################################################


