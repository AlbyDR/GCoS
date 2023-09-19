library(raster)
library(rSCOPE)
library(curl)
library(terra)
library(tidyverse)

# Global Canopy Height 2020

##################################################################################
# get GCH image link
##################################################################################
# run to get data
get_GCH(city_border = NA,
        i_city = "city_name_1",
        N_S_degree = 52.51228,
        W_E_degree = 13.32786,
        dest_file = "Data-Modelling/Canopy_Height/")

# for namy location (obj_locations_cities is a list of city coordination)
lapply(1:length(obj_locations_cities), FUN = function(i) 
  get_GCH(city_border = obj_locations_cities,
          i_city = i,
          N_S_degree = obj_locations_cities[[i]]$latlon$points[[1]][[1]][2],
          W_E_degree = obj_locations_cities[[i]]$latlon$points[[1]][[1]][1],
          dest_file = "D:/Data-Modelling/Canopy_Height/"))


##################################################################################
# open the GCH downloaded
#######################################################################################

##################################################################################
# run to open CGH
##################################################################################
list_GCH <- lapply(1:13, FUN = function(i)
  list.files("D:/Data-Modelling/Canopy_Height/downloaded/",
             pattern = names(obj_locations_cities)[i]))

GCH_Amsterdam <- open_GCH(crop_area = obj_locations_cities[[12]]$latlon$buffer_dist,
                          list_GCH = list_GCH[[12]], 
                          city_name = names(obj_locations_cities)[12],
                          patch_file = "D:/Data-Modelling/Canopy_Height/downloaded/")

plot(GCH_Amsterdam[[i]])
plot(obj_locations_cities[[12]]$latlon$border, add=T)
plot(obj_locations_cities[[12]]$latlon$points, add=T)

terra::writeRaster(GCH_Amsterdam, "D:/Research topics/Data-Modelling/EUcities/Canopy_Height/GCH_Amsterdam.tif")
#GCH_Amsterdam <- terra::rast("D:/Research topics/Data-Modelling/EUcities/Canopy_height/GCH_Amsterdam.tif")

GCH_Amsterdam_utm <- terra::project(GCH_Amsterdam, prj_utm)
terra::writeRaster(GCH_Amsterdam_utm, "D:/Research topics/Data-Modelling/EUcities/Canopy_height/GCH_Amsterdam_utm.tif")


# Amsterdam by hand

site = "Amsterdam"

N_S = "N"
N_S_degree = 52
W_E = "E"
W_E_degree = 4

degree_N_S <- seq(0, 180, 3)[which(data.table::between(N_S_degree, seq(0, 177, 3), seq(3, 180, 3)))]
degree_W_E <- seq(0, 180, 3)[which(data.table::between(W_E_degree, seq(0, 177, 3), seq(3, 180, 3)))]

degree_W_E <- ifelse(degree_W_E < 100, paste0("0", degree_W_E), degree_W_E)

url <- paste0("https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_",
              N_S, degree_N_S, W_E, degree_W_E,"_Map.tif")

download.file("https://share.phys.ethz.ch/~pf/nlangdata/ETH_GlobalCanopyHeight_10m_2020_version1/3deg_cogs/ETH_GlobalCanopyHeight_10m_2020_N51E003_Map.tif",
              destfile = "D:/Data-Modelling/Canopy_Height/ETH_GlobalCanopyHeight_10m_2020_N51E003_Map.tif", 
              method="curl")

canopy_height_Amsterdam <- raster::raster("D:/Data-Modelling/Canopy_Height/ETH_GlobalCanopyHeight_10m_2020_N51E003_Map.tif")
plot(canopy_height_Amsterdam)

canopy_height_Amsterdam <- crop(canopy_height_Amsterdam, extent(Amsterdam_border))

plot(canopy_height_Amsterdam)
plot(Amsterdam_border, color = "transparent", add = TRUE)

terra::writeRaster(canopy_height_Amsterdam, "canopy_height_Amsterdam.tif")
canopy_height_Amsterdam <- terra::rast("canopy_height_Amsterdam")

# vegetation height
raster::extract(canopy_height_Amsterdam, Amsterdam_tower)
summary(unlist(raster::extract(canopy_height_Amsterdam, Amsterdam_tower_buffer)))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 4.00   11.00   13.00   12.91   14.00   19.00   12481