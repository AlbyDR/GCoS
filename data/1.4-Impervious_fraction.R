####################################################################################
library(rSCOPE)
library(terra)
####################################################################################
# download the impervious fraction from the link
# https://land.copernicus.eu/pan-european/high-resolution-layers/imperviousness/status-maps/imperviousness-density-2018

impervious_Vienna <- get_nonimpervious(file_patch = "D:/Data-Modelling/LULC/AT/IMD_2018_010m_at_03035_v020/DATA/",
                                       file_pattern = "_v020.tif$",
                                       bbox = obj_locations_cities$AT_Vienna$laea$buffer_dist,
                                       prj = prj_utm)

plot(impervious_Vienna)
plot(obj_locations_cities$AT_Vienna$utm$buffer_dist , add = TRUE)
plot(obj_locations_cities$AT_Vienna$utm$border, add = TRUE)
plot(obj_locations_cities$AT_Vienna$utm$buffer_fetch, add = TRUE)
plot(obj_locations_cities$AT_Vienna$utm$points, add = TRUE)

terra::writeRaster(impervious_Vienna, "D:/Data-Modelling/LULC/AT/impervious_Vienna.tif", filetype = "GTiff", overwrite=TRUE)

# download the Urban Atlas set from the link
# https://land.copernicus.eu/local/urban-atlas/urban-atlas-2018

LULC_Vienna <-  sf::st_read(paste0("D:/Data-Modelling/LULC/Vienna/",
                                   "AT001L3_WIEN_UA2018_v013/Data/", 
                                   "AT001L3_WIEN_UA2018_v013.gpkg"))

LULC_Vienna <- sf::st_transform(LULC_Vienna, crs = crs(Border_Vienna))
LULC_Vienna <- sf::st_transform(LULC_Vienna, crs = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

# water LULC
LULC_Vienna %>%
  filter(class_2018 %in% c("Water")) -> LULC_water_Vienna

LULC_water_Vienna$water <- 1

plot(LULC_water_Vienna["water"])

LULC_water_Vienna <- fasterize::fasterize(LULC_water_Vienna, impervious_Vienna, field = "water")
LULC_water_Vienna[is.na(LULC_water_Vienna[])] <- 0 

LULC_water_Vienna <- terra::rast(LULC_water_Vienna)
