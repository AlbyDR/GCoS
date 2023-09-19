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
impervious_Vienna <- terra::rast("D:/Research topics/Data-Modelling/EUcities/LULC/AT/non_impervious_Vienna.tif")

plot(impervious_Vienna)

