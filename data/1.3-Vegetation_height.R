library(rSCOPE)
library(raster)
library(sf)
library(exactextractr)
library(ggplot2)
library(ggspatial)
library(gghighlight)
library(lubridate)
library(tidyverse)
#############################################################
############ polygons BERLIN ENVIRONMENT ATLAS ##############
#############################################################
# green volume map
Green.blocks <- get_fisbroker_map(url = "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_05_09_gruenvol2010")
Green.street <- get_fisbroker_map(url = "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/wfs_05_09_gruenvol2010_str")

names(Green.blocks)
names(Green.street)

str(filter(Green.blocks, FLALLE==160724.4)) 
####################################################################
#$ gml_id    : chr "s_05_09_gruenvol2010.0700231951000000" #key TUCC
#$ FLALLE    : num 160724  = area
#$ VEGHOE    : num 9.4     = Mean height of vegetation in relation to the area covered with vegetation [m]
#$ VEGPROZ   : num 30.4    = Area covered with vegetation in relation to the total area [%]
#$ VEGVOLA   : int 461279  = Green volume [m³]
#$ VEGVOL    : num 2.87    = Green volume number [m³ / m²]
#$ FLUBEB    : num 87791   = Area size of the above-ground undeveloped area [m²]
#$ VEGHOEUBEB: num 9.4     = Mean height of the vegetation in relation to the area of the undeveloped area covered with vegetation [m]
#$ VEGPROUBEB: num 53.9    = Area covered with vegetation in relation to the undeveloped area [%]
#$ VEGVOLAUBE: int 446854  = Green volume of the undeveloped area [m³]
#$ VEGVOLUBEB: num 5.09    = Number of green volumes related to the area of the undeveloped area covered with vegetation [m³ / m²]
################################################################

# combine both
Green_vol <- rbind(Green.blocks[c(-2,-7)], Green.street[-2])
Green_vol <- sf::st_cast(Green_vol)

ggplot() + 
  geom_sf(data = Green_vol, aes(fill = veghoe), colour = "transparent") +
  geom_sf(data = berlin.sf, fill = "transparent", size = 1.2, color = "black") +
  scale_fill_gradientn(breaks = seq(0, 30, 5), limits = c(0,31),
                       colors = rev(terrain.colors(10)), name = "", na.value = 0,
                       guide = guide_colorbar(direction = "horizontal",
                                              label.position = "bottom",
                                              label.vjust = -2,
                                              frame.colour = "black",
                                              frame.linewidth = 0.5,
                                              frame.linetype = 1,
                                              barwidth=35, barheight=1.5, nbin=10,
                                              label.theme=element_text(angle=0,size=16))) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position="bottom",
        legend.spacing.y = unit(-0.4, "lines"),
        legend.box.spacing = unit(-1.5, "lines"),
        panel.grid.major = element_blank(), 
        panel.background = element_rect("white"))

plot(Green.blocks["veghoe"], border = "transparent")
plot(Green.street["veghoe"], border = "transparent")
plot(Green_vol["veghoe"], border = "transparent")

summary(Green_vol["veghoe"])

# convert to a raster
krg_grid_10m <- raster::disaggregate(krg_grid, fact = 100)
veg_height_raster <- fasterize::fasterize(Green_vol, field = "veghoe", 
                                          krg_grid_10m, fun = "max")
plot(veg_height_raster)
summary(veg_height_raster)

# aggregate to 1km grid
veg_height_r_1km <- raster::aggregate(veg_height_raster, fact = 100)
plot(veg_height_r_1km)
summary(veg_height_r_1km)

# resample to 1km grid
VH_1km <- raster::resample(veg_height_r_1km, krg_grid, "ngb")
plot(VH_1km)
summary(VH_1km)

names(VH_1km) <- "VH_1km"

# raster::writeRaster(VH_1km, "hc_VH_1km", overwrite = TRUE)
# VH_1km <- raster::raster("hc_VH_1km")


####################################################################################
# vegetation fraction (block and streets)
####################################################################################
veg_fraction <- Green_vol["vegproz"]
names(veg_fraction)[1] <- "veg_fraction"
veg_fraction$vf <- Green_vol$vegproz/100

summary(veg_fraction)
plot(veg_fraction["veg_fraction"], border = "transparent")

#saveRDS(veg_fraction, "veg_fraction")
####################################################################################


############################################################################
# LULC Copernicus Urban Atlas 2018
LCLU_Uatlas <- sf::st_read("C:/Users/.../Documents/UWI/R_SCOPE/DE001L1_BERLIN_UA2018_v013/Data/DE001L1_BERLIN_UA2018_v013.gpkg")
LCLU_Uatlas <- sf::st_transform(LCLU_Uatlas, crs = sf::st_crs(Berlin_buffer_utm))

# crop to Berlin Area
LCLU_berlin <- sf::st_crop(LCLU_Uatlas, Berlin_buffer_utm)

unique(LCLU_Uatlas_berlinb$class_2018)
unique(LCLU_Uatlas_berlinb$code_2018)

LCLU_b <- LCLU_berlin["class_2018"]
LCLU_b$classII <- LCLU_berlin$class_2018

LCLU_b$classII[LCLU_berlin$class_2018 == "Fast transit roads and associated land"] <- "Roads"
LCLU_b$classII[LCLU_berlin$class_2018 == "Other roads and associated land"] <- "Roads"
LCLU_b$classII[LCLU_berlin$class_2018 == "Railways and associated land"] <- "Roads"

LCLU_b$classII[LCLU_berlin$class_2018 == "Arable land (annual crops)"] <- "Crops and pastures"
LCLU_b$classII[LCLU_berlin$class_2018 == "Complex and mixed cultivation patterns"] <- "Crops and pastures"
LCLU_b$classII[LCLU_berlin$class_2018 == "Pastures"] <- "Crops and pastures"

LCLU_b$classII[LCLU_berlin$class_2018 == "Construction sites"] <- "Other articifial areas"
LCLU_b$classII[LCLU_berlin$class_2018 == "Isolated structures"] <-  "Other articifial areas"
LCLU_b$classII[LCLU_berlin$class_2018 == "Land without current use"] <-  "Other articifial areas"
LCLU_b$classII[LCLU_berlin$class_2018 == "Mineral extraction and dump sites"] <-  "Other articifial areas"
LCLU_b$classII[LCLU_berlin$class_2018 == "Airports"] <- "Other articifial areas"
LCLU_b$classII[LCLU_berlin$class_2018 == "Port areas"] <- "Other articifial areas"

LCLU_b$classII[LCLU_berlin$class_2018 == "Sports and leisure facilities"] <- "Sports and leisure"

LCLU_b$classII[LCLU_berlin$class_2018 == "Industrial, commercial, public, military and private units"] <- "Industrial-commercial-public"
LCLU_b$classII[LCLU_berlin$class_2018 == "Continuous urban fabric (S.L. : > 80%)"] <-  "Urban fabric (>80%)"
LCLU_b$classII[LCLU_berlin$class_2018 == "Discontinuous dense urban fabric (S.L. : 50% -  80%)"] <-  "Urban fabric (50%-80%)"
LCLU_b$classII[LCLU_berlin$class_2018 == "Discontinuous medium density urban fabric (S.L. : 30% - 50%)"] <-  "Urban fabric (30%-50%)"
LCLU_b$classII[LCLU_berlin$class_2018 == "Discontinuous low density urban fabric (S.L. : 10% - 30%)"] <- "Urban fabric (10%-30%)"
LCLU_b$classII[LCLU_berlin$class_2018 == "Discontinuous very low density urban fabric (S.L. : < 10%)"] <- "Urban fabric (10%)"

LCLU_b$classII[LCLU_berlin$class_2018 == "Herbaceous vegetation associations (natural grassland, moors...)"] <- "Herbaceous vegetation"
LCLU_b$classII[LCLU_berlin$class_2018 == "Open spaces with little or no vegetation (beaches, dunes, bare rocks, glaciers)"] <- "Natural, no vegetation"

unique(LCLU_b$classII)
t(table(LCLU_berlin$class_2018))

paletteLULC <- c(
  "Urban fabric (>80%)" =  "#800000",
  "Urban fabric (50%-80%)"  =  "#BE0000",
  "Urban fabric (30%-50%)" =  "#FE4040",
  "Urban fabric (10%-30%)" =  "#FE8081",
  "Urban fabric (10%)" =  "#FFBFBF",
  "Industrial-commercial-public" =  "#CC4DF2",
  "Other articifial areas" =  "#E6CCE7",
  "Roads"=  "#B3B3B3",
  "Natural, no vegetation" ="#CDFFCC",
  "Sports and leisure" =  "#AED2A4",
  "Green urban areas" = "#8CDC00",
  "Herbaceous vegetation" ="#CCF24D",
  "Forests" = "#008C00",
  "Crops and pastures" = "#FFFFA7",
  "Water"  = "#80F3E5",
  "Wetlands" = "#A6A6FE")

LabelsLULC <- c(
  "Urban fabric (>80%)",
  "Urban fabric (50%-80%)",
  "Urban fabric (30%-50%)",
  "Urban fabric (10%-30%)",
  "Urban fabric (10%)",
  "Industrial-commercial-public",
  "Other articifial areas",
  "Roads",
  "Natural, no vegetation",
  "Sports and leisure",
  "Green urban areas",
  "Herbaceous vegetation",
  "Forests",
  "Crops and pastures",
  "Water",
  "Wetlands")

ggplot(LCLU_b) +
  geom_sf(aes(fill = classII), colour = NA) +
  geom_sf(data = Berlin_border_utm, fill = "transparent", size = 1.2, color = "black") +
  scale_fill_manual(values = paletteLULC,
                    labels = LabelsLULC,
                    breaks = LabelsLULC,
                    name = "LU/LC")
####################################################################################


# Biotype
###################################################################################
Biotope <- get_fisbroker_map(url = "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_fb_berlinbtk")

# subset some forest types to make it plot faster
Forest_biotope <- Biotope[Biotope$BT_NAME %in% unique(filter(Biotope, grepl('forst', BT_NAME, fixed = TRUE))$BT_NAME), ]
plot(Forest_biotope["BT_NAME"])

Pine <- Biotope[Biotope$BT_NAME %in% unique(filter(Biotope, grepl('Kiefern', BT_NAME, fixed = TRUE))$BT_NAME), ]
Beech <- Biotope[Biotope$BT_NAME %in% unique(filter(Biotope, grepl('Buche', BT_NAME, fixed = TRUE))$BT_NAME), ]
Oak <- Biotope[Biotope$BT_NAME %in% unique(filter(Biotope, grepl('Eiche', BT_NAME, fixed = TRUE))$BT_NAME), ]

Pine <- st_collection_extract(Pine)
Oak <- st_collection_extract(Oak)
Beech <- st_collection_extract(Beech)
###################################################################################

