library(tidyverse)
library(lubridate)
library(summarytools)
library(sf)
library(raster)
library(exactextractr)
library(ggplot2)
library(RColorBrewer)

########################################################################
names(Berlin2020_pred) # list the name of all outputs
########################################################################

summary(Berlin2020_pred)

# Average ET per month (without urban correction)
Berlin2020_pred %>%
  mutate_if(is.numeric, pmax, 0) %>% # convert negative ET to 0
  mutate(month = month(timestamp)) %>%
  group_by(month, id_pixel) %>%
  summarise(ET_month = sum(ET)) %>%
  descr(stats = c("mean", "min", "max"), 
        transpose = T, headings = F, Data.frame=T)

#Berlin2020_pred <- data.table::fread("Berlin2020_pred.csv")[,-1]

# map annual urban ET (with urban correction)
Urban_ET_map <- map_urbanET(dataset = Berlin2020_pred,
                            input_raster = krg_grid, 
                            NA_cells = cellNA,  
                            Input_vector = Green_vol,
                            veg_fraction = "vegproz",
                            function_var = sum,
                            function_time = data.table::year,
                            output_vars = c("ET", "ET_soil", "ET_canopy"),
                            period_var = "annual",
                            extract_fun = 'max')

plot(Urban_ET_map_func, border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)

# map urban ET in the hottest day (with urban correction)
Urban_ET_map_hotday <- map_urbanET(dataset = Berlin2020_pred[data.table::as.IDate(timestamp) == "2020-08-08",], # hottest day subset
                                   input_raster = krg_grid, 
                                   NA_cells = cellNA,  
                                   Input_vector = Green_vol,
                                   veg_fraction = "vegproz",
                                   function_var = sum,
                                   function_time = data.table::yday,
                                   output_vars = c("ET"),
                                   period_var = "hottest_day",
                                   extract_fun = 'max')

plot(Urban_ET_map_hotday, border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)

# map 24 hours urban ET in a specific day (with urban correction)
Urban_ET_map_24hour <- map_urbanET(dataset = Berlin2020_pred[data.table::as.IDate(timestamp) == "2020-08-08",], # hottest day subset
                                   input_raster = krg_grid, 
                                   NA_cells = cellNA,  
                                   Input_vector = Green_vol,
                                   veg_fraction = "vegproz",
                                   function_var = sum,
                                   function_time = data.table::hour,
                                   output_vars = c("ET"),
                                   period_var = "hottest_day",
                                   extract_fun = 'max')

plot(Urban_ET_map_24hour[c(3,25,49)], border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)

summary(Urban_ET_map_24hour[seq(3,49,2)])

# animate map of hourly urban ET

# create a pallet for ET
ETcolor = rev(c(rep("#4575b4",1),rep("#74add1",1),rep("#abd9e9",1), rep("#e0f3f8",1),
                rep("#ffffbf",1),rep("#fee090",1),rep("#fdae61",1),rep("#f46d43",1), 
                rep("#d73027",1),  rep("#d73027",1), rep("#a50026",1), rep("#a50026",1)))

create_GIF(
  map =  Urban_ET_map_24hour,
  period = "24hours",
  gif_subtitle = "Urban ET [mm/hour] - Berlin 08-08-2020",
  water_mask = water_polygons,
  border = Berlin_border_utm,
  limits_scale = c(0, 0.60),
  breaks_scale = seq(0, 0.60, 0.05),
  colors_pallete = ETcolor,
  time_interval = 0.8,
  loops = 1,
  movie_name = "Urban_ET_24h_08_08_2020.gif")

# map monthly urban ET (with urban correction)
Urban_ET_map_month <- map_urbanET(input_raster = krg_grid,
                                  NA_cells = cellNA,  
                                  Input_vector = Green_vol,
                                  veg_fraction = "vegproz",
                                  dataset = Berlin2020_pred,
                                  function_var = sum,
                                  function_time = data.table::month,
                                  output_vars = c("ET"),
                                  period_var = "monthly",
                                  extract_fun = 'max')

plot(Urban_ET_map_month[c(3,9,17,21)], border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)

# animate map of monthly urban ET
create_GIF(
  map =  Urban_ET_map_month,
  period = "month",
  gif_subtitle = "Urban ET [mm/month] - Berlin 2020",
  water_mask = water_polygons,
  border = Berlin_border_utm,
  limits_scale = c(0, 100),
  breaks_scale = seq(0, 100, 20),
  colors_pallete = ETcolor,
  time_interval = 0.8,
  loops = 1,
  movie_name = "Urban_ET_2020.gif")

# map urban ET per season (with urban correction)
Urban_ET_map_quarter <- map_urbanET(dataset = Berlin2020_pred, # data.table with datetime, id_pixel and SCOPE outputs
                                    input_raster = krg_grid, # raster used to interpolate and modelling
                                    NA_cells = cellNA, # vector with the NA cells mask by the city (Berlin) border  
                                    Input_vector = Green_vol, # vector map (sf) with the vegetation fraction
                                    veg_fraction = "vegproz", # name of the vegetation fraction var in the map (sf)
                                    function_var = sum, # function to summarize (sum, mean, max, min)
                                    function_time = quarter, # time to summarize (year, quarter, month, week, yday, hour). If hour or day maybe is need to reduce the range of the timestamp before run
                                    output_vars = c("ET", "ET_canopy"), # possible outputs (ET, ET_soil, Tsave - see names(Berlin2020_pred)). If many, better year, quarter or up tp month
                                    period_var = "quarter", # name to enumerate the period
                                    extract_fun = 'max') # function to extract the raster values into the vector. If the raster is high-resolution use "mean",  otherwise "max" (e.g. 1km grid) 

plot(Urban_ET_map_quarter[c(3,7,11,15)], border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)

#########################################################################
# map urban Cooling Services
Cooling_maps_2000 <- map_urban_cooling(dataset = Berlin2020_pred,
                                       date_hottest = "2020-08-08",
                                       input_raster = krg_grid, 
                                       NA_cells = cellNA,  
                                       Input_vector = Green_vol,
                                       veg_fraction = "vegproz",
                                       output_vars = c("ET", "Tsave"),
                                       extract_fun = 'max')

summary(Cooling_maps_2000)

plot(Cooling_maps_2000[c(3,4,5)], border = "transparent", nbreaks=11, 
     pal=RColorBrewer::brewer.pal('RdYlBu', n = 11), reset=FALSE)


#########################################################################
#########################################################################
write_sf(Cooling_Maps, "D:/Research_topics/UWI/Data_UWI/Cooling_Maps.gpkg")
Cooling_Maps <- read_sf("Cooling_Maps.gpkg")

