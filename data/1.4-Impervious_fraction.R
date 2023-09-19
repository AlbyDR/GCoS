####################################################################################
library(rSCOPE)
library(ggplot2)
library(ggspatial)
library(sf)
library(raster)
library(pbapply)
####################################################################################

####################################################################################
# air temperature downloaded dataset
summary(Air_temp[[1]]$MESS_DATUM)
length(Air_temp[[1]]$MESS_DATUM)

# Berlin border object to create a grid that include all the city
Berlin_border_utm <- sf::st_transform(berlin.sf, "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
Berlin_buffer_utm = sf::st_buffer(Berlin_border_utm, 1000) #buffer of 1km or 1 pixel

krg_Ta <- pbapply::pblapply(1:length(Air_temp[[1]]$MESS_DATUM), FUN = function(i) 
  interpolate_DWD(var_DWD = as.numeric(Air_temp[[1]][i,-1]),
                  stations_DWD = Air_temp[[2]],
                  res_utm = 1000,
                  radius_km = 70,
                  vgm_model = c("Sph"),
                  crop_extent = raster::extent(Berlin_buffer_utm))) # 

krg_Ta[[1]]
plot(krg_Ta[[8752]])
plot(krg_Ta[[17544]])

# Plot the interpolation for 1 timestamp
ggplot() +
  layer_spatial(krg_Ta[[7]]) + 
  scale_fill_continuous(na.value = NA) + 
  layer_spatial(Berlin_buffer_utm, fill = "transparent", size = 0.5, col = "white") + 
  layer_spatial(Berlin_border_utm, fill = "transparent", size = 1, col = "black") + 
  geom_spatial_point(aes(x = Air_temp[[2]]$longitude, y = Air_temp[[2]]$latitude), 
                     size = 2, col = "red") #

# check if there is na values/raters in the list 
unique(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_Ta[[i]]))))))

# saveRDS(krg_Ta, "krg_Ta")
# krg_Ta <- readRDS("krg_Ta")

#####################################################################################
# create a raster template of 1km resolution based on the final interpolation cropped
#####################################################################################
krg_grid <- krg_Ta[[1]]
names(krg_grid) <- "id"
values(krg_grid) <- 1:1920
plot(krg_grid)

# template grid 
plot(krg_grid, col="transparent", axes=FALSE, box=FALSE)
plot(rasterToPolygons(krg_grid), add=TRUE, border='black', lwd=1) 
# grid pixels inside Berlin
krg_grid_mask <- raster::mask(krg_grid, Berlin_buffer_utm)
plot(krg_grid_mask, col="transparent", axes=FALSE, box=FALSE)
plot(rasterToPolygons(krg_grid_mask), add=TRUE, border='black', lwd=1) 
plot(Berlin_border_utm, col="transparent", add=T, lwd=2)

####################################################################################

####################################################################################
# relative humidity
krg_RH <- pbapply::pblapply(1:length(relative_humidity[[1]]$MESS_DATUM), FUN = function(i) 
  interpolate_DWD(
    var_DWD = as.numeric(relative_humidity[[1]][i,-1]),
    stations_DWD = relative_humidity[[2]],
    res_utm = 1000,
    radius_km = 70,
    vgm_model = c("Sph"),
    crop_extent = raster::extent(Berlin_buffer_utm))) # 

krg_RH[[1]]
plot(krg_RH[[17544]])
plot(Berlin_border_utm, add=T, col = "transparent")

unique(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_RH[[i]]))))))

# saveRDS(krg_RH, "krg_RH")
# krg_RH <- readRDS("krg_RH")
####################################################################################

####################################################################################
# air pressure
krg_p <- pbapply::pblapply(1:length(pressure[[1]]$MESS_DATUM), FUN = function(i) 
  interpolate_DWD(
    var_DWD = as.numeric(pressure[[1]][i,-1]),
    stations_DWD = pressure[[2]],
    res_utm = 1000,
    radius_km = 70,
    vgm_model = c("Sph"),
    crop_extent = raster::extent(Berlin_buffer_utm))) # 

krg_p[[1]]
plot(krg_p[[2]])
plot(Berlin_border_utm, add=T, col = "transparent")

unique(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_p[[i]]))))))
which(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_p[[i]]))))))

# saveRDS(krg_p, "krg_p")
# krg_p <- readRDS("krg_p")
####################################################################################

####################################################################################
# vapour pressure
krg_ea <- pbapply::pblapply(1:length(atm_moisture_ea_VP[[1]]$MESS_DATUM), FUN = function(i) 
  interpolate_DWD(
    var_DWD = as.numeric(atm_moisture_ea_VP[[1]][i,-1]),
    stations_DWD = atm_moisture_ea_VP[[2]],
    res_utm = 1000,
    radius_km = 70,
    vgm_model = c("Sph"),
    crop_extent = raster::extent(Berlin_buffer_utm))) # 

krg_ea[[1]]
plot(krg_ea[[17544]])
plot(Berlin_border_utm, add=T, col = "transparent")

unique(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_ea[[i]]))))))

# saveRDS(krg_ea, "krg_ea")
# krg_ea <- readRDS("krg_ea")
####################################################################################

####################################################################################
# wind speed
krg_ws <- pbapply::pblapply(1:length(wind_ws[[1]]$MESS_DATUM), FUN = function(i) 
  interpolate_DWD(var_DWD = as.numeric(wind_ws[[1]][i,-1]),
                  stations_DWD = wind_ws[[2]],
                  res_utm = 1000,
                  radius_km = 70,
                  vgm_model = c("Sph"),
                  crop_extent = raster::extent(Berlin_buffer_utm))) # 

krg_ws[[1]]
plot(krg_ws[[17544]])
plot(Berlin_border_utm, add=T, col = "transparent")

unique(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_ws[[i]]))))))

# saveRDS(krg_ws, "krg_ws")
# krg_ws <- readRDS("krg_ws")
####################################################################################

####################################################################################
# wind direction
krg_wd <- pbapply::pblapply(1:length(wind_wd[[1]]$MESS_DATUM), FUN = function(i) 
  interpolate_DWD(var_DWD = as.numeric(wind_wd[[1]][i,-1]),
                  stations_DWD = wind_wd[[2]],
                  res_utm = 1000,
                  radius_km = 70,
                  vgm_model = c("Sph"),
                  crop_extent = raster::extent(Berlin_buffer_utm))) # 

krg_wd[[1]]
plot(krg_wd[[17544]])
plot(Berlin_border_utm, add=T, col = "transparent")

unique(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_wd[[i]]))))))

# saveRDS(krg_wd, "krg_wd")
# krg_wd <- readRDS("krg_wd")
####################################################################################
Rs_sun
####################################################################################
# solar radiation fro sun duration
krg_Rin <- pbapply::pblapply(1:length(RS_SunD_Rin[[1]]$timestamp), FUN = function(i) 
  interpolate_DWD(var_DWD = as.numeric(RS_SunD_Rin[[1]][i,-1]),
                  stations_DWD = RS_SunD_Rin[[2]],
                  res_utm = 1000,
                  radius_km = 70,
                  vgm_model = c("Sph"),
                  crop_extent = raster::extent(Berlin_buffer_utm))) # 

plot(krg_Rin[[560]])
plot(krg_Rin[[17544]])

krg_Rin[[1]]
plot(krg_Rin[[12]])
plot(Berlin_border_utm, add=T, col = "transparent")

unique(unlist(sapply(1:8760, FUN = function(i) unique(is.na(values(krg_Rin[[i]]))))))

# saveRDS(krg_Rin, "krg_Rin")
# krg_Rin <- readRDS("krg_Rin")
####################################################################################

####################################################################################
#### soil moisture
####################################################################################
####################################################################################
#SMC_60cm
# BFWSL - sandy loam wilting point of 13 volumic % and a field capacity of 37 volumic %. 
SMC60_daily <- sapply(1:length(SMC_daily[[2]]$stations_name), 
                      FUN = function(i) SMC_daily[[1]][[i]]$BFWSL)
summary(SMC60_daily)

krg_SMC60_BFWSL <- pbapply::pblapply(1:730, FUN = function(i) 
  interpolate_DWD(var_DWD = as.numeric(SMC60_daily[i,]),
                  stations_DWD = SMC_daily[[2]],
                  res_utm = 1000,
                  radius_km = 70,
                  vgm_model = c("Sph"),
                  crop_extent = raster::extent(Berlin_buffer_utm)))# 

# convert from daily to hourly, create NA grids for the others 23h
krg_SMC_NULL <- krg_SMC60_BFWSL[[1]]
raster::values(krg_SMC_NULL) <- NA
names(krg_SMC_NULL) <- "SMC60"
SMC_60_hour <- sapply(1:730, FUN = function(i) c(replicate(12,krg_SMC_NULL), 
                                                 krg_SMC60_BFWSL[[i]], 
                                                 replicate(11,krg_SMC_NULL)))

# include 29 of fev as 2020 was a leap year
SMC_60_leap <- c(SMC_60_hour[1:10176], SMC_60_hour[10177:10200], SMC_60_hour[10177:17520])
length(SMC_60_leap)

# convert to brick and stack to interpolate
SMC_60_raster <- sapply(1:17544, FUN = function(i) raster::brick(SMC_60_leap[[i]]))
SMC_60_raster <- raster::stack(SMC_60_raster)

plot(SMC_60_raster[[13]])
plot(SMC_60_raster[[14]])

# interpolate linearly in time from daily to hourly (NA grids)
SMC_60_filled <- raster::approxNA(SMC_60_raster, rule = 2)

plot(SMC_60_filled[[1]])
plot(SMC_60_filled[[13]])
plot(SMC_60_filled[[14]])
plot(SMC_60_filled[[17544]])

# convert field capacity of volumic to volumic % (vol_%)
krg_SMC60 <- lapply(1:17544, FUN = function(i) ((((37-13)*(raster::values(SMC_60_filled[[i]]))) + 13)/100))

krg_SMC60[[1]]
plot(krg_SMC60[[17544]])
plot(Berlin_border_utm, add=T, col = "transparent")

unique(unlist(sapply(1:17544, FUN = function(i) unique(is.na(values(krg_SMC60cm[[i]]))))))

# saveRDS(krg_SMC60, "krg_SMC60")
# krg_SMC60 <- readRDS("krg_SMC60")
####################################################################################
####################################################################################

  ####################################################################################
  ####################################################################################
  # precipitation mm
  # krg_Prec <- pbapply::pblapply(1:length(Prec_mm[[1]]$MESS_DATUM), FUN = function(i) 
  #   interpolate_DWD(var_DWD = as.numeric(Prec_mm[[1]][i,-1]),
  #                   stations_DWD = Prec_mm[[2]],
  #                   res_utm = 1000,
  #                   radius_km = 70,
  #                   vgm_model = c("Sph"),
  #                   crop_extent = raster::extent(Berlin_buffer_utm))) # 
  # 
  # krg_Prec[[1]]
  # plot(krg_Prec[[998]])
  # plot(krg_Prec[[912]])
  # plot(krg_Prec[[976]])
  # plot(krg_Prec[[17544]])
  # plot(Berlin_border_utm, add=T, col = "transparent")
  # 
  # Prec_mm[[1]][998,]
  # Prec_mm[[1]][912,]
  # Prec_mm[[1]][976,]
  # Prec_mm[[1]][17544,]
  # 
  # saveRDS(krg_Prec, "krg_Prec")
  # 
  # unique(unlist(sapply(1:17544, FUN = function(i) unique(is.na(values(krg_Prec[[i]]))))))
  # 
  # summary(filter(Prec_mm[[1]], ID_433>0))
  # which(Prec_mm[[1]][2] < 0.1 & Prec_mm[[1]][2] > 0)
  # Prec_mm[[1]][6530,2]
  # Prec_NA <- unlist(sapply(1:length(Prec_mm[[1]]$MESS_DATUM), FUN = function(i) unique(is.na(values(krg_Prec[[i]])))))
  # 
  # Prec_krg <- krg_Prec
  # 
  # for (i in 1:length(Prec_mm[[1]]$MESS_DATUM)) {    
  #   if(Prec_NA[i] == TRUE){
  #     values(Prec_krg[[i]]) <- rep(0, 1920)
  #   }else{
  #     Prec_krg[[i]][which(values(Prec_krg[[i]]) < 0.05)] <- 0
  #   }
  # }
  # 
  # Prec_krg[[52]]
  # Prec_krg[[936]]
  # Prec_krg[[6530]]
  # 
  # plot(krg_Prec[[17544]])
  # plot(Prec_krg[[17544]])
  # plot(Berlin_border_utm, add=T, col = "transparent")
  
  # saveRDS(Prec_krg, "Prec_krg")
  # Prec_krg <- readRDS("Prec_krg")
  ####################################################################################
  
####################################################################################
####################################################################################
# krg_Ta <- readRDS("krg_Ta")
# krg_RH <- readRDS("krg_RH")
# krg_p <- readRDS("krg_p")
# krg_ws <- readRDS("krg_ws")
# krg_wd <- readRDS("krg_wd")
# krg_ea <- readRDS("krg_ea")
# krg_Rin <- readRDS("krg_Rin")
# krg_SMC60 <- readRDS("krg_SMC60")
# Prec_krg <- readRDS("Prec_krg")
####################################################################################
####################################################################################

# create a df for DWD estimation for the tower location ROTH
DWD_ROTH <- data.frame(
  "Ta" = sapply(1:17544, FUN = function(i) raster::extract(krg_Ta[[i]], data.frame(x = 385566.5, y = 5813229))),
  "RH" = sapply(1:17544, FUN = function(i) raster::extract(krg_RH[[i]], data.frame(x = 385566.5, y = 5813229))),
  "ea" = sapply(1:17544, FUN = function(i) raster::extract(krg_ea[[i]], data.frame(x = 385566.5, y = 5813229))),
  "p" = sapply(1:17544, FUN = function(i) raster::extract(krg_p[[i]], data.frame(x = 385566.5, y = 5813229))),
  "ws" = sapply(1:17544, FUN = function(i) raster::extract(krg_ws[[i]], data.frame(x = 385566.5, y = 5813229))),
  "wd" = sapply(1:17544, FUN = function(i) raster::extract(krg_wd[[i]], data.frame(x = 385566.5, y = 5813229))),
  "Rin" = sapply(1:17544, FUN = function(i) raster::extract(krg_Rin[[i]], data.frame(x = 385566.5, y = 5813229))),
  "Rli" = solar_radiation_3987$Rli,
  "SMC60" = sapply(1:17544, FUN = function(i) raster::extract(krg_SMC60[[i]], data.frame(x = 385566.5, y = 5813229))),
  "prec_mm" = sapply(1:17544, FUN = function(i) raster::extract(Prec_krg[[i]], data.frame(x = 385566.5, y = 5813229)))
)

summary(DWD_ROTH)
DWD_ROTH$Rin[DWD_ROTH$Rin < 0] <- 0
DWD_ROTH$RH[DWD_ROTH$RH > 100] <- 100
DWD_ROTH$Rli[DWD_ROTH$Rli < 200] <- 200
DWD_ROTH$wd[which(DWD_ROTH$wd < 0)] <- DWD_ROTH$wd[which(DWD_ROTH$wd < 0)] + 360
DWD_ROTH$wd[which(DWD_ROTH$wd > 360)] <- DWD_ROTH$wd[which(DWD_ROTH$wd > 360)] - 360

saveRDS(DWD_ROTH, "DWD_ROTH")

# create a df for DWD estimation for the tower location TUCC
####################################################################################
DWD_TUCC <- data.frame(
  "Ta" = sapply(1:17544, FUN = function(i) raster::extract(krg_Ta[[i]], data.frame(x = 386525.1, y = 5819332))),
  "RH" = sapply(1:17544, FUN = function(i) raster::extract(krg_RH[[i]], data.frame(x = 386525.1, y = 5819332))),
  "ea" = sapply(1:17544, FUN = function(i) raster::extract(krg_ea[[i]], data.frame(x = 386525.1, y = 5819332))),
  "p" = sapply(1:17544, FUN = function(i) raster::extract(krg_p[[i]],   data.frame(x = 386525.1, y = 5819332))),
  "ws" = sapply(1:17544, FUN = function(i) raster::extract(krg_ws[[i]], data.frame(x = 386525.1, y = 5819332))),
  "wd" = sapply(1:17544, FUN = function(i) raster::extract(krg_wd[[i]], data.frame(x = 386525.1, y = 5819332))),
  "Rin" = sapply(1:17544, FUN = function(i) raster::extract(krg_Rin[[i]], data.frame(x = 386525.1, y = 5819332))),
  "Rli" = solar_radiation_3987$Rli,
  "SMC60" = sapply(1:17544, FUN = function(i) raster::extract(krg_SMC60[[i]],  data.frame(x = 386525.1, y = 5819332))),
  "prec_mm" = sapply(1:17544, FUN = function(i) raster::extract(Prec_krg[[i]], data.frame(x = 386525.1, y = 5819332)))
)

summary(DWD_TUCC)
DWD_TUCC$Rin[DWD_TUCC$Rin < 0] <- 0
DWD_TUCC$RH[DWD_TUCC$RH > 100] <- 100
DWD_TUCC$Rli[DWD_TUCC$Rli < 200] <- 200

DWD_TUCC$wd[which(DWD_TUCC$wd < 0)] <- DWD_TUCC$wd[which(DWD_TUCC$wd < 0)] + 360
DWD_TUCC$wd[which(DWD_TUCC$wd > 360)] <- DWD_TUCC$wd[which(DWD_TUCC$wd > 360)] - 360

summary(DWD_TUCC)

saveRDS(DWD_TUCC, "DWD_TUCC")


