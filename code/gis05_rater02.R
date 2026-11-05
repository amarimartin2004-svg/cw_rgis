#raster 2 
if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
                terra,
                tidyterra,
                mapview,
                stars)
#erase existing objects
rm(list=ls())

#precipitation example

(spr_prec <- rast("data/spr_prec_us.tif"))
ext(spr_prec)

ggplot() +
  geom_spatraster(data = spr_prec)

spr_prec_crop <- crop(x = spr_prec,
                      y = c(-80, -75, 34, 37))
## a map of cropped data
ggplot() +
  geom_spatraster(data = spr_prec_crop)

## get county polygon data

sf_nc_county <- readRDS("data/sf_nc_county.rds")


ggplot() +
  geom_spatraster(data = spr_prec_crop) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

##use polygon layer to crop raster
spr_prec_nc <- crop(x = spr_prec,
                   y = sf_nc_county)

##map spr_prec_nc with sf_nc_county
ggplot()+
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

# merge 
spr_nw <- rast("data/spr_prec_ncnw.tif")
spr_ne <- rast("data/spr_prec_ncne.tif")
spr_sw <- rast("data/spr_prec_ncsw.tif")
spr_se <- rast("data/spr_prec_ncse.tif")

#north west portion of NC
ggplot() +
  geom_spatraster(data = spr_nw) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

#merge () function to combine two panels then map
spr_n <- merge(spr_nw, spr_ne)

## merge more than two panels into one
list_spr <- list(spr_ne,
                 spr_nw,
                 spr_se,
                 spr_sw)
##convert from list to spatraster collection
spr_col <- sprc(list_spr)

## merge more than two panels
spr_merge <- merge(spr_col)

writeRaster(spr_merge,
            "data/spr_prec_nc.tif", 
            overwrite = TRUE)

#stack

spr_prec_nc <- rast("data/spr_prec_nc.tif")
spr_tmp_nc <- rast("data/spr_tmp_nc.tif")

ggplot() +
  geom_spatraster(data = spr_tmp_nc) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
## create a multi-layer raster (stack)
spr_pt_nc <- c(spr_prec_nc,
               spr_tmp_nc)
##acess one layer
## -precipitation 
spr_pt_nc$precipitation

##- temperature
spr_pt_nc$temperature

##reprojection

spr_prec_nc_proj <- project(x = spr_prec_nc,
                            y = "EPSG:32617")


# Exercise 4.35 -----------------------------------------------------------

library(terra)
library(ggplot2)
library(tidyterra)

list.files(pattern = "spr_tmp", recursive = TRUE, full.names = TRUE)


spr_nw <- rast("data/spr_tmp_ncnw.tif")
spr_ne <- rast("data/spr_tmp_ncne.tif")
spr_sw <- rast("data/spr_tmp_ncsw.tif")
spr_se <- rast("data/spr_tmp_ncse.tif")


raster_list <- list(spr_nw, spr_ne, spr_sw, spr_se)
raster_collection <- terra::sprc(raster_list)
spr_merge <- merge(raster_collection)


ggplot() +
  geom_spatraster(data = spr_merge) +
  geom_sf(data = sf_nc_county, alpha = 0.25)

names(sf_nc_county)
head(sf_nc_county)

sf_camden <- sf_nc_county[sf_nc_county$county == "camden", ]
terra::ext(sf_camden)

spr_tmp_camden <- terra::crop(spr_merge, sf_camden)

spr_tmp_camden <- terra::crop(spr_merge, sf_camden)


ggplot() +
  geom_spatraster(data = spr_tmp_camden) +
  geom_sf(data = sf_nc_county, alpha = 0.25)

spr_tmp_camden_proj <- terra::project(
  spr_tmp_camden,
  y = "EPSG:32618",
  method = "bilinear"
)

spr_tmp_camden_proj


