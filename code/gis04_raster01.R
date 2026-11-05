#Raster 1 Basic Terra operations

if(!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars)
#raster data format

#rast() is a function to read data from your directory
spr_ex <- rast("data/spr_example.tif")

#writeRaster() is a function to export raster data
writeRaster(x = spr_ex,
            filename = "data/spr_elev.tif",
            overwrite= TRUE)

#visualize raster data:geom_spatraster() (from tidyterre")
ggplot()+
  geom_spatraster(
    data = spr_ex
  )
#if you want map view () function to work
star_ex <- st_as_stars(spr_ex)
mapview(star_ex) 

#data type in raster 

#continious 
v_elev<-values(spr_ex)

#extract elevation value at a given lo
extract(spr_ex, y = cbind(6.0000, 50.0000))

#pick three spots from the map

#try to get the highest
extract(spr_ex, y = cbind(5.9, 49.85))

#lowest
extract(spr_ex, y = cbind(6.1, 49.7))

#random 
extract(spr_ex, y = cbind(6.4, 49.8))

#extract data at multiple points
df_point <- tibble(
  lon = c(6, 5.9),
  lat = c(50, 49.96)
)
extract(spr_ex, y = df_point)

#discrete data
spr_for <- rast("data/spr_forest_nc.tif")

ggplot() +
  geom_spatraster(data = spr_for)

unique(spr_for)

v_binary <- values(spr_for)
mean(v_binary)*100

#multiple classes - code values with multiple categories 
spr_land <- rast("data/spr_land_reclass.tif")

# 1001 = forest
# 1010 = crop
# 1100 + urban
unique(spr_land)

#coordinate, lon -79.8063 lat 36.0701
extract(spr_land, cbind(-79.8063, 36.0701))

#reclass
# - matrix for category mapping
cm <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 1, 0, 0)
)

spr_bin <- classify(spr_land,
                    rcl = cm)

unique(spr_bin)
v_bin<- values(spr_bin)
mean(v_bin) * 100

#calculate % cropland
cm_crop <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 0, 1, 0)
)

spr_crop <- classify(spr_land,
                      rcl = cm_crop)
unique(spr_crop)
v_crop <- values(spr_crop)
mean(v_crop) * 100

#calculate % urban
cm_urban <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 0, 0, 1)
)

spr_urban <- classify (spr_land,
                       rcl = cm_urban)

unique (spr_urban)
v_urban <- values(spr_urban)
mean(v_urban) * 100



#excersize 

spr_prec_ncne <-rast("data/spr_prec_ncne.tif")

spr_prec_ncne

#2
#raster has 162 rows, 532 coloums and 1 layer
#Each cell is 0.008333333 by 0.008333333 degrees
#longitude ranges from -79.89181 to -75.45847
#Latitude ranges from 35.24153 to 36.5913
#Coordinate refrence system is WGS84 (EPSG:4326), using longitude and latitude
#min max precipitation= 35.24153, 36.59153
#3 visualize the raster 
ggplot() +
  geom_spatraster(data = spr_prec_ncne) 
#4 extract values
sf_site<- readRDS("data/sf_finsync_nc.rds")

df_xy <- st_coordinates(sf_site)

df_land<- extract(spr_land, df_xy)
#forest
df_land %>% 
  filter(code == 1001) %>% 
  nrow()
#cropland
df_land %>% 
  filter(code == 1010) %>% 
  nrow()
#urban 
df_land %>% 
  filter(code == 1100 ) %>% 
  nrow()
#most common sampling site is forest

#quicker approach 
table(df_land)

