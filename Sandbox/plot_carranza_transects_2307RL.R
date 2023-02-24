library(tidyverse)
library(sf)
library(mapview)
library(rgdal)

jcf.transects = st_read(here::here("Data/Nav/Carranza_transects/Carranza_transects.shp"))

rl.transects = st_read(here::here("Output/shapefiles/transects.shp"))

mapview(jcf.transects) + mapview(rl.transects, zcol = "Type")

jcf.transects.gpx <- jcf.transects %>% 
  select(Transect)


jcf.points <- jcf.transects %>% 
  st_transform(crs = 4326) %>% 
  st_cast("POINT") %>% 
  mutate(
    long = as.data.frame(st_coordinates(.))$X,
    lat = as.data.frame(st_coordinates(.))$Y) %>% 
  select(transect = Transect, lat, long) %>% 
  st_set_geometry(NULL)

write_csv(jcf.points, here::here("Data/Nav/jcf_points.csv"))


