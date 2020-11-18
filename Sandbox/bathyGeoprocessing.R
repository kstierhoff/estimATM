library(sf)
library(tidyverse)

bathy.5m <- st_read(here::here("Data/GIS/bathy_5m_wc.shp")) %>% 
  st_cast("POLYGON")

ggplot(bathy.5m) + geom_sf()

st_write(bathy.5m, here::here("Data/GIS/isobath_5m_na.shp"),
         append = FALSE)

bathy.5m.buffer <- bathy.5m %>% 
  st_buffer(dist = 35/60) # Buffer by 35 nmi


ggplot(bathy.5m) + 
  geom_sf() +
  geom_sf(data = bathy.5m.buffer, fill = NA)
  
st_write(bathy.5m.buffer, here::here("Data/GIS/buffer_35_nmi.shp"),
         append = FALSE)
