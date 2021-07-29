library(tidyverse)
library(sf)

nav <- read_csv("Data/Nav/nav_CDFW.csv")

nav.sf <- st_as_sf(nav, coords = c("long","lat"), crs = 4326)

st_write(nav.sf, "Data/Nav/nav_CDFW_points.shp")


# Convert to lines, by date
nav.lines <- nav.sf %>% 
  group_by(date) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") 

st_write(nav.lines, "Data/Nav/nav_CDFW_lines.shp")
