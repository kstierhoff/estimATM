# Use to summarize and export nearshore transect data to shapefiles for CDFW

library(tidyverse)
library(sf)
library(lubridate)
library(mapview)

fs::dir_create(here::here("Output/CDFW"))

load(here::here("Data/Backscatter/nasc_nearshore.Rdata"))
# load(here::here("C:/KLS/CODE/R_packages/estimATM/2103RL/Data/Backscatter/nasc_nearshore.Rdata"))
# load(here::here("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Backscatter/nasc_nearshore.Rdata"))

nasc.sf <- st_as_sf(nasc.nearshore, coords = c("long","lat"), crs = 4326) %>% 
  mutate(date_group = date(datetime)) %>% 
  filter(vessel.name == "LBC")

# Convert to lines, by date
nearshore.lines <- nasc.sf %>% 
  group_by(vessel.name, transect.name) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))

nearshore.lines.date <- nasc.sf %>% 
  group_by(date_group) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))

st_write(nearshore.lines, "Output/CDFW/nav_nearshore_CDFW_lines.shp",
         delete_layer = TRUE)

st_write(nearshore.lines.date, "Output/CDFW/nav_nearshore_CDFW_lines_date.shp",
         delete_layer = TRUE)

# mapview(nearshore.lines)
