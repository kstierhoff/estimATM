# Use to summarize and export nearshore transect data to shapefiles for CDFW

library(tidyverse)
library(sf)
library(lubridate)
library(mapview)

fs::dir_create(here::here("Output/CDFW"))

load(here::here("Data/Backscatter/nasc_all.Rdata"))
# load(here::here("C:/KLS/CODE/R_packages/estimATM/2103RL/Data/Backscatter/nasc_nearshore.Rdata"))
# load(here::here("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Backscatter/nasc_nearshore.Rdata"))

nasc.sf <- st_as_sf(nasc, coords = c("long","lat"), crs = 4326) %>% 
  mutate(date_group = as.factor((date(datetime)))) %>% 
  filter(vessel.name == "RL")

nasc.summ <- nasc.sf %>% 
  group_by(transect.name) %>% 
  summarise(date = as.factor(min(date(datetime)))) %>% 
  st_set_geometry(NULL)

# Convert to lines, by date
core.lines <- nasc.sf %>% 
  group_by(vessel.name, transect.name) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  mutate(distance_nmi = as.numeric(st_length(.)*0.000539957)) %>% 
  left_join(nasc.summ) %>% 
  rename(vessel = vessel.name,
         transect = transect.name,
         dist_nmi = distance_nmi)

core.lines.date <- nasc.sf %>% 
  group_by(date_group) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  mutate(dist_nmi = as.numeric(st_length(.)*0.000539957))

st_write(core.lines, "Output/CDFW/nav_core_CDFW_lines.shp",
         delete_layer = TRUE)

st_write(core.lines.date, "Output/CDFW/nav_core_CDFW_lines_date.shp",
         delete_layer = TRUE)

# mapview(nearshore.lines)
