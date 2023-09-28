library(tidyverse)
library(plotKML)
library(sf)
library(mapview)
library(lubridate)

# Read gpx file
tracks <- readGPX(here::here("Data/Nav/lm_tracks_20230906.gpx"))
ctds <- read_csv(here::here("Data/CTD/LM/lm_ctd_casts.csv")) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)
# events <- readGPX(here::here("Data/Nav/lm_events_20230906.gpx"))

transects <- tracks$tracks %>% 
  map_dfr(bind_rows) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  mutate(datetime = ymd_hms(time))
  

events <- route$waypoints 

write_csv(events, here::here("Output/lm_events_20230906.csv"))
write_csv(nav, here::here("Output/lm_tracks_20230906.csv"))

lm.df <- do.call(rbind, lm.nav)

write_csv(wpts3, here::here("route_points_final.csv"))
