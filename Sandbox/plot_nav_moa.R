library(tidyverse)
nav.scs <- read_csv("C:/SURVEY/2107RL/MOA Continuous_001.elg") %>% 
  mutate(lat = surveyR::scs2dd(`GP170-Lat`),
         long = surveyR::scs2dd(`GP170-Lon`),
         datetime = lubridate::mdy_hms(paste(Date, Time)))

nav.scs.sf <- sf::st_as_sf(nav.scs, coords = c("long","lat"),crs = 4326) %>% 
  summarise(do_union = FALSE) %>% 
  sf::st_cast("LINESTRING")

nav.samos <- read_csv("C:/SURVEY/2107RL/SAMOS-OBS_001.elg") %>% 
  mutate(datetime = lubridate::mdy_hms(paste(Date, Time))) %>% 
  sf::st_as_sf(coords = c("SAMOS-Lon-VALUE","SAMOS-Lat-VALUE"),crs = 4326) %>% 
  summarise(do_union = FALSE) %>% 
  sf::st_cast("LINESTRING")

hauls <- readRDS(here::here("Output/haul_paths.rds"))

ggplot(nav.scs, aes(long, lat)) +
  geom_path() + 
  coord_map()

mapview::mapview(nav.scs.sf) + 
  mapview::mapview(hauls, zcol = "haul")

mapview::mapview(nav.samos) + 
  mapview::mapview(hauls, zcol = "haul")
