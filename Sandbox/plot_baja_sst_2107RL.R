library(tidyverse)
library(lubridate)
library(here)
library(cmocean)
library(sf)

load("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Nav/nav_data.Rdata")
load("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Backscatter/nasc_all.Rdata")

nav <- nav %>% 
  filter(between(lat, 26,33),
         between(long, -120,-112),
         between(SST, 10, 22))

nasc <- nasc %>% 
  filter(between(lat, 26,33),
         between(long, -120,-112)) %>% 
  filter(cps.nasc > 0)

# Get Carranza nav data from the meteorological station
nav.jcf <- sf::st_read(here("Data/Nav/JCF/JCFINP2110meteorologica00_27.gpkg")) %>% 
  mutate(
    time = ymd_hms(paste(date, time)),
    leg = "Leg 5") %>% 
  select(time, lat = latitude, long = longitude,
         SST = temperature, SOG = sog, 
         wind_dir = wind_direction10min,
         wind_speed = wind_speed10min,
         leg) %>% 
  st_set_geometry(NULL)

nav.jcf <- nav.jcf %>% 
  filter(between(lat, 26,33),
         between(long, -120,-112),
         between(SST, 10, 22))

ggplot() + 
  geom_point(data = nasc, aes(long, lat+0.05, size = cps.nasc)) +
  geom_path(data = nav, aes(long, lat, colour = SST), size = 1) +
  geom_path(data = nav.jcf, aes(long, lat, colour = SST), size = 1) +
  scale_colour_cmocean(name = "thermal") +
  coord_map()

tx <- st_read("C:/KLS/CODE/R_packages/estimATM/2207RL/Output/shapefiles/transects.shp") %>% 
  st_set_geometry(NULL)

write_csv(tx, here("Output/shapefiles/transect_summary.csv"))
