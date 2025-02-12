pacman::p_load(tidyverse, lubridate, here, sf, mapview, ggnewscale)

# Load Lasker nav data
load(here("Data/Nav/nav_data.Rdata"))
# Load nearshore nav data     
load(here("Data/Nav/nav_data_nearshore.Rdata"))

# Remake nearshore transects without transits
nav.ns.paths.sf <- nav.ns.sf %>% 
  group_by(vessel.name, transect) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") 

# mapview(nav.paths.sf)
# mapview(nav.ns.paths.sf)

# Load basemap
load("C:/KLS/CODE/Github/estimATM/2407RL/Data/Map/basemap.Rdata")

# Set map boundaries
crs.proj = 3310
crs.geog = 4326

map.bound.data <- nav.paths.sf %>% 
  bind_rows(nav.ns.paths.sf) 

waypoints_2407RL <- read_csv("Data/Nav/waypoints_2407RL.csv") %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog)

map.bounds <- waypoints_2407RL %>%
  st_transform(crs = crs.proj) %>%
  st_bbox()  

# Assemble nav on maps
combo.map <- base.map + 
  geom_sf(data = nav.paths.sf, colour = "purple") +
  geom_sf(data = nav.ns.paths.sf, aes(colour = vessel.name)) +
  scale_colour_manual(name = "Vessel", values = c("LBC" = "blue", "LM" = "green")) +
  theme(legend.position = 'none') +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

core.map <- base.map + 
  geom_sf(data = nav.paths.sf, colour = "purple") +
  theme(legend.position = 'none') +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

nearshore.map <- base.map + 
  geom_sf(data = nav.ns.paths.sf, aes(colour = vessel.name)) +
  scale_colour_manual(name = "Vessel", values = c("LBC" = "blue", "LM" = "green")) +
  theme(legend.position = 'none') +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

# Save maps
# Core and nearshore
ggsave(combo.map,
       filename = here("Figs/fig_vessel_nav_all.png"),
       height = 9, width = 5)

# Core only
ggsave(core.map,
       filename = here("Figs/fig_vessel_nav_core.png"),
       height = 9, width = 5)

# Nearshore only
ggsave(nearshore.map,
       filename = here("Figs/fig_vessel_nav_nearshore.png"),
       height = 9, width = 5)
