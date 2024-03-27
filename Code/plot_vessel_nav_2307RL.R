pacman::p_load(tidyverse, lubridate, here, sf, mapview, ggnewscale)

# Load Lasker nav data
load(here("Data/Nav/nav_data.Rdata"))
# Load Shimada nav data
load(here("Data/Nav/nav_data_sh.Rdata"))
# Load Saildrone nav data
load(here("Data/Nav/nav_data_saildrone.Rdata"))
# Load nearshore nav data     
load(here("Data/Nav/nav_data_nearshore.Rdata"))

# Load basemap
load("C:/KLS/CODE/Github/estimATM/2307RL/Data/Map/basemap.Rdata")

# Set map boundaries
crs.proj = 3310
crs.geog = 4326

map.bound.data <- nav.paths.sf %>% 
  bind_rows(nav.paths.sh.sf) %>%
  bind_rows(nav.sd.paths.sf) %>%
  bind_rows(nav.ns.paths.sf) 

waypoints_2307RL <- read_csv("Data/Nav/waypoints_2307RL.csv") %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog)

map.bounds <- waypoints_2307RL %>%
  st_transform(crs = crs.proj) %>%
  st_bbox()  

# mapview(nav.paths.sf)
# mapview(nav.paths.sh.sf)
# mapview(nav.sd.paths.sf, zcol = "saildrone")
# mapview(nav.ns.paths.sf)

# Assemble nav on map
base.map + 
  geom_sf(data = nav.paths.sf,    colour = "purple") +
  geom_sf(data = nav.paths.sh.sf, colour = "blue") +
  geom_sf(data = nav.sd.paths.sf, colour = "red") +
  geom_sf(data = nav.ns.paths.sf, aes(colour = vessel.name)) +
  scale_colour_manual(name = "Vessel", values = c("LBC" = "orange", "LM" = "green")) +
  theme(legend.position = 'none') +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

# Save map
ggsave(here("Figs/fig_vessel_nav_all.png"),
       height = 9, width = 5)

# nav.sd %>% 
#   mutate(date = date(datetime)) %>% 
#   group_by(saildrone) %>% 
#   summarise(n_days = n_distinct(date))

