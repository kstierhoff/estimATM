# library(tidyverse)

# Read file from Juan
route.updated <- read_csv(here("Output/routes_updated/CCE2004.csv")) %>% 
  mutate(transect.name = str_sub(id, 1, 3))

for (i in unique(route.updated$transect.name)) {
  # Write core transects
  core.tmp <- route.updated %>% 
    filter(transect.name == i, core == 1)
  
  if (nrow(core.tmp) > 0) {
    core.tmp %>% 
      select(id, lat, long) %>% 
      write_csv(here("Output/routes_updated", paste0(i, "_Core.csv")))
  }
  
  # Write extended transects
  ext.tmp <- route.updated %>% 
    filter(transect.name == i, core != 1)
  
  if (nrow(ext.tmp) > 0) {
    ext.tmp %>% 
      select(id, lat, long) %>% 
      write_csv(here("Output/routes_updated", paste0(i, "_Extended.csv")))
  }
}

# Create land object for plotting
usa <- map_data("usa") 

# Convert to sf
wpts.sf <- route.updated %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) 

# Get map boundaries for plotting
map.bounds <- wpts.sf %>%
  sf::st_bbox()  

# Plot waypoints
ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) + 
  geom_path(data = route.updated, aes(long, lat, group = transect)) + 
  geom_point(data = filter(route.updated, !str_detect(id, "Pairovet"), core == 1), 
             aes(long, lat)) +
  geom_point(data = filter(route.updated, str_detect(id, "Pairovet"), core == 1), 
             aes(long, lat, fill = type), shape = 21, size = 2.5) +
  
  geom_point(data = filter(route.updated, str_detect(id, "Pairovet"), core != 1), 
             aes(long, lat, colour = type), fill = NA, shape = 24, size = 2) +
  scale_fill_manual(name = "Type", values = list("A" = "blue", "C" = "red")) +
  scale_colour_manual(name = "Type", values = list("A" = "blue", "C" = "red")) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = 4326, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) + 
  # theme(legend.position = c(0,0),
  #       legend.justification = c(0,0)) + 
  theme_bw()

ggsave(filename = here("Figs/fig_survey_plan_map_final.png"),
       height = 10, width = 10)
