library(tidyverse)
library(sf)
library(here)
library(atm)
library(rnaturalearth)

crs.proj = 3310

# Get map data -------------------------------
# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego",
                "Ensenada","Punta Eugenia","El Rosario","Cabo San Lucas",
                "Punta Abreojos","San Carlos")

# Import landmarks
locations <- filter(read.csv(here("Data/Map/locations.csv"))) %>% 
  filter(name %in% label.list) %>% 
  project_df(to = crs.proj)

# Get land features --------------------------
# Get state data
states <- ne_states(country = 'United States of America', returnclass = 'sf')
ca     <- filter(states, name == "California")

# Get countries
countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

# Read bathy contours shapefile 
bathy <- st_read(here("Data/GIS/bathy_contours.shp")) %>% 
  st_transform(4326) %>% 
  rename(Depth = Contour)

# dataURL <- 'https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_Lon0360.csv0?
# potential_habitat%5B(2021-09-30T12:00:00Z)%5D%5B(31.7):(49.95)%5D%5B(230.0):(244.0)%5D&.draw=surface&.vars=longitude%7Clatitude%7Cpotential_habitat&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff'
# 
# hab <- read_csv(here::here("Data/Habitat/sardine_habitat_modis_Lon0360_b09c_60aa_966d.csv"), 
#                      col_names = c("datetime","lat","long","habitat_mask")) %>% 
#   filter(!is.nan(habitat_mask)) %>%  
#   mutate(long = long-360,
#          hab.cat = cut(habitat_mask, c(0, 0.01, 0.1, 0.2, 1), 
#                              include.lowest = TRUE, labels = FALSE)) %>% 
#   atm::project_df(to = 3310)

hab <- read_csv(here::here("Data/Habitat/sardine_habitat_mask_20210327.csv"), 
                col_names = c("datetime","lat","long","habitat_mask"),
                lazy = FALSE) %>% 
  filter(!is.nan(habitat_mask)) %>%  
  mutate(long = long-360) %>% 
  atm::project_df(to = 3310)

hab <- fs::path(here("Data/Habitat")) %>% 
  dir_ls(regexp = "sardine_habitat*.*csv") %>% 
  map_dfr(read_csv, col_names = c("datetime","lat","long","habitat_mask"),
         lazy = FALSE) %>% 
  filter(!is.nan(habitat_mask)) %>%  
  mutate(long = long-360,
         date = as.factor(lubridate::date(datetime))) %>% 
  atm::project_df(to = 3310)

map.bounds <- hab %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  st_bbox()

# hab.ras <- select(hab, X, Y, Z = habitat_mask) %>% 
#   raster::rasterFromXYZ()

# load(here::here("Data/Map/basemap.Rdata"))
# 
# Create base map
# base.map <- get_basemap(map.bounds, states, countries, locations, bathy, map.bounds, crs = 4326)

ggplot() + 
  facet_wrap(~date) +
  geom_raster(data = hab, aes(long, lat, fill = factor(habitat_mask))) +
  geom_sf(data = countries, fill = "black", color = "white") +
  geom_sf(data = states, fill = "black", colour = "white") +
  scale_fill_manual(name = "Potential habitat",
                    # breaks = c(1, 10, 20, 100),
                    values = c("blue","yellow","orange","red"), 
                    labels = c("Unsuitable","Bad","Good","Optimal"),
                    guide = guide_legend(reverse = TRUE)) +
  # Format axes and titles
  xlab("Longitude") + ylab("Latitude") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = 4326, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
  
  theme_bw() 

# ggplot() + 
#   geom_raster(data = hab, aes(long, lat, fill = factor(habitat_mask))) +
#   scale_fill_manual(values = c("blue","yellow","orange","red")) +
#   geom_sf(data = countries, fill = "gray90", color = "gray50") +
#   geom_sf(data = states, fill = "gray90", colour = "gray50") +
#   # Format axes and titles
#   xlab("Longitude") + ylab("Latitude") + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_sf(crs = 4326, # CA Albers Equal Area Projection
#            xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
#            ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
#   
#   theme_bw() 

base.map + 
  geom_raster(data = hab, aes(long, lat, fill = factor(habitat_mask))) +
  # scale_fill_manual(name = "Potential habitat", values = c("blue","yellow","orange","red")) +
  scale_fill_manual(name = "Potential habitat",
                    # breaks = c(1, 10, 20, 100),
                    values = c("blue","yellow","orange","red"), 
                    labels = c("Unsuitable","Bad","Good","Optimal"),
                    guide = guide_legend(reverse = TRUE)) +
  shadowtext::geom_shadowtext(data = locations, aes(long, lat, label = name),
                              angle = 30, hjust = 0) +
  # Format axes and titles
  xlab("Longitude") + ylab("Latitude") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = 4326, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    legend.position = c(0.95,0.5),
    legend.justification = c(1,0.5)
  )

ggsave(filename = here("Figs/fig_habitat_test.png"),
       height = 9)
