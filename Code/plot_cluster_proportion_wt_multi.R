library(tidyverse)
library(atm)
library(scatterpie)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)

# Load nav data -----------------------------
# 2017 data
load("E:/CODE/R_packages/EstimateCPS/1707RL/Data/Nav/nav_data.Rdata")
nav.paths.2017 <- nav.sf %>% 
  mutate(cruise = 2017)

# 2018 data
load("E:/CODE/R_packages/EstimateCPS/1807RL/Data/Nav/nav_data.Rdata")
nav.paths.2018 <- nav.sf %>% 
  mutate(cruise = 2018)

# 2019 data
load("E:/CODE/R_packages/estimATM/1907RL/Data/Nav/nav_data.Rdata")
nav.paths.2019 <- nav.paths.sf %>% 
  mutate(cruise = 2019)

# Combine nav.paths
nav.all <- rbind(nav.paths.2017, nav.paths.2018, nav.paths.2019)

# ggplot(nav.all) + geom_sf() + facet_wrap(~cruise)

# Load transects ----------------------------
transects.2017 <- st_read("E:/CODE/R_packages/EstimateCPS/1707RL/Output/planned_transects.shp") %>% 
  mutate(cruise = 2017)
transects.2018 <- st_read("E:/CODE/R_packages/EstimateCPS/1807RL/Output/planned_transects.shp") %>% 
  mutate(cruise = 2018)
transects.2019 <- st_read("E:/CODE/R_packages/estimATM/1907RL/Output/planned_transects.shp") %>% 
  mutate(cruise = 2019) %>% 
  select(type = Type, line = Transect, brg, cruise)

# Combine all transects
transects.all <- rbind(transects.2017, transects.2018, transects.2019)

# ggplot(transects.all) + geom_sf() + facet_wrap(~cruise)

# Get map data -------------------------------
# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")
# Import landmarks
locations <- filter(read.csv(here::here("Data/Map/locations.csv")), name %in% label.list) %>% 
  atm::project_df(to = 3310)

# Get land features --------------------------
# Get state data
states <- ne_states(country = 'United States of America', returnclass = 'sf')
ca     <- filter(states, name == "California")

# Get countries
countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

# Read bathy contours shapefile 
bathy <- st_read(here::here("Data/GIS/bathy_contours.shp")) %>% 
  st_transform(3310) %>% 
  rename(Depth = Contour)

# Use nav data to resize map to survey progress
map.bounds <- transects.all %>%
  st_transform(crs = 3310) %>%
  st_bbox()  

# Determine map aspect ratio and set height and width
map.height <- 10
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- map.height*map.aspect

base.map <- get_basemap(nav.all, states, countries, locations, bathy, map.bounds, crs = 3310) 

# Load proportion data ----------------------------
# Calculate pie radius based on latitude range
pie.scale  <- 0.0125 # 0.01-0.02 works well for coast-wide survey (i.e., summer), larger values (~0.03) for spring
pie.radius <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*pie.scale)

# Get acoustic proportions for mapping
prop.2017 <- read_csv("E:/CODE/R_packages/EstimateCPS/1707RL/Output/clf_ts_proportions.csv") %>% 
  select(cluster, lat, long, prop.anch, prop.jack, prop.her, prop.mack, prop.sar) %>% 
  replace(is.na(.), 0) %>% 
  mutate(cruise = 2017) %>% 
  atm::project_df(to = 3310)

prop.2018 <- read_csv("E:/CODE/R_packages/EstimateCPS/1807RL/Output/clf_ts_proportions.csv")%>% 
  select(cluster, lat, long, prop.anch, prop.jack, prop.her, prop.mack, prop.sar) %>%
  replace(is.na(.), 0) %>% 
  mutate(cruise = 2018) %>%
  atm::project_df(to = 3310)

prop.2019 <- read_csv("E:/CODE/R_packages/estimATM/1907RL/Output/clf_ts_proportions.csv")%>% 
  select(cluster, lat, long, prop.anch, prop.jack, prop.her, prop.mack, prop.sar) %>%
  replace(is.na(.), 0) %>% 
  mutate(cruise = 2019) %>% 
  atm::project_df(to = 3310)

# Combine data from all years
prop.all <- prop.2017 %>% bind_rows(prop.2018) %>% bind_rows(prop.2019) %>% 
  mutate(prop.tot = prop.anch + prop.jack + prop.her + prop.mack + prop.sar)

# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'

# Create trawl cluster map
combo.map <- base.map + 
  # Plot transects data
  geom_sf(data = transects.all, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  geom_sf(data = nav.all, colour = "gray50", size = 0.5, alpha = 0.5) +
  geom_scatterpie(data = filter(prop.all, prop.tot != 0), 
                  aes(X, Y, group = cluster, r = pie.radius),
                  cols = c("prop.anch","prop.jack","prop.her",
                           "prop.mack","prop.sar"),
                  color = 'black', alpha = 0.8) +
  # Plot empty cluster locations
  geom_point(data = filter(prop.all, prop.tot == 0), aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  # Configure pie scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "P. herring", "J. mackerel",
                               "P. mackerel", "Sardine"),
                    values = c(anchovy.color, pac.herring.color, jack.mack.color,
                               pac.mack.color, sardine.color)) +
  # Plot panel label
  facet_wrap(~cruise) +
  coord_sf(crs = 3310, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) + 
  theme(strip.background.x = element_blank(),
        strip.text.x       = element_text(face = "bold", size = 14))

ggsave(combo.map, filename = here::here("Figs/fig_cluster_prop_combo.png"),
       height = map.height, width = map.width*3)

