pacman::p_load(tidyverse, sf, atm, lubridate, here, fs, scatterpie, rnaturalearth, shadowtext, atm)
load(here("Data/Trawl/trawl_data.Rdata"))

haul.ts <- haul.all %>% 
  mutate(
    equilibriumTime = ymd_hms(equilibriumTime),
    haulBackTime    = ymd_hms(haulBackTime),
    season = case_when(
    month(equilibriumTime) < 6 ~ "Spring",
    TRUE ~ "Summer")) %>% 
  mutate(year = year(equilibriumTime),
         lat = startLatDecimal,
         long = startLongDecimal,
         facet.group = paste(season, year)) %>% 
  filter(season == "Summer",
         year >= 2008,
         year != 2010) %>% 
  project_df(to = 3310)

# Compute totalWeight and totalNum, which only exist in the Access database
catch.ts <- catch.all %>% 
  mutate(
    totalWeight = subSampleWtkg + remainingSubSampleWtkg,
    totalNum    = (subSampleCount/subSampleWtkg)*totalWeight) %>% 
  left_join(select(haul.ts, cruise, ship, haul, season, year, lat, long, X, Y, facet.group)) %>% 
  left_join(select(spp.codes, species, scientificName)) %>% 
  filter(totalWeight > 0,
         scientificName == "Sardinops sagax",
         !is.na(year))

# ggplot(haul.ts, aes(long, lat, colour = ship)) + geom_point() + coord_map() + facet_wrap(~year, nrow = 2)
# ggplot(haul.ts, aes(long, lat)) + 
#   geom_point() + 
#   geom_point(data = catch.all, aes(long, lat, size = totalWeight), colour = "red") +
#   coord_map() + facet_wrap(~year, nrow = 2)

# haul.ts <- haul.all %>% 
#   filter(season == "Summer",
#          !is.na(year), 
#          year > 2007) %>% 
#   mutate(key = paste(season, year, ship),
#          lat = startLatDecimal,
#          long = startLongDecimal)
# 
# catch.ts <- catch.all %>% 
#   mutate(key = paste(season, year, ship)) %>% 
#   left_join(select(haul.ts, key, lat, long)) %>% 
# 
#   filter(key %in% haul.ts$key,
#          scientificName == "Sardinops sagax") %>% 
#   mutate(facet.group = paste(season, year))
# 
# ggplot(haul.ts, aes(long, lat)) + geom_point() + facet_wrap(~key)
# 
# ggplot() + 
#   geom_point(data = haul.ts, aes(long, lat), colour = "red") +
#   geom_point(data = filter(catch.ts, totalWeight >= 0), aes(long, lat, size = totalWeight)) +
#   facet_wrap(~facet.group, nrow = 2) + 
#   coord_map()

# Load catch data
# 2017 
load("C:/KLS/CODE/R_packages/EstimateCPS/1707RL/Output/catch_final.Rdata")
load("C:/KLS/CODE/R_packages/EstimateCPS/1707RL/Output/haul_info.Rdata")

source(here("Code/process_trawl_catch_1707RL.R"))

haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2017")
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2017")
catch.all       <- catch %>% 
  left_join(select(haul.mid, haul, long, lat)) %>% 
  mutate(survey = "Summer 2017")

load("C:/KLS/CODE/R_packages/EstimateCPS/1707RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% mutate(survey = "Summer 2017")

# 2018
load("C:/KLS/CODE/R_packages/estimATM/1807RL/Output/catch_info.Rdata")
load("C:/KLS/CODE/R_packages/estimATM/1807RL/Output/catch_final.Rdata")

haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2018") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2018") %>% 
  bind_rows(cluster.pie.all)
catch.all <- catch %>% mutate(survey = "Summer 2018") %>% 
  left_join(select(haul.pie, haul, long, lat)) %>%
  bind_rows(catch.all)

load("C:/KLS/CODE/R_packages/estimATM/1807RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% mutate(survey = "Summer 2018") %>% 
  bind_rows(nav.all)

# 2019
load("C:/KLS/CODE/R_packages/estimATM/1907RL/Output/catch_info.Rdata")
load("C:/KLS/CODE/R_packages/estimATM/1907RL/Output/catch_final.Rdata")

haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2019") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2019") %>% 
  bind_rows(cluster.pie.all)
catch.all <- catch %>% mutate(survey = "Summer 2019") %>% 
  left_join(select(haul.pie, haul, long, lat)) %>%
  bind_rows(catch.all)

load("C:/KLS/CODE/R_packages/estimATM/1907RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% mutate(survey = "Summer 2019") %>% 
  bind_rows(nav.all)

# 2021
load("C:/KLS/CODE/R_packages/estimATM/2107RL/Output/catch_info.Rdata")
load("C:/KLS/CODE/R_packages/estimATM/2107RL/Output/catch_final.Rdata")
haul.pie.all   <- haul.pie %>% mutate(survey = "Summer 2021") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2021") %>% 
  bind_rows(cluster.pie.all)
catch.all <- catch %>% mutate(survey = "Summer 2021") %>% 
  left_join(select(haul.pie, haul, long, lat)) %>%
  bind_rows(catch.all)

load("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% 
  select(-time) %>% mutate(survey = "Summer 2021") %>% 
  bind_rows(nav.all)

# 2015
load("C:/KLS/CODE/R_packages/estimATM/1507SH/Output/catch_info.Rdata")
load("C:/KLS/CODE/R_packages/estimATM/1507SH/Output/catch_final.Rdata")
haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2015") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2015") %>% 
  bind_rows(cluster.pie.all)
catch.all <- catch %>% mutate(survey = "Summer 2015") %>% 
  left_join(select(haul.pie, haul, long, lat)) %>%
  bind_rows(catch.all)

load("C:/KLS/CODE/R_packages/estimATM/1507SH/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% 
  select(-time) %>% mutate(survey = "Summer 2015") %>% 
  bind_rows(nav.all)

# 2016
load("C:/KLS/CODE/R_packages/estimATM/1606RL/Output/catch_info.Rdata")
load("C:/KLS/CODE/R_packages/estimATM/1606RL/Output/catch_final.Rdata")
haul.pie.all    <- haul.pie %>% mutate(survey = "Summer 2016") %>% 
  bind_rows(haul.pie.all)
cluster.pie.all <- cluster.pie %>% mutate(survey = "Summer 2016") %>% 
  bind_rows(cluster.pie.all)
catch.all <- catch %>% mutate(survey = "Summer 2016") %>% 
  left_join(select(haul.pie, haul, long, lat)) %>%
  bind_rows(catch.all)

load("C:/KLS/CODE/R_packages/estimATM/1606RL/Data/Nav/nav_data.Rdata")
nav.all <- nav %>% 
  select(-time) %>% mutate(survey = "Summer 2016") %>% 
  bind_rows(nav.all)

# ggplot(filter(catch.all, scientificName == "Sardinops sagax"),
#        aes(long, lat, size = totalWeight)) +
#   geom_point() + facet_wrap(~survey)

# Replace missing round herring proportions with zeros
haul.pie.all <- haul.pie.all %>% 
  replace_na(list(RndHerring = 0)) 

cluster.pie.all <- cluster.pie.all %>% 
  replace_na(list(RndHerring = 0)) 

# Combine all nav data and make sf
nav.sf <- nav.all %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(survey) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") 

# Read bathy contours shapefile 
bathy <- st_read(here("C:/KLS/CODE/R_packages/estimATM/1907RL/Data/GIS/bathy_contours.shp")) %>% 
  st_transform(4326) %>% 
  rename(Depth = Contour)

# Get land features
# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

# Import landmarks
locations <- filter(read.csv(here("C:/KLS/CODE/R_packages/estimATM/1907RL/Data/Map/locations.csv")), 
                    name %in% label.list) %>% 
  project_df(to = 3310)

# Get land features --------------------------
# Get state data
states <- ne_states(country = 'United States of America', returnclass = 'sf')
ca     <- filter(states, name == "California")

# Get countries
countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

hauls.sf <- haul.pie.all %>%
  st_as_sf(coords = c("long","lat"), crs = 4326)

map.bounds <- hauls.sf %>% 
  st_transform(crs = 3310) %>%
  st_bbox()

# Calculate pie radius based on latitude range
pie.radius <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*0.0125)

# Filter for positive hauls and clusters
haul.pos <- filter(haul.pie.all, AllCPS > 0) %>% 
  arrange(X) %>% 
  mutate(group = paste(survey, haul), 
         r = pie.radius) %>% 
  select(-radius)

cluster.pos <- filter(cluster.pie.all, AllCPS > 0) %>% 
  arrange(X)%>% 
  mutate(group = paste(survey, cluster),
         r = pie.radius) %>% 
  select(-radius)

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(haul.pos) > 0) {
  haul.pos <- haul.pos %>% 
    replace(. == 0, 0.0000001) 
  
  cluster.pos <- cluster.pos %>% 
    replace(. == 0, 0.0000001) 
}

# Filter for empty trawls
haul.zero    <- filter(haul.pie.all, AllCPS == 0)

cluster.zero <- filter(cluster.pie.all, AllCPS == 0)

# Create base map -------------------------------------------
base.map <- atm::get_basemap(hauls.sf, states, countries, locations, bathy, 
                             map.bounds, add.labels = FALSE, crs = 3310)

# Figure preferences ------------------------------------------------------
# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'
rnd.herring.color  <- '#F0B81D'

# Create trawl cluster figure
base.map +
  # plot ship track data
  geom_sf(data = nav.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot trawl pies
  geom_scatterpie(data = cluster.pos,
                  aes(X, Y, group = group,
                      r = r),
                  cols = c("Anchovy", "JackMack", "Jacksmelt",
                           "PacHerring", "PacMack", "RndHerring", "Sardine"),
                  color = 'black', alpha = 0.8, sorted_by_radius = TRUE) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "R. herring", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, rnd.herring.color, 
                               sardine.color)) +
  # Plot empty cluster locations
  geom_point(data = cluster.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  facet_wrap(~survey, nrow = 1) +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold",
                                    size = 20)) +
  coord_sf(crs = 3310, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

ggsave(here("Figs/fig_cluster_weight_time_series.png"),
       height = 8, width = 20)

# Create trawl cluster figure, two rows
base.map +
  # plot ship track data
  geom_sf(data = nav.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot trawl pies
  geom_scatterpie(data = cluster.pos,
                  aes(X, Y, group = group,
                      r = r),
                  cols = c("Anchovy", "JackMack", "Jacksmelt",
                           "PacHerring", "PacMack", "RndHerring", "Sardine"),
                  color = 'black', alpha = 0.8, sorted_by_radius = TRUE) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "R. herring", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, rnd.herring.color, 
                               sardine.color)) +
  # Plot empty cluster locations
  geom_point(data = cluster.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  facet_wrap(~survey, nrow = 2) +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold",
                                    size = 16)) +
  coord_sf(crs = 3310, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

ggsave(here("Figs/fig_cluster_weight_time_series_long.png"),
       height = 11, width = 10)


# Filter sardine catch
# catch.sar <- filter(catch.all, scientificName == "Sardinops sagax") %>% 
#   project_df(to = 3310)

# Create sardine distribution figure
base.map +
  # plot ship track data
  # geom_sf(data = nav.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot empty cluster locations
  geom_point(data = haul.ts, aes(X, Y), size = 1) +
  geom_point(data = catch.ts,
                  aes(X, Y, size = totalWeight),
             colour = sardine.color) +
  facet_wrap(~facet.group, nrow = 2) +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold")) +
  coord_sf(crs = 3310, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# base.map +
#   # plot ship track data
#   geom_sf(data = nav.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
#   geom_point(data = catch.sar,
#              aes(X, Y, size = totalWeight),
#              colour = sardine.color) +
#   # Plot empty cluster locations
#   geom_point(data = haul.zero, aes(X, Y),
#              size = 2, shape = 21, fill = 'black', colour = 'white') +
#   facet_wrap(~survey, nrow = 1) +
#   theme(strip.background.x = element_blank(),
#         strip.text.x = element_text(face = "bold")) +
#   coord_sf(crs = 3310, 
#            xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
#            ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

ggsave(here("Figs/fig_sardine_catch_time_series.png"),
       height = 8, width = 10)

