# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,lubridate,here,scatterpie,ggspatial,sf,rnaturalearth)

# Install and load required packages from Github -------------------------------
# atm
pacman::p_load_gh("kstierhoff/atm")
# rnaturalearth data
pacman::p_load_gh("ropenscilabs/rnaturalearthdata")
pacman::p_load_gh("ropenscilabs/rnaturalearthhires")

# Load log book data
logs <- read_csv(here("Data/Seine/lbc_logs.csv")) %>% 
  mutate(datetime = with_tz(mdy_hms(paste(date, as.character(time)), tz = "America/Los_Angeles"), tzone = "UTC")) %>% 
  arrange(datetime) %>% 
  mutate(id = seq_along(date)) %>% 
  select(id, datetime, everything())

# Load backscatter data
load(here("Data/Backscatter/nasc_nearshore.Rdata"))

# Filter and format
nav <- as_tibble(nasc.nearshore) %>% 
  filter(vessel.name == "LBC") %>% 
  mutate(datetime = ymd_hms(datetime)) %>% 
  select(datetime, long, lat, transect) %>% 
  arrange(datetime) %>% 
  project_df(to = 3310)

# Plot nav
ggplot(nav, aes(long, lat, group = transect)) + 
  geom_path() + 
  coord_map()

# Match events with nav
match.nav <- data.frame()

for (i in unique(logs$id)) {
  lag.df <- abs(as.numeric(difftime(logs$datetime[logs$id == i], nasc.nearshore$datetime, units = "mins")))
  
  match.nav <- bind_rows(match.nav,
                         data.frame(
                           id   = i,
                           lat  = nasc.nearshore$lat[which.min(lag.df)],
                           long = nasc.nearshore$long[which.min(lag.df)],
                           lag  = min(lag.df)))
}

# Join events to nav data
logs <- left_join(logs, match.nav) %>% 
  project_df(to = 3310)

# Create base map
nav.sf <- st_as_sf(nav, coords = c("long", "lat"), crs = 4326)

nav.path.sf <- nav.sf %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING")

transects.sf <- nav.sf %>% 
  group_by(transect) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING")

# Set padding around data  
map.bounds <- nav.sf %>% 
  st_transform(crs = 3310) %>%
  st_bbox() 

# Read bathy contours shapefile 
bathy <- st_read(here("Data/GIS/bathy_contours.shp")) %>% 
  st_transform(4326) 

# Get land features --------------------------
# Get state data
states <- ne_states(country = 'United States of America', returnclass = 'sf')
ca     <- filter(states, name == "California")

# Get countries
countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

# Import landmarks
# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

locations <- filter(read.csv(here("Data/Map/locations.csv")), name %in% label.list) %>% 
  project_df(to = 3310)

# Create base map
base.map <- get_basemap(nav.sf, states, countries, locations, bathy, map.bounds, crs = 3310) +
  # Add scalebar
  annotation_scale(style = "ticks", location = "br", height = unit(0.15, "cm"))

# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'

# Calculate pie radius based on latitude range
pie.scale  <- 0.0125 
pie.radius.ns <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*pie.scale)

# Load seine pie data
load(here("Output/purse_set_pies.Rdata"))

# Load Lasker pie data
load(here("Output/catch_info.Rdata"))

# Calculate pie radius of each pie, based on All CPS landings
set.pos <- set.pos %>% 
  mutate(radius = pie.radius.ns) %>% 
  filter(!str_detect(key.set, "Lisa Marie"))

cluster.pos <- filter(cluster.pie, AllCPS > 0) %>% 
  mutate(radius = pie.radius.ns) %>% 
  arrange(desc(X)) %>% 
  replace(. == 0, 0.0000001) 

cluster.zero <- filter(cluster.pie, AllCPS == 0) 

# Prepare logs for plotting
logs.plot <- logs %>% 
  filter(species %in% c("sardine","anchovy","mackerel")) %>% 
  mutate(
    species = case_when(
      species == "sardine" ~ "Sardine",
      species == "anchovy" ~ "Anchovy",
      species == "mackerel" ~ "PacMack"),
    lag.size = cut(lag, seq(0,400,100), labels = FALSE)
  )

# Create seine map
log.set.comp.map <- base.map +
  geom_sf(data = nav.path.sf) +
  geom_sf(data = transects.sf, size = 1) + 
  # Plot purse seine pies
  scatterpie::geom_scatterpie(data = set.pos, aes(X, Y, group = key.set, r = r*3),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, sardine.color)) +
  geom_point(data = set.zero, aes(X, Y)) +
  geom_point(data = logs.plot, aes(X, Y, fill = species), shape = 21, size = 2) +
  facet_wrap(~species, ncol = 1) +
  labs(title = "Long Beach Carnage Sets") +
  coord_sf(crs = 3310, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]*1.1), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]*0.95))

# Create map
log.trawl.comp.map <- base.map +
  geom_sf(data = nav.path.sf) +
  geom_sf(data = transects.sf, size = 1) + 
  # Plot trawl clusters
  scatterpie::geom_scatterpie(data = cluster.pos, aes(X, Y, group = cluster, r = r*3),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, sardine.color)) +
  geom_point(data = set.zero, aes(X, Y)) +
  geom_point(data = logs.plot, aes(X, Y, fill = species), shape = 21, size = 2) +
  geom_point(data = cluster.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  facet_wrap(~species, ncol = 1) +
  labs(title = "Lasker Clusters") +
  coord_sf(crs = 3310, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]*1.1), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]*0.95))

# Combine plots
seine.combo <- cowplot::plot_grid(log.set.comp.map, log.trawl.comp.map, nrow = 1)

# Save figure
ggsave(log.set.comp.map, filename = here("Figs/fig_purse_seine_logs.png"),
       width = 6, height = 15)

ggsave(log.trawl.comp.map, filename = here("Figs/fig_trawl_logs.png"),
       width = 6, height = 15)

ggsave(seine.combo, filename = here("Figs/fig_trawl_seine_logs_combo.png"),
       width = 12, height = 15)


# Create map
log.set.comp.map.jitter <- base.map +
  geom_sf(data = nav.path.sf) +
  geom_sf(data = transects.sf, size = 1) + 
  # Plot trawl clusters
  scatterpie::geom_scatterpie(data = set.pos, aes(X, Y, group = key.set, r = r*3),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "Sardine"),
                              color = 'black', alpha = 0.6) +
  # # Plot trawl clusters
  # scatterpie::geom_scatterpie(data = cluster.pos, aes(X, Y, group = cluster, r = r*2.5),
  #                             cols = c("Anchovy", "JackMack", "Jacksmelt",
  #                                      "PacHerring", "PacMack", "Sardine"),
  #                             color = 'black', alpha = 0.6) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, sardine.color)) +
  geom_point(data = set.zero, aes(X, Y)) +
  geom_point(data = filter(logs.plot, species == "Sardine"), aes(X, Y, fill = species), shape = 21, size = 2) +
  geom_point(data = filter(logs.plot, species == "Anchovy"), aes(X + 2000, Y, fill = species), shape = 21, size = 2) +
  geom_point(data = filter(logs.plot, species == "PacMack"), aes(X - 2000, Y + 2000, fill = species), shape = 21, size = 2) +
  coord_sf(crs = 3310, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]*1.1), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]*0.95))

ggsave(log.set.comp.map.jitter, filename = here("Figs/fig_seine_logs_jitter.png"))
