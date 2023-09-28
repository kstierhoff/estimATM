# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,lubridate,here,sf,knitr,fs,leaflet,leaflet.extras,bookdown,
               leafem,mapview,htmltools)

# Install and load required packages from Github -------------------------------
# atm
pacman::p_load_gh("kstierhoff/atm")
pacman::p_load_gh("kstierhoff/surveyR")

# Get project name from directory
prj.name <- last(unlist(str_split(here(), "/")))

# Get all settings files
settings.files <- dir(here("Doc/settings"))

# Source survey settings file
prj.settings <- settings.files[str_detect(settings.files, paste0("settings_", prj.name, ".R"))]
source(here("Doc/settings", prj.settings))

# Load Shimada nasc
load(here("Output/nasc_plotCSV.Rdata"))

# Define cps.nasc
nasc <- nasc %>% 
  mutate(cps.nasc = purrr::pluck(., "NASC.250"),
         cps.nasc.source = "NASC.250")

# Average cps.nasc over defined interval
# Summarize by filename, not transect, so that renamed (i.e., strip.tx.chars == T) transects get included.
nasc.sf <- nasc %>%
  select(filename, transect, int, dist_m, datetime, lat, long, cps.nasc) %>% 
  group_by(filename, transect, int) %>% 
  summarise(
    lat   = lat[1],
    long  = long[1],
    NASC  = mean(cps.nasc),
    label = paste0('Transect: ', transect[1], "; ",
                   'Distance: ', round(min(dist_m)), "-", round(max(dist_m)), ' m'),
    popup = paste0('<b>Transect: </b>', transect[1], '<br/>',
                   '<b>Time: </b>', min(datetime), " - ", max(datetime), ' UTC<br/>',
                   '<b>Distance: </b>', round(min(dist_m)), "-", round(max(dist_m)), ' m<br/>',
                   '<b>NASC: </b>', round(mean(NASC)), ' m<sup>2</sup> nmi<sup>-2</sup>')) %>%
  # Create bins for defining point size in NASC plots
  mutate(bin       = cut(NASC, nasc.breaks, include.lowest = T),
         bin.level =  as.numeric(bin)) %>% 
  filter(!is.na(bin)) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) 

# Format for plotting
nasc.plot <- project_sf(nasc.sf, crs.proj)

# Load trawl locations
hake.tows <- readxl::read_excel(here("Data/Trawl/2307RL_trawl_logs.xlsx"), sheet = "Shimada") %>% 
  filter(!is.na(lat), !is.na(long)) %>% 
  project_df(to = crs.proj)


# Quick plot
ggplot() + 
  geom_point(data = nasc.plot, aes(X, Y, size = NASC)) + 
  geom_point(data = hake.tows, aes(X, Y), colour = "red") + 
  coord_sf(crs = crs.proj)

# Load base map
load("C:/KLS/CODE/Github/estimATM/2307RL/Data/Map/basemap.Rdata")

map.bounds <- nasc.sf %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Plot sA for CPS #####
# Select plot levels for backscatter data
nasc.levels.all <- unique(nasc.plot$bin.level)
nasc.labels.all <- nasc.labels[sort(nasc.levels.all)]
nasc.sizes.all  <- nasc.sizes[sort(nasc.levels.all)]
nasc.colors.all <- nasc.colors[sort(nasc.levels.all)]

# Map backscatter
base.map +
  # Plot NASC data
  geom_point(data = nasc.plot, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and color scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Add hake tows
  geom_point(data = hake.tows, aes(X, Y), shape = 21, fill = "green", size = 3) + 
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
