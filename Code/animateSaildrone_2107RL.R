# gganimate Wiki:
# https://github.com/thomasp85/gganimate/wiki

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,lubridate,here,rnaturalearth,shadowtext,sf,
               surveyR,gganimate,transformr,ggmap,xts,gifski,mapproj,
               stringr,mapview)

# Google Maps API key for Kevin Stierhoff (@Gmail)
ggmap::register_google(google_map_api)

# User controls ----------------------------------------------------------------
get.nav <- FALSE

# Set limits for latitude and longitude ----------------------------------------
min.lat  <-   31
max.lat  <-   52
min.long <- -132
max.long <- -117

# Get project name from directory
prj.name <- data.table::last(unlist(str_split(here(),"/")))

# Get all settings files
settings.files <- dir(here("Doc/settings"))

# Source survey settings file
prj.settings <- settings.files[str_detect(settings.files, paste0("settings_", prj.name, ".R"))]
source(here("Doc/settings", prj.settings))

# Load nav data ----------------------------------------------------------------
if (get.nav) {
  # Get Nav data
  source(here("Code/get_nav.r"))
} else {
  # Load saildrone data
  load(here("Data/Nav/nav_data.Rdata"))
  load(here("Data/Nav/nav_data_saildrone.Rdata"))
}

# Format and downsample Lasker data
nav <- nav %>%
  mutate(date  = lubridate::date(time),
         hour  = lubridate::hour(time),
         time.align = align.time(time, n=3600)) %>% 
  filter(hour %in% seq(1,24,2),
         date <= date("2021-10-07")) %>%
  group_by(date, hour) %>%
  slice(1) %>% 
  ungroup() %>% 
  arrange(time.align)

# Format and downsample Saildrone data
nav.sd    <- nav.sd %>%
  mutate(date  = lubridate::date(datetime),
         hour  = lubridate::hour(datetime),
         time.align = align.time(datetime, n=3600)) %>% 
  filter(hour %in% seq(1,24,2)) %>%
  group_by(saildrone, date, hour) %>%
  slice(1) %>%
  ungroup() %>% 
  arrange(time.align)

# Read bathy contours shapefile 
bathy_contours <- st_read(here("Data/GIS/bathy_contours.shp")) %>% 
  st_transform(4326) %>% 
  rename(Depth = Contour)

# Get land features
na_sf <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

locations <- filter(read.csv(here("Data/Map/locations.csv")), name %in% label.list)

# Set map boundaries
map.bounds <- map_bounds(nav$lat, nav$long, 0.1)

# Create base map -------------------------------------------
base.map <- ggplot() +
  # Plot bathymetry contours
  geom_sf(data = bathy_contours, colour = "gray70") +
  # Plot high-res land polygons
  geom_sf(data = na_sf, fill = "gray70", color = "black") +
  # Plot landmarks
  geom_point(data = locations, aes(long,lat), size = 2, colour = 'black') +
  geom_shadowtext(data  = locations,aes(long,lat, label = name), 
                  colour = 'gray20', size = 5, fontface = 'bold', 
                  hjust = 0, nudge_x = 0.2, nudge_y = 0.05, angle = 25, 
                  bg.colour = "white") +
  # Format axes and titles
  xlab("Longitude") + ylab("Latitude") + 
  coord_sf(
    xlim = map.bounds$range.lon,
    ylim = map.bounds$range.lat) +
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position =  c(0,0),
        legend.justification = c(0,0),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(base.map, filename = here("Figs/basemap_sf.png"),
       height = 15, width = 10)

# Create animations on simple features
cce.map <- base.map + 
  geom_path(data = nav, 
            aes(long, lat), 
            colour = "black", size = 1) + 
  geom_path(data = nav.sd, 
            aes(long, lat, colour = factor(saildrone)),
            size = 1) +
  scale_colour_discrete("Saildrone") +
  coord_sf(crs = 4326,
           xlim = c(-130,-116),
           ylim = c(32,51)) +
  labs(title = "2021 Summer CCE: Lasker and Saildrone Tracks",
       subtitle = "Time:{frame_along}",
       x = "Longitude", y = "Latitude") +
  transition_reveal(date)

# Animate map
animate(cce.map, duration = 60, 
        width = 800*0.8, height = 1200*0.8)

# Save output
anim_save("cce_map_2107RL.gif", path = here("Output"))

# # Animate as an MPEG4
# animate(cce.map, duration = 60, width = 800*0.8, height = 1200*0.8, renderer = ffmpeg_renderer())
# # Save output as MPEG file
# anim_save("cce_map_2107RL.mpeg", path = here("Output"))

# # draw a base box for maps.
# tracks_box <- make_bbox(lon = nav$long, lat = nav$lat, f = 0.1)
# wc.map <- get_map(location = tracks_box, maptype = "satellite", source = "google", zoom = 5)

# # Create animation on data frames
# cce.map <- ggmap(wc.map) + 
#   geom_path(data = nav, 
#             aes(long, lat), 
#             colour = "white", size = 1) + 
#   geom_path(data = nav.sd, 
#             aes(long, lat, colour = factor(saildrone)),
#             size = 1) +
#   scale_colour_discrete("Saildrone") +
#   coord_map(xlim = c(-130,-116),
#             ylim = c(32,51)) +
#   theme_bw() +
#   labs(title = "2021 Summer CCE: Lasker and Saildrone Tracks",
#        subtitle = "Time:{frame_along}",
#        x = "Longitude", y = "Latitude") +
#   # transition_time(date) +
#   transition_reveal(date) +
#   # transition_manual(date, cumulative = TRUE) +
#   NULL

# # Animate as a GIF
# animate(cce.map, duration = 60,  
#         width = 800*0.8, height = 1200*0.8)
# 
# # Save output as GIF
# anim_save("cce_map_2021.gif", path = here("Output"))
