# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,mapproj,plotKML,ggmap,shadowtext,lubridate,
               sf,here,rnaturalearth,swfscMisc,fs,photobiology,
               mapview,marmap)


# Configure document parameters -------------------------------------------
# Set ggplot2 theme
theme_set(theme_bw())

# Create directory to store tables
dir_create(here("Output/tables"))

# Source functions
source(here("Code/functions.R"))

# User input --------------------------------------------------------------
# Define transit and survey speed (kn) for estimating progress
survey.speed  <- 9.5
transit.speed <- 12

# Get NOAA bathymetry (used to extract bathymetry)
get.bathy     <- FALSE
extract.bathy <- TRUE

# Region vector
region.vec <- c(0,34.448,41.99,48.490, 55)

# Beginning transit length (d)
transit.distance <- 850

transit.duration <- ceiling(transit.distance/transit.speed/24)

# Leg waste (d) due to transit, late departures, and early arrivals
leg.waste <- c(4,2,2,2)

# Remove transects to adjust survey progress
transects.rm <- c(seq(110, 128, 2), # Adaptive transects along Vancouver Is.
                  seq(42,58,2)) # Adaptive transects between Mendocino and SF

# Compute leg durations and breaks ----------------------------------------
# Calculate total days at sea (DAS)
total.das <- sum(c(0,
                   abs(ymd("2019-06-13") - ymd("2019-07-02")) + 1,
                   abs(ymd("2019-07-07") - ymd("2019-07-25")) + 1,
                   abs(ymd("2019-07-30") - ymd("2019-08-17")) + 1,
                   abs(ymd("2019-08-22") - ymd("2019-09-09")) + 1))

# Leg durations used to split transects 
leg.length <- c(0,
                abs(ymd("2019-06-13") - ymd("2019-07-02")) + 1 - leg.waste[1],
                abs(ymd("2019-07-07") - ymd("2019-07-25")) + 1 - leg.waste[2],
                abs(ymd("2019-07-30") - ymd("2019-08-17")) + 1 - leg.waste[3],
                abs(ymd("2019-08-22") - ymd("2019-09-09")) + 1 - leg.waste[4]) 

leg.breaks <- cumsum(as.numeric(leg.length))

# Read GPX file from Rose Point
# GPX file is created by exporting the waypoints (only, not routes) from Rose Point
route <- readGPX(here("Data/rosepoint_export.gpx"))

# create data frame of waypoints
wpts <- route$waypoints %>% 
  # select(-sym) %>% 
  arrange(name) %>% 
  # write to csv
  write_csv(here("Output/all_waypoints.csv"))

# extract transect waypoints
transects <- wpts %>% 
  filter(!str_detect(name, "UCTD")) %>% 
  mutate(
    type = case_when(
      str_detect(name, "A") ~ "Adaptive",
      str_detect(name, "C") ~ "Compulsory",
      str_detect(name, "S") ~ "Nearshore",
      str_detect(name, "M") ~ "Mammal",
      str_detect(name, "E") ~ "Extra",
      str_detect(name, "T") ~ "Transit",
      str_detect(name, "N") ~ "Nearshore",
      str_detect(name, "O") ~ "Offshore",
      TRUE ~ "Unknown"),
    Waypoint = as.numeric(str_extract(name,"\\d{1,3}\\.\\d{1,3}")),
    Transect = floor(Waypoint)) %>%
  mutate(group = paste(Transect, type)) %>% 
  arrange(type, Transect, Waypoint) %>% 
  select(Transect, Waypoint, Latitude = lat, Longitude = lon, Type = type, group) %>% 
  filter(!is.na(Type), !is.na(Transect))

# day.length <- seq(12, 16, (16 - 12)/length(unique(transects$Transect)))

# get inshore most waypoint of compulsory and adaptive transects
starts <- transects %>% 
  group_by(Type, Transect, group) %>% 
  summarise(
    long = max(Longitude),
    lat = Latitude[which.max(Longitude)]) %>% 
  filter(Type %in% c("Adaptive","Compulsory"))

# Calculate daylength across survey area ----------------------------------
daylength.max <- day_length(date = ymd("2019-06-13"),
                         geocode = data.frame(lat = max(starts$lat), lon = max(starts$long)),
                         twilight = "none")

daylength.min <- day_length(date = ymd("2019-09-09"),
                         geocode = data.frame(lat = min(starts$lat), lon = min(starts$long)),
                         twilight = "none")

daylength.df <- data.frame(Transect = seq(1, max(starts$Transect)),
                           daylength = seq(daylength.min, daylength.max,
                                           (daylength.max - daylength.min)/(max(starts$Transect) - 1)))

# Get survey regions from inshore most waypoints
transect.regions <- transects %>% 
  group_by(Type, Transect, group) %>% 
  summarise(
    long = max(Longitude),
    lat = Latitude[which.max(Longitude)]) %>% 
  filter(Type %in% c("Adaptive","Compulsory", "Nearshore", "Offshore")) %>% 
  mutate(loc = cut(lat, region.vec, labels = FALSE),
         Region = as.factor(case_when(
           loc == 1 ~ "S. CA Bight",
           loc == 2 ~ "Central CA",
           loc == 3 ~ "WA/OR",
           loc == 4 ~ "Vancouver Is.",
           TRUE ~ "Other")),
         Region = fct_reorder(Region, loc))

# add leg designations to transects
transects <- left_join(transects, select(starts, group)) %>% 
  left_join(select(transect.regions, group, Region)) %>% 
  arrange(Type, Transect, Waypoint) 

# extract UCTD stations
uctds <- wpts %>% 
  filter(str_detect(name, "UCTD")) %>% 
  mutate(station = name,
         loc = cut(lat, region.vec, labels = FALSE),
         Region = as.factor(case_when(
           loc == 1 ~ "S. CA Bight",
           loc == 2 ~ "Central CA",
           loc == 3 ~ "WA/OR",
           loc == 4 ~ "Vancouver Is.",
           TRUE ~ "Other")),
         Region = fct_reorder(Region, loc)) %>% 
  arrange(station) 

# Get map data ------------------------------------------------------------
# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

# Coordinate reference systems for geographic and projected data
crs.geog <- 4326 # WGS84
crs.proj <- 3310 # Califoria Albers Equal Area

# Import landmarks
locations <- filter(read.csv(here("Data/places.csv")), name %in% label.list) 

locations.sf <- locations %>%
  st_as_sf(coords = c("lon","lat"), crs = crs.geog)

# Project sf
locations.sf <- project_sf(locations.sf, crs.proj) %>% 
  arrange(Y)

# Get 1000fm isobath
bathy <- st_read(here("Data/GIS/bathy_contours.shp"))

# Download worldwide states 
# and filter for Western N. Am.
states <- ne_states(
  country = c("Canada",
              "United States of America",
              "Mexico"),
  returnclass = 'sf') %>% 
  filter(name %in% c("California","Oregon","Idaho",
                     "Washington","Nevada",
                     "British Columbia","Alberta",
                     "Baja California",
                     "Baja California Sur")) %>% 
  st_cast("POLYGON") %>% 
  mutate(area = st_area(.)) %>% 
  arrange(desc(area)) 

# Set bounding box around transects
map.bounds <- transects %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Create base map -------------------------------------------
base.map <- ggplot() +
  # Plot bathymetry contours
  geom_sf(data = bathy, colour = "gray90", alpha = 0.5) +
  # Plot high-res land polygons
  geom_sf(data = states, fill = "gray90", colour = "gray50") +
  # Plot landmarks
  geom_point(data = locations.sf, aes(X, Y), size = 2, colour = 'gray50') +
  geom_shadowtext(data  = locations.sf, aes(X, Y, label = name), 
                  colour = 'gray20', size = 2, fontface = 'bold', 
                  hjust = 0, nudge_x = 0.2, nudge_y = 0.05, angle = 25, 
                  bg.colour = "white") +
  # Format axes and titles
  xlab("Longitude") + ylab("Latitude") + 
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) +
  theme(axis.text.y          = element_text(angle = 90, hjust = 0.5),
        legend.position      = c(0,0),
        legend.justification = c(0,0),
        legend.background    = element_blank(),
        legend.key           = element_blank(),
        # panel.background     = element_rect(fill = alpha("lightblue", 0.5)),
        plot.title           = element_text(hjust = 0.5),
        panel.grid.major     = element_line(color = "gray90"))

# Extract odd (Compulsory) transects, sort onshore to offshore
transects.odd <- filter(transects, Transect %% 2 == 1) %>% 
  # filter(!Transect %in% transects.rm) %>% 
  arrange(desc(Transect), Waypoint) %>% 
  mutate(order = seq_along(Transect))

# Extract odd transects (Adaptive), sort offshore to onshore
transects.even <- filter(transects, Transect %% 2 == 0) %>% 
  # filter(!Transect %in% transects.rm) %>% 
  arrange(desc(Transect), desc(Waypoint)) %>% 
  mutate(order = seq_along(Transect))

# Create route for planning
empty <- st_as_sfc("POINT(EMPTY)")

# Combine even and odd transects into one continuous route
route.fsv <- filter(transects.odd, Type %in% c("Adaptive","Compulsory")) %>% 
  bind_rows(filter(transects.even, Type %in% c("Adaptive","Compulsory"))) %>% 
  arrange(desc(Transect), order) %>%
  left_join(daylength.df) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) %>% 
  select(-group, -order) %>%
  mutate(long = as.data.frame(st_coordinates(.))$X,
         lat = as.data.frame(st_coordinates(.))$Y,
         distance_to_next = as.numeric(na.omit(c(0, st_distance(geometry,
                                                                lead(geometry, 
                                                                     default = empty),
                                                                by_element = TRUE))))/1852,
         distance_cum = cumsum(distance_to_next),
         time_to_next = distance_to_next / survey.speed / daylength,
         time_cum = cumsum(time_to_next) + transit.duration,
         leg      = cut(time_cum, leg.breaks, labels = FALSE, include.lowest = TRUE)) %>%
  st_set_geometry(NULL)

# Write route plan to CSV
write_csv(route.fsv, here("Output/route_plan_fsv.csv"))

# Plot the route
route.plot.fsv <- ggplot(route.fsv, aes(long, lat, colour = factor(leg))) +
  geom_path() +
  geom_text(data = locations, aes(lon, lat, label = name), 
            size = 2, hjust = 0, inherit.aes = FALSE) +
  scale_colour_discrete("Leg") + 
  coord_map()

# Save the route plot
ggsave(route.plot.fsv, filename = here("Figs/route_plan.png"))

# Add legs to transects ---------------------------------------------------
leg.summ <- route.fsv %>% 
  group_by(Transect) %>% 
  summarise(Leg = max(leg))

transects <- transects %>% 
  left_join(leg.summ)

# Extract bathymetry ------------------------------------------------------
# Get bathymetry data across range of nav data (plus/minus one degree lat/long)
if (get.bathy) {
  bathy_dem <- getNOAA.bathy(lon1 = min(transects$Longitude - 1), 
                             lon2 = max(transects$Longitude + 1),
                             lat1 = max(transects$Latitude) + 1, 
                             lat2 = min(transects$Latitude) - 1, 
                             resolution = 1)
  # Save bathy results
  save(bathy_dem, file = here("Data/bathymetry.Rdata"))  
} else {
  load(here("Data/bathymetry.Rdata"))
}

# Extract bathymetry
transects$Depth <- get.depth(bathy_dem, transects$Longitude, transects$Latitude, locator = F, distance = T)$depth
uctds$Depth     <- get.depth(bathy_dem, uctds$lon, uctds$lat, locator = F, distance = T)$depth

# Extract bathymetry info
if (extract.bathy) {
  source(here("Code/extract_bathymetry.R"))
}

# Convert transects to sf
transects.sf <- transects %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) %>% 
  group_by(Transect, Type, Leg, Region) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(brg = 360 + stplanr::line_bearing(.)) %>% 
  st_transform(crs = crs.proj) %>% 
  mutate(dist = as.numeric(st_length(.)/1852))

# mapview(transects.sf, zcol = "Type")

# Convert uctds to sf
uctds.sf <- uctds %>% 
  st_as_sf(coords = c("lon","lat"), crs = crs.geog)

# export tables to csv
table.export <- select(transects, -group, -Leg)
write_csv(table.export, here("Output/tables/waypoints_all.csv"))
write_csv(filter(table.export, Type == "Adaptive"),  here("Output/tables/waypoints_adaptive.csv"))
write_csv(filter(table.export, Type == "Compulsory"),here("Output/tables/waypoints_compulsory.csv"))
write_csv(filter(table.export, Type == "Saildrone"), here("Output/tables/waypoints_saildrone.csv"))
# write_csv(filter(table.export, Type == "Mammal"),    here("Output/tables/waypoints_mammal.csv"))
write_csv(filter(table.export, Type == "Nearshore"), here("Output/tables/waypoints_nearshore.csv"))
write_csv(filter(table.export, Type == "Transit"),   here("Output/tables/waypoints_transit.csv"))
write_csv(filter(table.export, Type == "Offshore"),  here("Output/tables/waypoints_offshore.csv"))
# write_csv(filter(table.export, Type == "Extra"),     here("Output/tables/waypoints_extra.csv"))

# Export survey plan for survey report
wpt.plan <- table.export %>% 
  filter(Type %in% c("Adaptive","Compulsory")) %>% 
  select(line = Transect, wpt = Waypoint, lon = Longitude, lat = Latitude, type = Type) %>% 
  # mutate(type = tolower(type)) %>% 
  write_csv(here("Output/tables/waypoint_plan.csv"))

# format UCTDs for export
uctd.export <- uctds %>% 
  select(Name = name, Latitude = lat, Longitude = lon, Region)

# export to csv
write_csv(uctd.export, here("Output/tables/waypoints_uctd.csv"))

# Create the map with all transects
survey.map <- base.map +
  # geom_path(data = bathy, aes(long,lat), colour = "gray70", size = 0.25) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Mammal", 
                                                  "Nearshore","Offshore", "Transit")),
          aes(linetype = Type, colour = Type)) +
  scale_colour_manual(name = "Type", values = c("Adaptive" = "red","Compulsory" = "blue",
                                                "Offshore" = "green", "Nearshore" = "#F08C09",
                                                "Transit" = "cyan")) +
  geom_sf(data = uctds.sf, shape = 21, size = 1, fill = "white") +
  scale_linetype_manual(name = "Type", values = c("Adaptive" = "solid", "Compulsory" = "solid", 
                                                  "Mammal" = "dashed", "Nearshore" = "solid",
                                                  "Offshore" = "solid","Transit" = "dashed")) +
  # geom_sf(data = filter(transects.sf, Type == "Saildrone"), colour = c("#F08C09")) +
  # geom_point(data = uctds, aes(lon, lat), shape = 21, fill = "white", size = 1) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) 

# Save the map
ggsave(survey.map, filename = here("Figs/survey_map.png"), height = 10, width = 7)

# Create map not showing nearshore transects along Vancouver Is. and in the SCB
transects.sd <- filter(transects.sf, Type == "Nearshore", between(Transect, 35, 211))

survey.map.sd <- base.map +
  # geom_path(data = bathy, aes(long,lat), colour = "gray70", size = 0.25) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Mammal", 
                                                  "Offshore", "Transit")),
          aes(linetype = Type, colour = Type)) +
  geom_sf(data = filter(transects.sd, Type %in% c("Adaptive", "Compulsory", "Mammal", 
                                                  "Nearshore","Offshore", "Transit")),
          aes(linetype = Type, colour = Type)) +
  scale_colour_manual(name = "Type", values = c("Adaptive" = "red","Compulsory" = "blue",
                                                "Offshore" = "green", "Nearshore" = "#F08C09",
                                                "Transit" = "cyan")) +
  geom_sf(data = uctds.sf, shape = 21, size = 1, fill = "white") +
  scale_linetype_manual(name = "Type", values = c("Adaptive" = "solid", "Compulsory" = "solid", 
                                                  "Mammal" = "dashed", "Nearshore" = "solid",
                                                  "Offshore" = "solid","Transit" = "dashed")) +
  # geom_sf(data = filter(transects.sf, Type == "Saildrone"), colour = c("#F08C09")) +
  # geom_point(data = uctds, aes(lon, lat), shape = 21, fill = "white", size = 1) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) 

# Save the map
ggsave(survey.map.sd, filename = here("Figs/survey_map_saildrone.png"), height = 10, width = 7)

# Create the map with all transects
survey.map.leg = base.map +
  # geom_path(data = bathy, aes(long,lat), colour = "gray70", size = 0.25) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Offshore", "Nearshore", "Transit")),
          aes(linetype = Type), colour = "grey50", show.legend = FALSE) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory")),
          aes(linetype = Type, colour = factor(Leg))) +
  geom_sf(data = uctds.sf, shape = 21, size = 1, fill = "white") +
  scale_linetype_manual(name = "Type", values = c("Adaptive" = "dashed", "Compulsory" = "solid", 
                                                  "Offshore" = "dashed", "Nearshore" = "dashed",
                                                  "Transit" = "dashed")) +
  scale_colour_discrete("Leg") +
  # geom_sf(data = filter(transects.sf, Type == "Saildrone"), colour = c("#F08C09")) +
  # geom_point(data = uctds, aes(lon, lat), shape = 21, fill = "white", size = 1) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) 

# Save the map
ggsave(survey.map.leg, filename = here("Figs/survey_map_leg.png"), height = 10, width = 7)

# Create the map with all transects
survey.map.region = base.map +
  # geom_path(data = bathy, aes(long,lat), colour = "gray70", size = 0.25) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Offshore", "Nearshore", "Transit")),
          aes(linetype = Type), colour = "grey50", show.legend = FALSE) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Nearshore", "Offshore")),
          aes(linetype = Type, colour = factor(Region))) +
  geom_sf(data = uctds.sf, shape = 21, size = 1, fill = "white") +
  scale_linetype_manual(name = "Type", values = c("Adaptive" = "dashed", "Compulsory" = "solid", 
                                                  "Offshore" = "dashed", "Nearshore" = "dashed",
                                                  "Transit" = "dashed")) +
  scale_colour_discrete("Leg") +
  # geom_sf(data = filter(transects.sf, Type == "Saildrone"), colour = c("#F08C09")) +
  # geom_point(data = uctds, aes(lon, lat), shape = 21, fill = "white", size = 1) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) 

# Save the map
ggsave(survey.map.region, filename = here("Figs/survey_map_region.png"), height = 10, width = 7)

# Route plan-Nearshore ----------------------------------------------------
transects.ns <- read_csv(here("Output/transect_wpts_ns.csv")) %>% 
  rename(Waypoint = id) %>% 
  mutate(Transect = round(as.numeric(str_replace(Waypoint, "N",""))),
         Type = "Nearshore")

# Extract odd (Compulsory) transects, sort onshore to offshore
transects.odd <- filter(transects.ns, Transect %% 2 == 1) %>% 
  # filter(!Transect %in% transects.rm) %>% 
  arrange(Transect, Waypoint) %>% 
  mutate(order = seq_along(Transect))

# Extract odd transects (Adaptive), sort offshore to onshore
transects.even <- filter(transects.ns, Transect %% 2 == 0) %>% 
  # filter(!Transect %in% transects.rm) %>% 
  arrange(Transect, desc(Waypoint)) %>% 
  mutate(order = seq_along(Transect))

# Combine even and odd transects into one continuous route
route.ns <- filter(transects.odd, Type %in% c("Nearshore")) %>% 
  bind_rows(filter(transects.even, Type %in% c("Nearshore"))) %>% 
  arrange(Transect, order) %>%
  # left_join(daylength.df) %>%
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  select(-order) %>%
  mutate(long = as.data.frame(st_coordinates(.))$X,
         lat = as.data.frame(st_coordinates(.))$Y,
         distance_to_next = as.numeric(na.omit(c(0, st_distance(geometry,
                                                                lead(geometry, 
                                                                     default = empty),
                                                                by_element = TRUE))))/1852,
         distance_cum = cumsum(distance_to_next)) %>%
  st_set_geometry(NULL)

# Write route plan to CSV
write_csv(route.ns, here("Output/route_plan_nearshore.csv"))

# Plot the route
route.plot.ns <- ggplot(route.ns, aes(long, lat)) +
  geom_path() +
  geom_text(data = locations, aes(lon, lat, label = name), 
            size = 2, hjust = 0, inherit.aes = FALSE) +
  # scale_colour_discrete("Leg") + 
  coord_map()

# Save the route plot
ggsave(route.plot.ns, filename = here("Figs/route_plan_nearshore.png"))


# Route plan-Saildrone ----------------------------------------------------
transects.sd <- read_csv(here("Output/transect_wpts_sd.csv")) %>% 
  rename(Waypoint = id) %>% 
  mutate(Transect = round(as.numeric(str_replace(Waypoint, "S",""))),
         Type = "Saildrone")

# Extract odd (Compulsory) transects, sort onshore to offshore
transects.odd <- filter(transects.sd, Transect %% 2 == 1) %>% 
  # filter(!Transect %in% transects.rm) %>% 
  arrange(Transect, Waypoint) %>% 
  mutate(order = seq_along(Transect))

# Extract odd transects (Adaptive), sort offshore to onshore
transects.even <- filter(transects.sd, Transect %% 2 == 0) %>% 
  # filter(!Transect %in% transects.rm) %>% 
  arrange(Transect, desc(Waypoint)) %>% 
  mutate(order = seq_along(Transect))

# Combine even and odd transects into one continuous route
route.sd <- filter(transects.odd, Type %in% c("Saildrone")) %>% 
  bind_rows(filter(transects.even, Type %in% c("Saildrone"))) %>% 
  arrange(Transect, order) %>%
  # left_join(daylength.df) %>%
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  select(-order) %>%
  mutate(long = as.data.frame(st_coordinates(.))$X,
         lat = as.data.frame(st_coordinates(.))$Y,
         distance_to_next = as.numeric(na.omit(c(0, st_distance(geometry,
                                                                lead(geometry, 
                                                                     default = empty),
                                                                by_element = TRUE))))/1852,
         distance_cum = cumsum(distance_to_next)) %>%
  st_set_geometry(NULL)

# Write route plan to CSV
write_csv(route.sd, here("Output/route_plan_saildrone.csv"))

# Plot the route
route.plot.sd <- ggplot(route.sd, aes(long, lat)) +
  geom_path() +
  geom_text(data = locations, aes(lon, lat, label = name), 
            size = 2, hjust = 0, inherit.aes = FALSE) +
  # scale_colour_discrete("Leg") + 
  coord_map()

# Save the route plot
ggsave(route.plot.sd, filename = here("Figs/route_plan_saildrone.png"))

