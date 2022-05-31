# Configure document parameters -------------------------------------------
# Create directory to store tables
dir_create(here("Output/tables"))
dir_create(here("Figs"))

# Read GPX file
route <- read_GPX(here("Data/Nav", gpx.file))

# Create data frame of waypoints
wpts <- route$waypoints %>% 
  project_sf(crs = crs.geog) %>% 
  select(lon = X, lat = Y, name)

write_csv(wpts, here("Output/waypoints/all_waypoints.csv"))

# Extract transect waypoints
if (!exists("renumber.transects")) {
  # Process normally
  transects <- wpts %>% 
    filter(!str_detect(name, "UCTD")) %>%
    filter(!str_detect(name, "Pairovet")) %>% 
    mutate(
      type = case_when(
        str_detect(name, "A+$") ~ "Adaptive",
        str_detect(name, "C+$") ~ "Compulsory",
        str_detect(name, "S+$") ~ "Saildrone",
        str_detect(name, "M+$") ~ "Mammal",
        str_detect(name, "E+$") ~ "Extra",
        str_detect(name, "T+$") ~ "Transit",
        str_detect(name, "N+$") ~ "Nearshore",
        str_detect(name, "O+$") ~ "Offshore",
        TRUE ~ "Unknown"),
      Waypoint = as.numeric(str_extract(name,"\\d{1,3}\\.\\d{1,3}")),
      Transect = floor(Waypoint)) %>%
    mutate(group = paste(Transect, type)) %>% 
    arrange(type, Transect, Waypoint) %>% 
    select(Transect, Waypoint, Latitude = lat, Longitude = lon, Type = type, group, name) %>% 
    filter(!is.na(Type), !is.na(Transect))  
} else {
  # Renumber transects
  transects <- wpts %>% 
    filter(!str_detect(name, "UCTD")) %>%
    filter(!str_detect(name, "Pairovet")) %>% 
    mutate(
      type = case_when(
        str_detect(name, "A+$") ~ "Adaptive",
        str_detect(name, "C+$") ~ "Compulsory",
        str_detect(name, "S+$") ~ "Saildrone",
        str_detect(name, "M+$") ~ "Mammal",
        str_detect(name, "E+$") ~ "Extra",
        str_detect(name, "T+$") ~ "Transit",
        str_detect(name, "N+$") ~ "Nearshore",
        str_detect(name, "O+$") ~ "Offshore",
        TRUE ~ "Unknown"),
      Waypoint = as.numeric(str_extract(name,"\\d{1,3}\\.\\d{1,3}"))) %>% 
    arrange(type, Waypoint)
  
  transects.adjusted <- data.frame()
  
  for (kk in unique(transects$type)) {
    if (kk %in% c("Adaptive", "Compulsory")) {
      # Calculate waypoint adjustment
      adjust.tx.n <- transects %>% 
        filter(type %in% c("Adaptive","Compulsory")) %>% 
        summarise(minTx = min(floor(Waypoint)) - 1) %>% 
        pull()
    } else {
      adjust.tx.n <- transects %>% 
        filter(type == kk) %>% 
        summarise(minTx = min(floor(Waypoint)) - 1) %>% 
        pull()
    }
    
    # Finish formatting transects
    tx.kk <- transects %>%
      filter(type == kk) %>% 
      mutate(
        Waypoint = case_when(
          renumber.transects ~ Waypoint - adjust.tx.n,
          TRUE ~ Waypoint),
        Transect = as.integer(floor(Waypoint)),
        wpt.tmp = paste0(sprintf("%03d", Transect), trimws(str_extract(.$Waypoint,"\\.\\d{1,3}$"))),
        name = case_when(
          str_detect(name, "A+$") ~ paste0(wpt.tmp, "A"),
          str_detect(name, "C+$") ~ paste0(wpt.tmp, "C"),
          str_detect(name, "S+$") ~ paste0(wpt.tmp, "S"),
          str_detect(name, "M+$") ~ paste0(wpt.tmp, "M"),
          str_detect(name, "E+$") ~ paste0(wpt.tmp, "E"),
          str_detect(name, "T+$") ~ paste0(wpt.tmp, "T"),
          str_detect(name, "N+$") ~ paste0(wpt.tmp, "N"),
          str_detect(name, "O+$") ~ paste0(wpt.tmp, "O"),
          TRUE ~ "Unknown")) %>%
      mutate(group = paste(Transect, type)) %>% 
      arrange(type, Transect, Waypoint) %>% 
      select(Transect, Waypoint, Latitude = lat, Longitude = lon, Type = type, group, name) %>% 
      filter(!is.na(Type), !is.na(Transect)) 
    
    # Combine results
    transects.adjusted <- bind_rows(transects.adjusted, tx.kk)
  }
}

# Replace existing data frame with new values
transects <- transects.adjusted %>%
  arrange(Type, Transect, Waypoint)

# If specific transects are to be removed manually
if (!is.na(rm.i.transects)) {
  transects <- transects %>% 
    filter(!group %in% rm.i.transects)
}

# Get inshore most waypoint of compulsory and adaptive transects
starts <- transects %>% 
  group_by(Type, Transect, group) %>% 
  summarise(
    long = max(Longitude),
    lat = Latitude[which.max(Longitude)]) %>% 
  filter(Type %in% c("Adaptive","Compulsory")) %>% 
  ungroup()

# Calculate day length across survey area ----------------------------------
daylength.max <- day_length(date = min(leg.ends),
                            geocode = data.frame(lat = max(starts$lat), lon = max(starts$long)),
                            twilight = "none")

daylength.min <- day_length(date = max(leg.ends),
                            geocode = data.frame(lat = min(starts$lat), lon = min(starts$long)),
                            twilight = "none")

daylength.df <- data.frame(Transect = seq(1, max(starts$Transect)),
                           daylength = seq(daylength.min, daylength.max,
                                           (daylength.max - daylength.min)/(max(starts$Transect) - 1))) %>% 
  left_join(select(starts, Transect, lat))

# Get survey regions from inshore most waypoints
transect.regions <- transects %>% 
  group_by(Type, Transect, group) %>% 
  summarise(
    long = max(Longitude),
    lat = Latitude[which.max(Longitude)]) %>% 
  filter(Type %in% c("Adaptive","Compulsory", "Nearshore", "Offshore", "Saildrone")) %>% 
  mutate(loc = cut(lat, region.vec, labels = FALSE),
         Region = as.factor(case_when(
           loc == 1 ~ "Mexico",
           loc == 2 ~ "S. CA Bight",
           loc == 3 ~ "Central CA",
           loc == 4 ~ "WA/OR",
           loc == 5 ~ "Vancouver Is.",
           TRUE ~ "Other")),
         Region = fct_reorder(Region, loc)) %>% 
  ungroup()

# add leg designations to transects
transects <- left_join(transects, select(starts, group)) %>% 
  left_join(select(transect.regions, group, Region)) %>% 
  arrange(Type, Transect, Waypoint) 

wpts.sf <- transects %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog)

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
         Region = fct_reorder(Region, loc),
         Type = "UCTD") %>% 
  rename(Latitude = lat, Longitude = lon) %>% 
  arrange(station) 

# Extract Pairovet stations
pairovets <- wpts %>% 
  filter(str_detect(name, "Pairovet")) %>% 
  mutate(station = name,
         loc = cut(lat, region.vec, labels = FALSE),
         Region = as.factor(case_when(
           loc == 1 ~ "S. CA Bight",
           loc == 2 ~ "Central CA",
           loc == 3 ~ "WA/OR",
           loc == 4 ~ "Vancouver Is.",
           TRUE ~ "Other")),
         Region = fct_reorder(Region, loc),
         Type   = "Pairovet") %>% 
  mutate(transect = as.numeric(str_sub(name, 1, 3))) %>%
  group_by(transect) %>% 
  mutate(station.order = seq_along(transect)) %>% 
  ungroup() %>% 
  mutate(type = case_when(
    station.order%%2 == 1 ~ "Compulsory",
    TRUE ~ "Adaptive"),
    name = case_when(
      type == "Adaptive" ~ str_replace(name, "C", "A"),
      TRUE ~ name)) %>% 
  rename(Latitude = lat, Longitude = lon) %>% 
  arrange(station) 

# Import landmarks
locations <- filter(read.csv(here("Data/Map/locations.csv")), name %in% label.list) %>% 
  project_df(to = crs.proj)

# Get 1000 fm isobath
bathy <- st_read(here("Data/GIS/bathy_contours.shp"))

# Get state data
states <- ne_states(country = 'United States of America', returnclass = 'sf')
ca     <- filter(states, name == "California")

# Get countries
countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

# Set bounding box around transects
map.bounds <- transects %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- map.height*map.aspect

# Create base map -------------------------------------------
base.map <- get_basemap(transects, states, countries, locations, bathy, map.bounds, crs = crs.proj) +
  # Add scalebar
  annotation_scale(style = "ticks", location = "br", height = unit(0.15, "cm"))

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

# Create route plan for the FSV -------------------------------------------
# Combine even and odd transects into one continuous route
route.fsv <- filter(transects.odd, Type %in% c("Adaptive","Compulsory")) %>% 
  bind_rows(filter(transects.even, Type %in% c("Adaptive","Compulsory"))) %>% 
  arrange(desc(Transect), order) %>%
  left_join(daylength.df) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) %>% 
  select(-group, -order) %>%
  mutate(long = as.data.frame(st_coordinates(.))$X,
         lat  = as.data.frame(st_coordinates(.))$Y,
         distance_to_next = as.numeric(
           na.omit(c(0, st_distance(geometry,
                                    lead(geometry, 
                                         default = NA),
                                    by_element = TRUE))))/1852) %>% 
  mutate(distance_to_next = c(distance_to_next[2:n()],0),
         distance_cum = cumsum(distance_to_next),
         time_to_next = distance_to_next / survey.speed / daylength,
         time_cum     = cumsum(time_to_next) + transit.duration,
         on_off       = 1,
         leg          = cut(time_cum, leg.breaks.gpx, 
                            labels = FALSE, include.lowest = TRUE)) %>%
  st_set_geometry(NULL) %>% 
  project_df(to = 3310) %>% 
  mutate(
    diff.wpt = c(diff(round(Waypoint)), 0),
    speed = case_when(
      diff.wpt == 0 ~ survey.speed,
      TRUE ~ transit.speed),
    mode         = case_when(
      speed == survey.speed ~ "survey",
      speed == transit.speed ~ "transit",
      TRUE ~ "other")) %>% 
  select(Transect, Waypoint, Type, daylength, long, lat, X, Y, on_off, speed, mode, 
         distance_to_next, distance_cum, time_to_next, time_cum, leg, Region) 

# Plot the route
route.plot.fsv <- base.map + 
  geom_path(data = route.fsv, aes(X, Y, colour = factor(leg))) +
  scale_colour_discrete("Leg") 

# Save the route plot
ggsave(route.plot.fsv, filename = here("Figs/fig_route_plan.png"),
       height = map.height, width = map.width)  

# Write route plan to CSV
write_csv(select(route.fsv, -X, -Y), 
          here("Output/routes/route_plan_fsv.csv"))

# Add legs to transects ---------------------------------------------------
leg.summ <- route.fsv %>% 
  group_by(Transect) %>% 
  summarise(Leg = max(leg)) %>% 
  ungroup()

transects <- transects %>% 
  left_join(leg.summ) %>% 
  arrange()

# Extract bathymetry ------------------------------------------------------
# Get bathymetry data across range of nav data (plus/minus one degree lat/long)
if (get.bathy) {
  noaa.bathy <- getNOAA.bathy(lon1 = min(wpts$lon - 1), 
                              lon2 = max(wpts$lon + 1),
                              lat1 = max(wpts$lat) + 1, 
                              lat2 = min(wpts$lat) - 1, 
                              resolution = 1)
  # Save bathy results
  save(noaa.bathy, file = paste(here("Data/GIS"), "/bathy_data_",
                                survey.name,".Rdata", sep = "")) 
  
} else {
  load(paste(here("Data/GIS"), "/bathy_data_",
             survey.name,".Rdata", sep = ""))
  
}

# Extract bathymetry
transects$Depth <- get.depth(noaa.bathy, transects$Longitude, transects$Latitude, locator = F, distance = T)$depth

if (nrow(uctds) > 0) {
  uctds$Depth     <- get.depth(noaa.bathy, uctds$Longitude, uctds$Latitude, locator = F, distance = T)$depth
}

if (nrow(pairovets) > 0) {
  pairovets$Depth     <- get.depth(noaa.bathy, pairovets$Longitude, pairovets$Latitude, locator = F, distance = T)$depth
}

# Extract bathymetry info
if (extract.bathy) {
  source(here("Code/extractBathy.R"))
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

# Create acoustic transect labels for maps
tx.labels.tmp <- transects %>% 
  mutate(transect.name = paste(str_sub(name, 1, 3), str_sub(name, -1))) %>% 
  group_by(group, Type) %>% 
  summarise(
    transect.name = transect.name[1],
    start.lat = Latitude[which.max(Longitude)],
    start.long = max(Longitude),
    end.lat = Latitude[which.min(Longitude)],
    end.long = min(Longitude),
    brg = 90 - swfscMisc::bearing(end.lat,end.long,start.lat,start.long)[1]) %>% 
  ungroup()

tx.end.labels <- tx.labels.tmp %>% 
  filter(start.lat < 48.54116) %>% 
  select(group, Type, transect.name, lat = end.lat, long = end.long, brg) 

tx.start.labels <- tx.labels.tmp %>% 
  filter(start.lat >= 48.54116) %>% 
  select(group, Type, transect.name, lat = start.lat, long = start.long, brg) %>% 
  rbind(tx.end.labels)

tx.labels <- project_df(tx.start.labels, to = crs.proj)

# export tables to csv
wpt.export <- select(transects, Transect, name, everything(), -group, -Leg, -Waypoint) %>% 
  rename(Waypoint = name)

dir_create(here("Output/tables_updated"))

# Write all waypoints
write_csv(wpt.export, here("Output/tables_updated/waypoints_all.csv"))

# Write adaptive waypoints
if (nrow(filter(wpt.export, Type == "Adaptive")) > 0) {
  write_csv(filter(wpt.export, Type == "Adaptive"),  here("Output/tables_updated/waypoints_adaptive.csv"))  
}
# Write compulsory waypoints
if (nrow(filter(wpt.export, Type == "Compulsory")) > 0) {
  write_csv(filter(wpt.export, Type == "Compulsory"),  here("Output/tables_updated/waypoints_compulsory.csv"))  
}
# Write Saildrone waypoints
if (nrow(filter(wpt.export, Type == "Saildrone")) > 0) {
  write_csv(filter(wpt.export, Type == "Saildrone"),  here("Output/tables_updated/waypoints_saildrone.csv"))  
}
# Write mammal waypoints
if (nrow(filter(wpt.export, Type == "Mammal")) > 0) {
  write_csv(filter(wpt.export, Type == "Mammal"),  here("Output/tables_updated/waypoints_mammal.csv"))  
}
# Write nearshore waypoints
if (nrow(filter(wpt.export, Type == "Nearshore")) > 0) {
  write_csv(filter(wpt.export, Type == "Nearshore"),  here("Output/tables_updated/waypoints_nearshore.csv"))  
}
# Write transit waypoints
if (nrow(filter(wpt.export, Type == "Transit")) > 0) {
  write_csv(filter(wpt.export, Type == "Transit"),  here("Output/tables_updated/waypoints_transit.csv"))  
}
# Write offshore waypoints
if (nrow(filter(wpt.export, Type == "Offshore")) > 0) {
  write_csv(filter(wpt.export, Type == "Offshore"),  here("Output/tables_updated/waypoints_offshore.csv"))  
}
# Write extra waypoints
if (nrow(filter(wpt.export, Type == "Extra")) > 0) {
  write_csv(filter(wpt.export, Type == "Extra"),  here("Output/tables_updated/waypoints_extra.csv"))  
}

# Export survey plan for survey report -----------------------------------------
wpt.plan <- wpt.export %>% 
  filter(Type %in% c("Adaptive","Compulsory")) %>% 
  select(line = Transect, wpt = Waypoint, lon = Longitude, lat = Latitude, type = Type) %>% 
  # mutate(type = tolower(type)) %>% 
  write_csv(here("Output/tables_updated/waypoint_plan.csv"))

if (nrow(uctds) > 0) {
  # format UCTD stations for export
  uctd.export <- uctds %>% 
    select(Name = name, Latitude, Longitude, Region, Depth)
  
  # export to csv
  # Write UCTD waypoints
  if (nrow(uctd.export) > 0) {
    write_csv(uctd.export,  here("Output/tables_updated/waypoints_uctd.csv"))  
  }  
}

if (nrow(pairovets) > 0) {
  # format Pairovet stations for export
  pairovet.export <- pairovets %>% 
    select(Name = name, Type = type, Latitude, Longitude, Region, Depth)
  
  # export to csv
  # Write Pairovet waypoints
  if (nrow(pairovet.export) > 0) {
    write_csv(pairovet.export,  here("Output/tables_updated/waypoints_pairovet.csv"))  
  }  
}

# Convert stations to sf
uctds.sf     <- st_as_sf(uctds, coords = c("Longitude","Latitude"), crs = crs.geog)
pairovets.sf <- st_as_sf(pairovets, coords = c("Longitude","Latitude"), crs = crs.geog)

# Create the map with all transects --------------------------------------------
survey.map <- base.map +
  # geom_path(data = bathy, aes(long,lat), colour = "gray70", size = 0.25) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Mammal", 
                                                  "Nearshore","Offshore", "Transit", "Saildrone")),
          aes(linetype = Type, colour = Type), show.legend = "line") +
  scale_linetype_manual(name = "Type", values = wpt.linetypes) +
  scale_colour_manual(name = "Type", values = wpt.colors) +
  geom_sf(data = uctds.sf, shape = 21, size = 1, fill = "white") +
  geom_sf(data = pairovets.sf, aes(fill = type), shape = 21, size = 1) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
          xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
          ylim = c(map.bounds["ymin"], map.bounds["ymax"])) 

# Save the map
ggsave(survey.map, filename = here("Figs/fig_survey_map.png"), 
       height = map.height, width = map.width)

# Save results for use with checkTransects.Rmd
save(transects, tx.labels, wpts, uctds, wpt.export, pairovets,
     file = (here("Output/process_transects_output.Rdata")))

# Create the map with all transects --------------------------------------------
survey.map.leg = base.map +
  # geom_path(data = bathy, aes(long,lat), colour = "gray70", size = 0.25) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Offshore", "Nearshore", "Transit", "Saildrone")),
          aes(linetype = Type), colour = "grey50", show.legend = "line") +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory"), !is.na(Leg)),
          aes(linetype = Type, colour = factor(Leg)), show.legend = "line") +
  geom_sf(data = uctds.sf, shape = 21, size = 1, fill = "white") +
  scale_linetype_manual(name = "Type", values = wpt.linetypes) +
  scale_colour_discrete("Leg") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) 

# Save the map
ggsave(survey.map.leg, filename = here("Figs/fig_survey_map_leg.png"), 
       height = map.height, width = map.width)

# Create the map with all transects
survey.map.region = base.map +
  # geom_path(data = bathy, aes(long,lat), colour = "gray70", size = 0.25) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Offshore", "Nearshore", "Transit", "Saildrone")),
          aes(linetype = Type), colour = "grey50", show.legend = FALSE) +
  geom_sf(data = filter(transects.sf, Type %in% c("Adaptive", "Compulsory", "Nearshore", "Offshore", "Saildrone")),
          aes(linetype = Type, colour = factor(Region)), show.legend = "line") +
  geom_sf(data = uctds.sf, shape = 21, size = 1, fill = "white") +
  scale_linetype_manual(name = "Type", values = wpt.linetypes) +
  scale_colour_discrete("Leg") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) 

# Save the map
ggsave(survey.map.region, filename = here("Figs/fig_survey_map_region.png"), 
       height = map.height, width = map.width)  

# Update route files
if (update.routes) {
  # Recombine acoustic transects and sampling stations
  updated.route <- transects %>% 
    bind_rows(uctds) %>% 
    bind_rows(pairovets) %>% 
    mutate(
      Transect = case_when(
        is.na(Transect) ~ transect,
        TRUE ~ as.numeric(Transect)),
      transect.name = str_sub(name, 1, 3),
      transect = case_when(
        Type == "Compulsory" ~ paste0(transect.name, "C"),
        Type == "Adaptive"   ~ paste0(transect.name, "A")),
      id = name)
  
  # Create output directories
  dir_create(here("Output", c("waypoints_updated")))
  dir_create(here("Output/routes_updated"), c("Adaptive","Compulsory","Nearshore","Saildrone"))
  
  # Delete existing CSV files
  rm.csv.routes <- dir_ls(here("Output/routes_updated"), regexp = "*.csv", recurse = TRUE)
  rm.csv.wpts <- dir_ls(here("Output/waypoints_updated"), regexp = "*.csv", recurse = TRUE)
  file_delete(rm.csv.routes)
  file_delete(rm.csv.wpts)
  
  # Write all waypoints to one CSV (to import one route)
  write_csv(select(updated.route, id, lat = Latitude, long = Longitude),
            here("Output/waypoints_updated/transect_wpts.csv"))
  # 
  # write_csv(select(waypoints.final.sd.csv, id, lat, long), 
  #           here("Output/waypoints/transect_wpts_sd.csv"))
  # 
  # write_csv(select(waypoints.final.ns.csv, id, lat, long), 
  #           here("Output/waypoints/transect_wpts_ns.csv"))
  
  # Write waypoints from individual files to multiple CSV to create single routes
  # Compulsory transects
  for (i in unique(updated.route$transect.name)) {
    wpts.sub <- updated.route %>%
      filter(transect.name == i, Type == "Compulsory") %>%
      select(id = name, Latitude, Longitude)
    
    if (nrow(wpts.sub) > 0) {
      write_csv(wpts.sub, here("Output/routes_updated/Compulsory", paste0(i, "C.csv")),
                col_names = FALSE)
    }
  }
  
  # Adaptive transects
  for (i in unique(updated.route$transect.name)) {
    wpts.sub <- updated.route %>%
      filter(transect.name == i, Type == "Adaptive") %>%
      select(id = name, Latitude, Longitude)
    
    if (nrow(wpts.sub) > 0) {
      write_csv(wpts.sub, here("Output/routes_updated/Adaptive", paste0(i, "A.csv")),
                col_names = FALSE)
    }
  }
  
  # Nearshore transects
  for (i in unique(updated.route$transect.name)) {
    wpts.sub <- updated.route %>%
      filter(transect.name == i, Type == "Nearshore") %>%
      select(id = name, Latitude, Longitude)
    
    if (nrow(wpts.sub) > 0) {
      write_csv(wpts.sub, here("Output/routes_updated/Nearshore", paste0(i, "N.csv")),
                col_names = FALSE)
    }
  }
  
  # Saildrone transects
  for (i in unique(updated.route$transect.name)) {
    wpts.sub <- updated.route %>%
      filter(transect.name == i, Type == "Saildrone") %>%
      select(id = name, Latitude, Longitude)
    
    if (nrow(wpts.sub) > 0) {
      write_csv(wpts.sub, here("Output/routes_updated/Saildrone", paste0(i, "S.csv")),
                col_names = FALSE)
    }
  }
  
  # Write UCTD waypoints to CSV file
  uctd.sub <- updated.route %>%
    filter(transect.name == i, Type == "UCTD") %>%
    select(id = name, Latitude, Longitude)
  
  if (nrow(uctd.sub) > 0) {
    write_csv(uctd.sub, here("Output/waypoints_updated/uctd_wpts.csv"),
              col_names = FALSE)
  }
  
  pairovet.sub <- updated.route %>%
    filter(transect.name == i, Type == "Pairovet") %>%
    select(id = name, Latitude, Longitude)
  
  if (nrow(uctd.sub) > 0) {
    write_csv(uctd.sub, here("Output/waypoints_updated/pairovet_wpts.csv"),
              col_names = FALSE)
  }
}

