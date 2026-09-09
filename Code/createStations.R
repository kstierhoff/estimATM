# Load bathymetry contours for creating eE stations
bathy.ctd <- st_read(here("Data/GIS/bathy_us_wc_50m.shp"))
transects.ctd <- transects %>% 
  filter(Type %in% c("Adaptive","Compulsory"))

# Buffer transects to get transect info for eDNA points
transects.buffer <- transects %>% 
  filter(Type %in% c("Compulsory","Adaptive")) %>%
  st_transform(crs.proj) %>%
  st_buffer(units::set_units(1 * 1852, m)) %>% 
  select(Type, Transect) %>% 
  st_transform(crs.geog)

if (nrow(ctd) == 0) {
  # Create eE stations as intersection between transects and isobaths
  ctd.sf <- st_intersection(st_transform(bathy.ctd, crs.geog), 
                         st_transform(transects.ctd, crs.geog)) %>% 
    st_cast("POINT") %>% 
    mutate(Transect = sprintf("%03d", Transect),
           depth = abs(CONTOUR),
           name = paste0("CTD", Transect,"_", sprintf("%04d", depth)),
           Longitude = as.data.frame(st_coordinates(.))$X,
           Latitude  = as.data.frame(st_coordinates(.))$Y) %>% 
    select(Transect, name, Region, Longitude, Latitude, depth, CONTOUR) %>% 
    filter(as.numeric(Transect) %in% ctd.tx.range)
  
  # Get depth values
  ctd.sf$Depth <- round(get.depth(noaa.bathy, ctd.sf$Longitude, ctd.sf$Latitude, 
                               locator = FALSE, distance = TRUE)$depth)
  
  # Write waypoints to CSV
  ctd.sf %>%
    st_set_geometry(NULL) %>% 
    select(name, Latitude, Longitude) %>% 
    write_csv(here("Output/waypoints_updated/waypoints_ctd.csv"), col_names = FALSE)
  
  # Write waypoints to GPX
  ctd.sf %>%
    select(name) %>% 
    st_write(dsn = here("Output/waypoints_updated/waypoints_ctd.gpx"), driver = "GPX", delete_layer = TRUE)
  
  # Create data frame
  ctd <- ctd.sf %>%
    st_set_geometry(NULL)
}

if (nrow(eDNA) == 0) {
  # Get first waypoint for each transect
  eDNA.starts <- wpts %>% 
    filter(Type %in% c("Compulsory","Adaptive"),
           Transect %in% edna.tx.range) %>%
    group_by(Transect) %>% 
    slice(1) %>% 
    mutate(id = 0) %>% 
    select(Transect, Type, Longitude = long, Latitude = lat, id)
  
  # Create stations along each transect
  eDNA.sf <- transects %>%
    filter(Type %in% c("Compulsory","Adaptive"),
           Transect %in% edna.tx.range) %>%
    st_transform(crs.proj) %>%
    st_line_sample(density = 1/units::set_units(edna.spacing * 1852, m)) %>%
    st_cast("POINT") %>% st_as_sf() %>% st_transform(crs.geog) %>% 
    mutate(
      Longitude = as.data.frame(st_coordinates(.))$X,
      Latitude  = as.data.frame(st_coordinates(.))$Y) %>% 
    rename(geometry = x)
  
  # Add transect info to station waypoints and create names
  eDNA.sf <- eDNA.sf %>%
    st_intersection(transects.buffer) %>% 
    mutate(id = seq_along(Transect)) %>% 
    bind_rows(eDNA.starts) %>% 
    arrange(Transect, id) %>% 
    group_by(Transect) %>% 
    mutate(Station = rank(id)) %>% 
    ungroup() %>% 
    mutate(Transect = sprintf("%03d", Transect),
           name = paste0("eDNA", Transect,"_", sprintf("%03d", Station), "S")) %>% 
    select(Transect, name, Type, Longitude, Latitude, everything())
  
  # Get depth values
  eDNA.sf$Depth <- round(get.depth(noaa.bathy, eDNA.sf$Longitude, eDNA.sf$Latitude, 
                                locator = FALSE, distance = TRUE)$depth)
  
  # Write waypoints to CSV
  eDNA.out <- eDNA.sf %>%
    st_set_geometry(NULL) %>% 
    select(name, Latitude, Longitude) %>% 
    write_csv(here("Output/waypoints_updated/waypoints_eDNA.csv"), col_names = FALSE)
  
  # Write waypoints to GPX
  eDNA.sf %>%
    select(name) %>% 
    st_write(dsn = here("Output/waypoints_updated/waypoints_eDNA.gpx"), driver = "GPX", delete_layer = TRUE)
  
  # Create data frame
  eDNA <- eDNA.sf %>%
    st_set_geometry(NULL)
}

if (nrow(uctd) == 0) {
  uctd.sf <- transects %>%
    filter(Type %in% c("Compulsory","Adaptive"),
           Transect %in% uctd.tx.range) %>%
    st_transform(crs.proj) %>%
    st_line_sample(density = 1/units::set_units(uctd.spacing * 1852, m)) %>%
    st_cast("POINT") %>% st_as_sf() %>% st_transform(crs.geog) %>% 
    mutate(
      Longitude = as.data.frame(st_coordinates(.))$X,
      Latitude  = as.data.frame(st_coordinates(.))$Y)
  
  # Add transect info to station waypoints and create names
  uctd.sf <- uctd.sf %>%
    st_intersection(transects.buffer) %>% 
    mutate(id = seq_along(Transect)) %>% 
    group_by(Transect) %>% 
    mutate(Station = rank(id)) %>% 
    ungroup() %>% 
    mutate(Transect = sprintf("%03d", Transect),
           name = paste0("UCTD", Transect,"_", sprintf("%03d", Station))) %>% 
    select(Transect, name, Type, Longitude, Latitude, everything())
  
  # Get depth values
  uctd.sf$Depth <- round(get.depth(noaa.bathy, uctd.sf$Longitude, uctd.sf$Latitude, 
                                locator = FALSE, distance = TRUE)$depth)
  # Write waypoints to CSV
  uctd.sf %>%
    st_set_geometry(NULL) %>% 
    select(name, Latitude, Longitude) %>% 
    write_csv(here("Output/waypoints_updated/waypoints_uctd.csv"), col_names = FALSE)
  
  # Write waypoints to GPX
  uctd.sf %>%
    select(name) %>% 
    st_write(dsn = here("Output/waypoints_updated/waypoints_uctd.gpx"), driver = "GPX", delete_layer = TRUE)
  
  # Create data frame
  uctd <- uctd.sf %>%
    st_set_geometry(NULL)
}
