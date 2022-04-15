# Draw survey polygons ----------------------------------------------------------
# Process data from each vessel
for (j in unique(nasc$vessel.name)) {
  # Get transect ends, calculate bearing, and add transect spacing
  tx.ends <- filter(nasc, vessel.name == j) %>% 
                      group_by(transect, transect.name, vessel.name) %>% 
                      summarise(
                        lat.i  = lat[which.max(long)],
                        long.i = max(long),
                        lat.o  = lat[which.min(long)],
                        long.o = min(long)
                      ) %>% 
                      # left_join(select(tx.nn, transect.name, spacing = min.dist)) %>% 
                      mutate(
                        brg = swfscMisc::bearing(lat.i, long.i,
                                                 lat.o, long.o)[1]) %>% 
    # filter(!transect.name %in% c("RL 595","RL 900")) %>% 
    ungroup()
  
  # Get original inshore transect ends -------------------------------------------
  # Select original inshore waypoints
  tx.i <- tx.ends %>% 
    select(-lat.o, -long.o) %>% 
    rename(lat = lat.i, long = long.i) %>% 
    mutate(
      grp = "original",
      loc = "inshore",
      order = 2) %>% 
    arrange(transect, desc(order))
  
  # Get original offshore transect ends ------------------------------------------
  tx.o <- tx.ends %>% 
    select(-lat.i, -long.i) %>% 
    rename(lat = lat.o, long = long.o) %>% 
    mutate(
      grp = "original",
      loc = "offshore",
      order = 2) %>% 
    arrange(desc(transect), desc(order))
  
  # Assemble the final data frame with all waypoints -----------------------------
  strata.points <- tx.i %>% 
    bind_rows(tx.o)   %>%
    mutate(key = paste(transect.name, grp)) 
  
  # Convert to points
  strata.points.sf <- st_as_sf(strata.points, coords = c("long","lat"), crs = 4326) 
  
  # Create polygons
  tmp.polygon <- strata.points.sf %>% 
    group_by(vessel.name) %>% 
    summarise(do_union = F) %>% 
    st_cast("POLYGON") %>% 
    st_make_valid() %>% 
    st_difference(st_union(bathy_20m_poly)) %>% 
    mutate(area = st_area(.))
  
  if (exists("survey.polygons")) {
    # Append polygon object
    survey.polygons <- rbind(survey.polygons, tmp.polygon)
  } else {
    survey.polygons <- tmp.polygon
  }
}

# Make polygons valid
survey.polygons <- st_make_valid(survey.polygons)
