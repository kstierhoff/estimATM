# Convert NASC to lines
nasc.lines <- nasc %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(transect.name) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING")

nasc.lines.sd <- nasc.nearshore %>%
  filter(vessel.name == "SD") %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(transect.name) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING")

# Get intervals closest to shore
nasc.comp <- nasc %>% 
  group_by(transect.name) %>% 
  summarise(lat  = lat[which.max(long)],
            long = max(long)) %>% 
  ungroup()

nasc.comp.sd <- nasc.nearshore %>%
  filter(vessel.name == "SD") %>% 
  group_by(transect.name) %>% 
  summarise(lat  = lat[which.max(long)],
            long = max(long)) %>% 
  ungroup()

# Convert to sf
nasc.comp.sf <- nasc.comp %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

nasc.comp.sd.sf <- nasc.comp.sd %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

# Find nearest Saildron point to Lasker
nasc.comp.sf$nearest.tx <- nasc.comp.sd$transect.name[st_nearest_feature(nasc.comp.sf, nasc.comp.sd.sf)]

# Add Saildrone transect to Lasker data
nasc.comp <- nasc.comp %>% 
  left_join(nasc.comp.sf) %>% 
  select(-geometry) %>% 
  mutate(transect = as.numeric(str_replace(transect.name, "RL ", ""))) %>% 
  filter(between(transect, 20, 212))

# Extract depth for both
nasc.comp$depth <- get.depth(noaa.bathy, 
          nasc.comp$long, 
          nasc.comp$lat, 
          locator = F, distance = F)$depth

nasc.comp.sd$depth <- get.depth(noaa.bathy, 
          nasc.comp.sd$long, 
          nasc.comp.sd$lat, 
          locator = F, distance = F)$depth 

ggplot() +
  geom_sf(data = nasc.lines) +
  geom_point(data = nasc.comp, aes(long, lat), shape = 21, fill = "white") 
  # geom_text(data = nasc.comp, aes(long, lat, label = transect)) 

mapview(nasc.lines) + mapview(nasc.comp.sf)

mapview(nasc.lines.sd) + mapview(nasc.comp.sd.sf)

save(nasc.comp, nasc.comp.sd,
     file = here::here("Output/nasc_comparison_nearshore.Rdata"))
  
