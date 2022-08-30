nasc.paths <- nasc %>% 
  # group_by(transect) %>% 
  # arrange(long) %>% 
  # ungroup() %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(vessel.orig, transect.name) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup()

mapview(nasc.paths, zcol = "transect.name")

ggplot(filter(nasc, vessel.orig == "SD"), aes(long, lat, group = transect, colour = vessel.orig)) + 
  geom_path() + 
  coord_map()

