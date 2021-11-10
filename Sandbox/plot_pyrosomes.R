pyrosomes <- filter(catch.all, species == 159636) %>% 
  left_join(select(haul, haul, lat = startLatDecimal, long = startLongDecimal)) %>% 
  filter(!is.na(lat), !is.na(long)) %>% 
  project_df(to = 3310)

base.map +
geom_point(data = pyrosomes, aes(X, Y, size = TotalWtKg)) + 
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
