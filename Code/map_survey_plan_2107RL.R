# Map Long Beach Carnage transects for Summer 2021 CCE Survey (2107RL)
lbc.transects <- c(138:228, 357:378)

# Set padding around data  
map.bounds <- transects %>%
  filter(Type == "Nearshore", Transect %in% lbc.transects) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox()  

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- map.height.region*map.aspect

# Create base map
base.map <- get_basemap(filter(transects, loc == ii), states, countries, 
                        landmarks, bathy, map.bounds, crs = crs.proj)

lbc.map <- base.map +
  # geom_sf(data = si.mpa, colour = "green", fill = NA, linetype = "dashed") +
  # geom_sf(data = ca_mpas, aes(fill = Type), alpha = 0.5) +
  geom_sf(data = filter(transects, Type != "Nearshore"),  
          colour = "gray50", alpha = 0.5, show.legend = "line") +
  geom_sf(data = filter(transects, Type == "Nearshore", Transect %in% lbc.transects), 
          aes(colour = Type, linetype = Type), 
          show.legend = "line") +
  # # Plot acoustic transect labels N of Cape Flattery
  # geom_shadowtext(data = tx.labels,
  #                 aes(X, Y, label = transect.name,
  #                     angle = brg, colour = Type),
  #                 size = 2, fontface = 'bold.italic',
  #                 bg.colour = "white") +
  scale_colour_manual("Type", 
                      values = c(Adaptive = "red", Compulsory = "black",
                                 Offshore = "green", Nearshore = "magenta",
                                 Transit = "orange", Saildrone = "cyan")) +
  scale_linetype_manual("Type", 
                        values = c(Adaptive = "dashed", Compulsory = "solid",
                                   Offshore = "dashed", Nearshore = "solid",
                                   Transit = "dashed", Saildrone = "solid")) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save the base map
ggsave(lbc.map, file = here("Figs", paste0("fig_survey_plan_LBC.png")), 
       height = map.height.region, width = map.width) 

# Map Lisa Marie transects for Summer 2021 CCE Survey (2107RL)
# Set padding around data  
map.bounds <- transects %>%
  filter(Type == "Compulsory", Transect %in% c(65, 158)) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox()

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- 4
# map.width  <- map.height.region*map.aspect

# Create base map
base.map <- get_basemap(filter(transects, loc == ii), states, countries, 
                        landmarks, bathy, map.bounds, crs = crs.proj)

lm.map <- base.map +
  # geom_sf(data = si.mpa, colour = "green", fill = NA, linetype = "dashed") +
  # geom_sf(data = ca_mpas, aes(fill = Type), alpha = 0.5) +
  geom_sf(data = filter(transects, Type != "Nearshore"), 
          colour = "gray50", alpha = 0.5, show.legend = "line") +
  geom_sf(data = filter(transects, Type == "Nearshore", !Transect %in% lbc.transects), 
          aes(colour = Type, linetype = Type), 
          show.legend = "line") +
  # # Plot acoustic transect labels N of Cape Flattery
  # geom_shadowtext(data = tx.labels,
  #                 aes(X, Y, label = transect.name,
  #                     angle = brg, colour = Type),
  #                 size = 2, fontface = 'bold.italic',
  #                 bg.colour = "white") +
  scale_colour_manual("Type", 
                      values = c(Adaptive = "red", Compulsory = "black",
                                 Offshore = "green", Nearshore = "magenta",
                                 Transit = "orange", Saildrone = "cyan")) +
  scale_linetype_manual("Type", 
                        values = c(Adaptive = "dashed", Compulsory = "solid",
                                   Offshore = "dashed", Nearshore = "solid",
                                   Transit = "dashed", Saildrone = "solid")) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save the base map
ggsave(lm.map, file = here("Figs", paste0("fig_survey_plan_LM.png")), 
       height = map.height.region, width = map.width) 

# Map Saildrone transects for Summer 2021 CCE Survey (2107RL)
# Set padding around data  
map.bounds <- transects %>%
  filter(Type == "Saildrone") %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox()  

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- 4

# Create base map
base.map <- get_basemap(filter(transects, loc == ii), states, countries, 
                        landmarks, bathy, map.bounds, crs = crs.proj)

saildrone.map <- base.map +
  # geom_sf(data = si.mpa, colour = "green", fill = NA, linetype = "dashed") +
  # geom_sf(data = ca_mpas, aes(fill = Type), alpha = 0.5) +
  geom_sf(data = filter(transects, Type != "Saildrone"),  
          colour = "gray50", alpha = 0.5, show.legend = "line") +
  geom_sf(data = filter(transects, Type == "Saildrone"), 
          aes(colour = Type, linetype = Type), 
          show.legend = "line") +
  # # Plot acoustic transect labels N of Cape Flattery
  # geom_shadowtext(data = tx.labels,
  #                 aes(X, Y, label = transect.name,
  #                     angle = brg, colour = Type),
  #                 size = 2, fontface = 'bold.italic',
  #                 bg.colour = "white") +
  scale_colour_manual("Type", 
                      values = c(Adaptive = "red", Compulsory = "black",
                                 Offshore = "green", Nearshore = "magenta",
                                 Transit = "orange", Saildrone = "cyan")) +
  scale_linetype_manual("Type", 
                        values = c(Adaptive = "dashed", Compulsory = "solid",
                                   Offshore = "dashed", Nearshore = "solid",
                                   Transit = "dashed", Saildrone = "solid")) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save the base map
ggsave(saildrone.map, file = here("Figs", paste0("fig_survey_plan_saildrone.png")), 
       height = map.height.region, width = map.width) 
