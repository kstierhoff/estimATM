# Map Long Beach Carnage and Lisa Marie transects for Summer 2022 CCE Survey (2207RL)
lbc.transects <- c(1:90, 334:397)
lm.transects  <- c(91:216)

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
  # geom_sf(data = ca_mpas, aes(fill = Type), alpha = 0.5) +
  geom_sf(data = filter(transects, Type != "Nearshore"),
          aes(colour = Type, linetype = Type),
          show.legend = "line") +
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
                      values = wpt.colors) +
  scale_linetype_manual("Type", 
                        values = wpt.linetypes) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save the base map
ggsave(lbc.map, file = here("Figs", paste0("fig_survey_plan_LBC.png")), 
       height = map.height.region, width = map.width) 

# Map Lisa Marie transects for Summer 2023 CCE Survey (2207RL)
# Set padding around data  
map.bounds <- transects %>%
  filter(Type == "Compulsory", Transect %in% c(77, 141)) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox()

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- 5
# map.width  <- map.height.region*map.aspect

# Create base map
base.map <- get_basemap(filter(transects, loc == ii), states, countries, 
                        landmarks, bathy, map.bounds, crs = crs.proj)

lm.map <- base.map +
  geom_sf(data = filter(transects, Type != "Nearshore"),
          aes(colour = Type, linetype = Type),
          show.legend = "line") +
  geom_sf(data = filter(transects, Type == "Nearshore", Transect %in% lm.transects), 
          aes(colour = Type, linetype = Type), 
          show.legend = "line") +
  # # Plot acoustic transect labels N of Cape Flattery
  # geom_shadowtext(data = tx.labels,
  #                 aes(X, Y, label = transect.name,
  #                     angle = brg, colour = Type),
  #                 size = 2, fontface = 'bold.italic',
  #                 bg.colour = "white") +
  scale_colour_manual("Type", 
                      values = wpt.colors) +
  scale_linetype_manual("Type", 
                        values = wpt.linetypes) +
  theme(legend.position      = c(1,0.5),
        legend.justification = c(0,0.5)) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save the base map
ggsave(lm.map, file = here("Figs", paste0("fig_survey_plan_LM.png")), 
       height = map.height.region, width = map.width) 

# Set padding around data  
map.bounds <- transects %>%
  filter(Type == "Saildrone") %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox()  

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.width  <- 4

# Create base map
base.map <- get_basemap(filter(transects, Type == "Saildrone"), states, countries, 
                        landmarks, bathy, map.bounds, crs = crs.proj)

saildrone.map <- base.map +
  geom_sf(data = filter(transects, Type != "Saildrone"),
          aes(colour = Type, linetype = Type),
          show.legend = "line") +
  geom_sf(data = filter(transects, Type == "Saildrone"), 
          aes(colour = Type, linetype = Type), 
          show.legend = "line") +
  scale_colour_manual("Type", 
                      values = wpt.colors) +
  scale_linetype_manual("Type", 
                        values = wpt.linetypes) +
  # theme(legend.position      = c(1,0.5),
  #       legend.justification = c(0,0.5)) +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save the base map
ggsave(saildrone.map, file = here("Figs", paste0("fig_survey_plan_saildrone.png")), 
       height = map.height.region, width = map.width) 
