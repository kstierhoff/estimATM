# Plot purse seine results. This script requires the following to already exist:
#     - Seine data (e.g., processSeine.R)
#     - Basemap
#     - Acoustic data

# Define the latitude (y) and longitude (x) boundary extensions (in meters)
xbuff <- c(300000, 100000)
ybuff <- c(50000, 50000)

# Cycle through purse seine vessels
for (v in seine.vessels) {
  
  # Compute the boundary extents for the current vessel sets
  map.bounds.seine <- set.pie %>%
    filter(str_detect(key.set, v)) %>%                    # Retain sets for current vessel
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%   # Convert to simple feature, specifying the input data uses a GPS coordinate system
    st_transform(crs = 3310) %>%                          # Convert to California Albers coordinate system
    st_bbox()                                             # Return bounding box
  
  # Create plot of pies containing species proportions by weight
  set.pies <- base.map + 
    
    # Plot purse seine pies
    scatterpie::geom_scatterpie(data = filter(set.pos, str_detect(key.set, v)), 
                                aes(X, Y, group = key.set, r = r*0.8),
                                cols = c("Anchovy", "JackMack", "Jacksmelt",
                                         "PacHerring", "PacMack", "RndHerring","Sardine"),
                                color = 'black', alpha = 0.8) +
    
    # Configure legend
    scale_fill_manual(name = 'Species',
                      labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                                 "P. Herring", "P. Mackerel", "R. Herrinng", "Sardine"),
                      values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                                 pac.herring.color, pac.mack.color, rnd.herring.color, sardine.color)) +
    
    # Plot empty sets as dots
    geom_point(data = filter(set.zero, str_detect(key.set, v)), aes(X, Y)) +
    
    # Configure coordinate system
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = c(map.bounds.seine["xmin"]-xbuff[1], map.bounds.seine["xmax"]+xbuff[2]),
             ylim = c(map.bounds.seine["ymin"]-ybuff[1], map.bounds.seine["ymax"]+ybuff[2]))
  
  # Save plot
  ggsave(set.pies, filename = paste0(here("Figs/fig_seine_proportion_set_wt_"), v, ".png"),
         height = 10, width = 6)
  
  # Create figure variable specific to current vessel
  assign(paste0("fig.seine.", v), set.pies)
}

#### Plot the vessels combined

# Compute the boundary extents for all vessel sets
map.bounds.seine <- set.pie %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%   # Convert to simple feature, specifying the input data uses a GPS coordinate system
  st_transform(crs = 3310) %>%                          # Convert to California Albers coordinate system
  st_bbox()                                             # Return bounding box

# Create plot of pies containing species proportions by weight
fig.seine.combined <- base.map + 
  
  # Plot purse seine pies
  scatterpie::geom_scatterpie(data = set.pos, 
                              aes(X, Y, group = key.set, r = r*0.8),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "RndHerring","Sardine"),
                              color = 'black', alpha = 0.8) +
  
  # Configure legend
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. Herring", "P. Mackerel", "R. Herrinng", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, rnd.herring.color, sardine.color)) +
  
  # Plot empty sets as dots
  geom_point(data = set.zero, aes(X, Y)) +
  
  # Configure coordinate system
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.seine["xmin"]-xbuff[1], map.bounds.seine["xmax"]+xbuff[2]),
           ylim = c(map.bounds.seine["ymin"]-ybuff[1], map.bounds.seine["ymax"]+ybuff[2]))

# Save plot
ggsave(fig.seine.combined, filename = here("Figs/fig_seine_proportion_set_wt_combined.png"),
       height = 10, width = 6)
