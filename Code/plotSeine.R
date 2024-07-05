# Plot purse seine results. This script requires the following to already exist:
#     - Seine data (e.g., processSeine.R)
#     - Basemap
#     - Acoustic data

# Define the latitude (y) and longitude (x) boundary extensions (in meters)
xbuff <- c(300000, 100000)
ybuff <- c(50000, 50000)

# Cycle through purse seine vessels
for (v in seine.vessels) {
  # Get species present in the seine catches of each vessel
  pie.spp.seine.vessel  <- sort(unique(set.catch$scientificName[set.catch$vessel.name == v]))
  
  # Set map image dimensions (inches)
  if (v == "LM") {plot.dim  <- c(7, 4, 1.75)} else {plot.dim <- c(5.5, 8, 1.5)}
  
  # Compute the boundary extents for the current vessel sets
  map.bounds.seine <- set.pie %>%
    filter(str_detect(key.set, v)) %>%                    # Retain sets for current vessel
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%   # Convert to simple feature, specifying the input data uses a GPS coordinate system
    st_transform(crs = 3310) %>%                          # Convert to California Albers coordinate system
    st_bbox()                                             # Return bounding box
  
  # Create plot of pies containing species proportions by weight
  set.pies <- base.map + 
    # Plot purse seine pies
    geom_scatterpie(data = filter(set.pos, str_detect(key.set, v)), 
                    aes(X, Y, group = key.set, r = r*0.8),
                    cols = pie.cols[names(pie.cols) %in% pie.spp.seine.vessel],
                    color = 'black', alpha = 0.8) +
    
    # Configure legend
    scale_fill_manual(name = 'Species',
                      labels = unname(pie.labs[names(pie.labs) %in% pie.spp.seine.vessel]),
                      values = unname(pie.colors[names(pie.colors) %in% pie.spp.seine.vessel])) +
    
    # Plot empty sets as dots
    geom_point(data = filter(set.zero, str_detect(key.set, v)), aes(X, Y),
               shape = 21, fill = 'black', colour = 'white') +
    
    # Configure coordinate system
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = c(map.bounds.seine["xmin"]-xbuff[1], map.bounds.seine["xmax"]+xbuff[2]),
             ylim = c(map.bounds.seine["ymin"]-ybuff[1], map.bounds.seine["ymax"]+ybuff[2]))
  
  # Save plot
  ggsave(set.pies, filename = paste0(here("Figs/fig_seine_proportion_set_wt_"), v, ".png"),
         height = plot.dim[1], width = plot.dim[2])
  
  # Create figure variable specific to current vessel
  assign(paste0("fig.seine.", v), set.pies)
  
  # Select plot levels for backscatter data
  nasc.plot.ns.sub <- filter(nasc.plot.ns, str_detect(transect.name, v))
  
  nasc.levels.all <- sort(unique(nasc.plot.ns.sub$bin.level))
  nasc.labels.all <- nasc.labels[nasc.levels.all]
  nasc.sizes.all  <- nasc.sizes[nasc.levels.all]
  nasc.colors.all <- nasc.colors[nasc.levels.all]
  
  # Map backscatter
  nasc.map.ns <- base.map +
    # Plot NASC data
    geom_path(data = nasc.plot.ns.sub, 
              aes(X, Y, group = transect.name),
              colour = "gray50", linewidth = 0.5, alpha = 0.5) +
    # Plot NASC data
    geom_point(data = nasc.plot.ns.sub, aes(X, Y, size = bin, fill = bin),
               shape = 21, alpha = 0.75) +
    # Configure size and colour scales
    scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                      values = nasc.sizes.all,labels = nasc.labels.all) +
    scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                      values = nasc.colors.all,labels = nasc.labels.all) +
    # Configure legend guides
    guides(fill = guide_legend(), size = guide_legend()) +
    # Plot title
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = c(map.bounds.seine["xmin"]-xbuff[1], map.bounds.seine["xmax"]+xbuff[2]),
             ylim = c(map.bounds.seine["ymin"]-ybuff[1], map.bounds.seine["ymax"]+ybuff[2]))
  
  ggsave(nasc.map.ns, filename = here("Figs", paste0("fig_backscatter_cps_", v, ".png")),
         height = plot.dim[1], width = plot.dim[2])
  
  # Combine purse seine and backscatter data for each vessel
  nasc.set.wt.combo <- cowplot::plot_grid(nasc.map.ns, set.pies, nrow = 1, align = "hv",
                                          labels = c("a)", "b)"))
  
  # Save combined figure
  ggsave(nasc.set.wt.combo, filename = here("Figs", paste0("fig_nasc_seine_proportion_set_wt_", v, ".png")),
         height = plot.dim[1], width = plot.dim[2]*plot.dim[3])
}

#### Plot the vessels combined

# Compute the boundary extents for all vessel sets
map.bounds.seine <- set.pie %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%   # Convert to simple feature, specifying the input data uses a GPS coordinate system
  st_transform(crs = 3310) %>%                          # Convert to California Albers coordinate system
  st_bbox()                                             # Return bounding box

# Get species present in the seine catches from all vessels
pie.spp.seine  <- sort(unique(set.catch$scientificName))

# Create plot of pies containing species proportions by weight
fig.seine.combined <- base.map + 
  
  # Plot purse seine pies
  geom_scatterpie(data = set.pos, 
                  aes(X, Y, group = key.set, r = r*0.8),
                  cols = pie.cols[names(pie.cols) %in% pie.spp.seine],
                  color = 'black', alpha = 0.8) +
  
  # Configure legend
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp.seine]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp.seine])) +
  
  # Plot empty sets as dots
  geom_point(data = set.zero, aes(X, Y)) +
  
  # Configure coordinate system
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.seine["xmin"]-xbuff[1], map.bounds.seine["xmax"]+xbuff[2]),
           ylim = c(map.bounds.seine["ymin"]-ybuff[1], map.bounds.seine["ymax"]+ybuff[2]))

# Save plot
ggsave(fig.seine.combined, filename = here("Figs/fig_seine_proportion_set_wt_combined.png"),
       height = 10, width = 6)
