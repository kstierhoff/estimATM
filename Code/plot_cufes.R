# CUFES Plots #####
# Plot CUFES for sardine and anchovy -------------------------------------------------------  
# Define plot levels for all species
cufes.levels.all     <- cufes.plot %>% distinct(bin.level)
cufes.labels.all     <- cufes.labels[sort(cufes.levels.all$bin.level)]
cufes.sizes.all      <- cufes.sizes[sort(cufes.levels.all$bin.level)]
cufes.colors.all     <- cufes.colors[which(names(cufes.colors) %in% unique(cufes.plot$Species))]
cufes.spp.labels.all <- cufes.spp.labels[which(names(cufes.spp.labels) %in% unique(cufes.plot$Species))]

cufes.density.all <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # Plot all cufes samples, including zeros
  geom_point(data = cufes.neg, aes(X, Y),
             shape = 3, size = 0.5, colour = 'black', alpha = 0.5) +
  # Plot only positive cufes samples
  geom_point(data = cufes.plot,
             aes(X, Y, size = bin, colour = Species), alpha = 0.6) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop('Density','(eggs' ~m^-3*')')),
                    values = cufes.sizes.all,labels = cufes.labels.all) +
  scale_colour_manual(name = 'Species', values = cufes.colors.all, labels = cufes.spp.labels.all) +
  # Format axes and titles
  # ggtitle("CUFES Egg Densities") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

cufes.density.facet <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # Plot all cufes samples, including zeros
  geom_point(data = cufes.neg, aes(X, Y),
             shape = 3, size = 0.5, colour = 'black', alpha = 0.5) +
  # Plot only positive cufes samples
  geom_point(data = cufes.plot,
             aes(X, Y, size = bin, colour = Species), alpha = 0.6) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop('Density','(eggs' ~m^-3*')')),
                    values = cufes.sizes.all,labels = cufes.labels.all) +
  scale_colour_manual(name = 'Species', values = cufes.colors.all, labels = cufes.spp.labels.all) +
  facet_wrap(~Species) +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold")) +
  # Format axes and titles
  # ggtitle("CUFES Egg Densities") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save CUFES plot
ggsave(cufes.density.all,
       filename = here("Figs/fig_cufes_egg_density.png"),
       width = map.width, height = map.height)

# Save CUFES plot
ggsave(cufes.density.facet,
       filename = here("Figs/fig_cufes_egg_density_facet.png"),
       width = map.width*3, height = map.height)

if (doc.name == "simulateBiomass.Rmd") {
  # Save nasc plot
  ggsave(cufes.density.all,
         filename = here("Simulation/Figs/fig_cufes_egg_density.png"),
         width = map.width, height = map.height) 
  
  ggsave(cufes.density.facet,
         filename = here("Simulation/Figs/fig_cufes_egg_density_facet.png"),
         width = map.width, height = map.height) 
  
} else {
  # Save nasc plot
  ggsave(cufes.density.all,
         filename = here("Figs/fig_cufes_egg_density.png"),
         width = map.width, height = map.height) 
  
  ggsave(cufes.density.facet,
         filename = here("Figs/fig_cufes_egg_density_facet.png"),
         width = map.width*3, height = map.height) 
}

# Project nav for plotting
nav.cufes <- project_df(nav, to = 3310) 
nav.cufes <- nav.cufes[seq(1, nrow(nav.cufes), nrow(nav.cufes)*0.005), ]

if (!is.null(nav.cufes$wind_angle) & !is.null(nav.cufes$wind_speed)) {
  cufes.density.wind <- base.map +
    # Plot nav path
    geom_path(data = nav.cufes,
              aes(X, Y), size = 0.1) +
    # Plot wind vectors
    geom_spoke(data = nav.cufes,
               aes(X, Y, angle = wind_angle, radius = wind_speed*2000),
               alpha = 1, size = 0.1) +
    # Plot transects data
    geom_sf(data = transects.sf, size = 0.1, colour = "gray70",
            alpha = 0.75) +
    # Plot all cufes samples, including zeros
    geom_point(data = cufes.neg, aes(X, Y),
               shape = 3, size = 0.5, colour = 'black', alpha = 0.5) +
    # Plot only positive cufes samples
    geom_point(data = cufes.plot,
               aes(X, Y, size = bin, colour = Species), alpha = 0.6) +
    # Configure size and colour scales
    scale_size_manual(name = bquote(atop('Density','(eggs' ~m^-3*')')),
                      values = cufes.sizes.all,labels = cufes.labels.all) +
    scale_colour_manual(name = 'Species', values = cufes.colors.all, labels = cufes.spp.labels.all) +
    # Format axes and titles
    # ggtitle("CUFES Egg Densities") +
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))  
    
    ggsave(cufes.density.wind,
           filename = here("Figs/fig_cufes_egg_density_wind.png"),
           width = map.width, height = map.height)
}
