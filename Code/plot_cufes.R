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
  ggtitle("CUFES Egg Densities") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

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
  ggtitle("CUFES Egg Densities") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

# Save cufes plot
ggsave(cufes.density.all,
       filename = here("Figs/fig_cufes_egg_density.png"),
       width = map.width, height = map.height)

# Save cufes plot
ggsave(cufes.density.facet,
       filename = here("Figs/fig_cufes_egg_density_facet.png"),
       width = map.width*3, height = map.height)
