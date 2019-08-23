# NASC Plot #####
# Select plot levels for backscatter data
nasc.levels.all <- unique(nasc.plot.cps$bin.level)
nasc.labels.all <- nasc.labels[sort(nasc.levels.all)]
nasc.sizes.all  <- nasc.sizes[sort(nasc.levels.all)]
nasc.colors.all <- nasc.colors[sort(nasc.levels.all)]

# Map backscatter
nasc.map.cps <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.plot.cps, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Plot title
  ggtitle("CPS Backscatter") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

# Save nasc plot
ggsave(nasc.map.cps,
       filename = here("Figs/fig_backscatter_cps.png"),
       width = map.width, height = map.height) 

save(nasc.map.cps, file = here("Output/nasc_plot_cps.Rdata"))

# Create plot for Krill NASC
# Select plot levels for backscatter data
nasc.levels.all <- unique(nasc.plot.krill$bin.level)
nasc.labels.all <- nasc.labels[sort(nasc.levels.all)]
nasc.sizes.all  <- nasc.sizes[sort(nasc.levels.all)]
nasc.colors.all <- nasc.colors[sort(nasc.levels.all)]

# Map backscatter
nasc.map.krill <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.plot.krill, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Plot title
  ggtitle("Krill Backscatter") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

# Save nasc plot
ggsave(nasc.map.krill,
       filename = here("Figs/fig_backscatter_krill.png"),
       width = map.width, height = map.height) 

save(nasc.map.krill, file = here("Output/nasc_plot_krill.Rdata"))

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

# Map trawl species proportions -------------------------------------------------------
if (nrow(cluster.pos) > 0) {
  # Create trawl figure
  trawl.catch.plot <- base.map +
    # Plot transects data
    geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # plot ship track data
    geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = cluster.pos, aes(X, Y, group = cluster, r = pie.radius),
                    cols = c("Anchovy","JackMack","Jacksmelt","PacHerring","PacMack","Sardine"),
                    color = 'black', alpha = 0.8) +
    # Plot empty trawl locations
    geom_point(data = cluster.zero, aes(X, Y),
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # Configure trawl scale
    scale_fill_manual(name = 'Species',
                      labels = c("Anchovy","J. Mackerel","Jacksmelt","P. herring","P. mackerel","Sardine"),
                      values = c(anchovy.color,jack.mack.color,jacksmelt.color,
                                 pac.herring.color,pac.mack.color,sardine.color)) +
    # Plot panel label
    ggtitle("CPS Proportions in Trawl Clusters") +
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
} else {
  # Create trawl figure
  trawl.catch.plot <- base.map +
    # Plot transects data
    geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # Plot empty trawl locations
    geom_point(data = cluster.zero, aes(X, Y), 
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # Plot panel label
    ggtitle("CPS Proportions in Trawl Clusters") +
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
}

# save trawl plot
ggsave(trawl.catch.plot,
       filename = here("Figs/fig_trawl_proportion_cluster_wt.png"),
       width = map.width, height = map.height)

# Combine all plots #####
nasc.cufes.trawl.plot <- plot_grid(nasc.map.cps, cufes.density.all, trawl.catch.plot, 
                                   nrow = 1, labels = c("a)", "b)", "c)")) 

# Save composite plot
ggsave(nasc.cufes.trawl.plot,
       filename = here("Figs/fig_nasc_cufes_trawl.png"),
       width = map.width*3, height = map.height)
