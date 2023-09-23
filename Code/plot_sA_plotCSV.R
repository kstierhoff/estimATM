# Plot sA for CPS #####
# Select plot levels for backscatter data
nasc.levels.all <- unique(nasc.plot$bin.level)
nasc.labels.all <- nasc.labels[sort(nasc.levels.all)]
nasc.sizes.all  <- nasc.sizes[sort(nasc.levels.all)]
nasc.colors.all <- nasc.colors[sort(nasc.levels.all)]

# Map backscatter
nasc.map <- base.map +
  # # Plot transects data
  # geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
  #         alpha = 0.75, linetype = "dashed") +
  # # plot ship track data
  # geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # # Plot NASC data
  geom_point(data = nasc.plot, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and color scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Plot title
  # ggtitle("CPS Backscatter") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

# Save backscatter plot
ggsave(nasc.map,
       filename = here("Figs", img.filename),
       width = map.width, height = map.height)
