# Map trawl species proportions -------------------------------------------------------
if (nrow(haul.pos) > 0) {
  # Create trawl haul figure
  trawl.pie.haul.wt <- base.map +
    # Plot transects data
    geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # plot ship track data
    geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = haul.pos, aes(X, Y, group = haul, r = r),
                    cols = c("Anchovy", "JackMack", "Jacksmelt",
                             "PacHerring", "PacMack", "RndHerring", "Sardine"),
                    color = 'black', alpha = 0.8, sorted_by_radius = TRUE) +
    # Configure trawl scale
    scale_fill_manual(name = 'Species',
                      labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                                 "P. herring", "P. mackerel", "R. herring", "Sardine"),
                      values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                                 pac.herring.color, pac.mack.color, rnd.herring.color, 
                                 sardine.color)) +
    # Plot empty cluster locations
    geom_point(data = haul.zero, aes(X, Y),
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # geom_scatterpie_legend(radius = trawl.breaks, x = -50000, y = -600000) + 
    coord_sf(crs = crs.proj, 
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
  
} else {
  # Create trawl figure
  trawl.pie.haul.wt <- base.map +
    # Plot transects data
    geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # Plot empty trawl locations
    geom_point(data = haul.zero, aes(X, Y), 
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    coord_sf(crs = crs.proj, 
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
}

if (doc.name == "simulateBiomass.Rmd") {
  # Save nasc plot
  ggsave(trawl.pie.haul.wt,
         filename = here("Simulation/Figs/fig_trawl_proportion_haul_wt.png"),
         width = map.width, height = map.height) 
} else if (doc.name == "reportMexico.Rmd") {
  # Save nasc plot
  ggsave(trawl.pie.haul.wt,
         filename = here("Figs/fig_trawl_proportion_haul_wt_mx.png"),
         width = map.width, height = map.height) 
} else {
  # Save nasc plot
  ggsave(trawl.pie.haul.wt,
         filename = here("Figs/fig_trawl_proportion_haul_wt.png"),
         width = map.width, height = map.height) 
}
