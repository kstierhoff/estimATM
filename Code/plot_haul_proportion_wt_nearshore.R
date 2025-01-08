# Map trawl species proportions -------------------------------------------------------
# Get species present in the trawl catch
pie.spp.ns <- sort(unique(catch$scientificName))

if (nrow(haul.pos.ns) > 0) {
  # Create trawl haul figure
  set.pie.haul.wt <- base.map +
    # Plot transects data
    geom_sf(data = filter(transects.sf, Type == "Nearshore"), size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # # plot ship track data
    # geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = haul.pos.ns, aes(X, Y, group = haul, r = pie.radius, colour = sample.type),
                    cols = pie.cols[names(pie.cols) %in% pie.spp.ns],
                    alpha = 0.8, sorted_by_radius = TRUE) +
    # Configure pie outline colors
    scale_colour_manual(name = "Sample type", 
                        labels = c("Purse seine", "Trawl"),
                        values = c("Purse seine" = seine.color, "Trawl" = trawl.color),
                        guide = "none") +
    # Configure trawl scale
    scale_fill_manual(name = 'Species',
                      labels = unname(pie.labs[names(pie.labs) %in% pie.spp.ns]),
                      values = unname(pie.colors[names(pie.colors) %in% pie.spp.ns])) +
    # Plot empty cluster locations
    geom_point(data = haul.zero.ns, aes(X, Y),
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # geom_scatterpie_legend(radius = trawl.breaks, x = -50000, y = -600000) + 
    coord_sf(crs = crs.proj, 
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
  
} else {
  # Create trawl figure
  set.pie.haul.wt <- base.map +
    # Plot transects data
    geom_sf(data = filter(transects.sf, Type == "Nearshore"), size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # Plot empty trawl locations
    geom_point(data = haul.zero.ns, aes(X, Y), 
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    coord_sf(crs = crs.proj, 
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
}

if (doc.name == "simulateBiomass.Rmd") {
  # Save nasc plot
  ggsave(set.pie.haul.wt,
         filename = here("Simulation/Figs/fig_trawl_proportion_haul_wt_nearshore.png"),
         width = map.width, height = map.height) 
} else if (doc.name == "reportMexico.Rmd") {
  # Save nasc plot
  ggsave(set.pie.haul.wt,
         filename = here("Figs/fig_trawl_proportion_haul_wt_mx_nearshore.png"),
         width = map.width, height = map.height) 
} else {
  # Save nasc plot
  ggsave(set.pie.haul.wt,
         filename = here("Figs/fig_trawl_proportion_haul_wt_nearshore.png"),
         width = map.width, height = map.height) 
}
