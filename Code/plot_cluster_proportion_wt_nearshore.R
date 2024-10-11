# Map trawl species proportions -------------------------------------------------------
# Get species present in the trawl catch
pie.spp.ns <- sort(unique(set.catch$scientificName))

if (nrow(cluster.pos.ns) > 0) {
  # Create trawl haul figure
  set.pie.cluster.wt <- base.map +
    # Plot transects data
    geom_sf(data = filter(transects.sf, Type == "Nearshore"), size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # plot ship track data
    # geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = cluster.pos.ns, aes(X, Y, group = cluster, r = pie.radius, colour = sample.type),
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
    geom_point(data = cluster.zero.ns, aes(X, Y),
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # Plot panel label
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
} else {
  # Create trawl figure
  set.pie.cluster.wt <- base.map +
    # Plot transects data
    geom_sf(data = filter(transects.sf, Type == "Nearshore"), size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # Plot empty trawl locations
    geom_point(data = cluster.zero.ns, aes(X, Y), 
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # Plot panel label
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
}

if (doc.name == "simulateBiomass.Rmd") {
  # Save nasc plot
  ggsave(set.pie.cluster.wt,
         filename = here("Simulation/Figs/fig_trawl_proportion_cluster_wt_nearshore.png"),
         width = map.width, height = map.height) 
} else if (doc.name == "reportMexico.Rmd") {
  # Save nasc plot
  ggsave(set.pie.cluster.wt,
         filename = here("Figs/fig_trawl_proportion_cluster_wt_mx_nearshore.png"),
         width = map.width, height = map.height) 
} else {
  # Save nasc plot
  ggsave(set.pie.cluster.wt,
         filename = here("Figs/fig_trawl_proportion_cluster_wt_nearshore.png"),
         width = map.width, height = map.height) 
  
}
