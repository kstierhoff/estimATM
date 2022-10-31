# Get acoustic proportions for mapping
acoustic.prop.indiv.combo <- clf %>%
  filter(!is.na(CPS.wg)) %>% 
  select(cluster, lat, long, prop.anch, prop.jack, prop.her,
         prop.mack, prop.sar, sample.type) %>% 
  replace(. == 0, 0.0000001) %>%
  project_df(to = crs.proj) 

# Get empty clusters
cluster.zero.combo <- clf %>% 
  filter(is.na(CPS.wg))

# Create trawl figure
acoustic.prop.cluster.combo.final <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot trawl pies
  geom_scatterpie(data = acoustic.prop.indiv.combo, 
                  aes(X, Y, group = cluster, r = pie.radius,
                      colour = sample.type),
                  cols = c("prop.anch","prop.her","prop.jack",
                           "prop.mack","prop.sar"),
                  alpha = 0.8) +
  # geom_scatterpie(data = acoustic.prop.indiv.combo, 
  #                 aes(X, Y, group = cluster, r = pie.radius),
  #                 cols = c("prop.anch","prop.her","prop.jack",
  #                          "prop.mack","prop.sar"),
  #                 color = 'black', alpha = 0.8) +
  # Plot empty trawl locations
  geom_point(data = cluster.zero.combo, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  # Configure pie fill colors
  scale_fill_manual(name = "Species",
                    labels = c("Anchovy", "P. herring", "J. mackerel",
                               "P. mackerel", "Sardine"),
                    values = c(anchovy.color, pac.herring.color, jack.mack.color,  
                               pac.mack.color, sardine.color)) +
  # Configure pie outline colors
  scale_colour_manual(name = "Sample type", 
                      labels = c("Purse seine", "Trawl"),
                      values = c(seine.color, trawl.color),
                      guide = "none") +
  coord_sf(crs = crs.proj, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

if (doc.name == "simulateBiomass.Rmd") {
  # Save nasc plot
  ggsave(acoustic.prop.cluster.combo.final,
         filename = here("Simulation/Figs/fig_trawl_acoustic_proportion_cluster_combo.png"),
         width = map.width, height = map.height) 
  
} else {
  # Save nasc plot
  ggsave(acoustic.prop.cluster.combo.final,
         filename = here("Figs/fig_trawl_acoustic_proportion_cluster_combo.png"),
         width = map.width, height = map.height) 
}
