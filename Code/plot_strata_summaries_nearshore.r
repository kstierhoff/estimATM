# This script plots the biomass density by transect and species, 
# and includes the results of the stratification. 

if (nrow(strata.summ.rm) > 0) {
  # If strata were removed
  # Biomass density by species and transect, showing stratum breaks
  biomass.dens.plot.lat.ns <- ggplot(strata.summ.plot.ns) + 
    geom_rect(aes(xmin = lat.start, xmax = lat.end, ymin = 0, ymax = max.tx.dens.log, 
                  group = stratum, colour = factor(stratum)), 
              fill = 'gray70', alpha = 0.5) +
    geom_rect(data = strata.summ.rm, aes(xmin = lat.start, xmax = lat.end, ymin = 0, ymax = max.tx.dens.log,
                                         group = stratum.orig),
              fill = 'gray50', alpha = 0.5, linetype = "dotted") +
    geom_hline(yintercept = 0, colour = 'gray20') +
    geom_vline(xintercept = landmarks$lat.start, linetype = "dashed") +
    geom_vline(xintercept = stock.breaks$lat.start, linetype = "dashed", colour = "red") +
    geom_text(data = landmarks, 
              aes(lat.start, max.tx.dens.log, label = name), 
              inherit.aes = FALSE, nudge_x = 0.1, hjust = 1, vjust = 0, 
              fontface = "italic", alpha = 0.5) +
    geom_path(data = nasc.density.summ.ns,  
              aes(start.lat, log(density + 1), group = scientificName)) +
    geom_point(data = nasc.density.summ.ns, 
               aes(start.lat, log(density + 1), group = scientificName,
                   fill = dist.bin), shape = 21, size = 3) +
    # Label non-zero biomass density to facilitate picking strata
    geom_text_repel(data = filter(nasc.density.summ.ns, log(density + 1) > 0),
                    aes(start.lat, log(density + 1), label = transect, group = scientificName),
                    colour = 'blue', size = 3, fontface = 'italic', nudge_y = 1) +
    scale_colour_discrete("Stratum") +
    scale_fill_discrete("Spacing (nmi)") +
    facet_wrap(scientificName ~ vessel.name, nrow = 1) + 
    scale_x_continuous("Start latitude", 
                       breaks = seq(floor(min(strata.summ$lat.start)), 
                                    ceiling(max(strata.summ$lat.end)),1)) + 
    scale_y_continuous(expression(Mean~log~biomass~density~(log(t~nmi^-2+1)))) +
    theme_bw() + 
    theme(strip.text.x       = element_text(face = "bold.italic"),
          strip.background.x = element_rect(fill = NA)) +
    coord_flip()
  
  # Original plot, based on transect number on the x-axis
  biomass.dens.plot.tx.ns <- ggplot(strata.summ) +
    geom_rect(aes(xmin = start - 0.4, xmax = end + 0.4, 
                  ymin = 0, ymax = max.tx.dens.log, 
                  group = stratum, colour = factor(stratum)),
              fill = 'gray70', alpha = 0.5) +
    geom_rect(data = strata.summ.rm, aes(xmin = start - 0.4, xmax = end + 0.4, 
                                         ymin = 0, ymax = max.tx.dens.log, 
                                         group = stratum.orig),
              fill = 'gray50', alpha = 0.5, linetype = "dotted") +
    geom_hline(yintercept = 0, colour = 'gray20') +
    geom_path(data = nasc.density.summ.ns, 
              aes(transect, log(density + 1), group = scientificName)) +
    geom_point(data = nasc.density.summ.ns, 
               aes(transect, log(density + 1), group = scientificName, fill = dist.bin), 
               shape = 21, size = 3) +
    # Label non-zero biomass density to facilitate picking strata
    geom_text_repel(data = filter(nasc.density.summ.ns,log(density + 1) > 0),
                    aes(transect, log(density + 1), label = transect, group = scientificName),
                    colour = 'blue', size = 3, fontface = 'italic', nudge_y = 1) +
    scale_colour_discrete("Stratum") +
    scale_fill_discrete("Spacing (nmi)") +
    facet_wrap(~scientificName,ncol = 1) +
    xlab("Transect") + 
    scale_y_continuous(expression(Mean~log~biomass~density~(log(t~nmi^-2)+1)),
                       limits = c(-0.5, max.tx.dens.log), expand = c(0,0)) +
    theme_bw() +
    theme(strip.background.x = element_blank(),
          strip.text.x       = element_text(face = "italic"))
  
} else {
  # If no strata were removed
  # Biomass density by species and transect, showing stratum breaks
  biomass.dens.plot.lat.ns <- ggplot(strata.summ.plot.ns) + 
    geom_rect(aes(xmin = lat.start, xmax = lat.end, ymin = 0, ymax = max.tx.dens.log, 
                  group = stratum, colour = factor(stratum)), 
              fill = 'gray70', alpha = 0.5) +
    geom_hline(yintercept = 0, colour = 'gray20') +
    geom_vline(xintercept = landmarks$lat.start) +
    geom_vline(data = stock.breaks, aes(xintercept = lat.start, group = scientificName), 
               linetype = "dashed", colour = "red") +
    geom_text(data = landmarks, 
              aes(lat.start, max.tx.dens.log, label = name), 
              inherit.aes = FALSE, nudge_x = 0.1, hjust = 1, vjust = 0, 
              fontface = "italic", alpha = 0.5) +
    geom_text(data = stock.breaks,
              aes(lat.start, max.tx.dens.log, label = scientificName, group = scientificName),
              inherit.aes = FALSE, nudge_x = -0.1, hjust = 1, vjust = 1,
              fontface = "italic", alpha = 0.5, colour = "red") +
    geom_path(data = nasc.density.summ.ns,  
              aes(start.lat, log(density + 1), group = scientificName)) +
    geom_point(data = nasc.density.summ.ns, 
               aes(start.lat, log(density + 1), group = scientificName,
                   fill = dist.bin), shape = 21, size = 3) +
    # Label non-zero biomass density to facilitate picking strata
    geom_text_repel(data = filter(nasc.density.summ.ns, log(density + 1) > 0),
                    aes(start.lat, log(density + 1), label = transect, group = scientificName),
                    colour = 'blue', size = 3, fontface = 'italic', nudge_y = 1) +
    scale_colour_discrete("Stratum") +
    scale_fill_discrete("Spacing (nmi)") +
    facet_grid(vessel.name~scientificName) + 
    scale_x_continuous("Start latitude", 
                       breaks = seq(floor(min(strata.summ.plot.ns$lat.start)), 
                                    ceiling(max(strata.summ.plot.ns$lat.end)),1)) + 
    scale_y_continuous(expression(Biomass~density~(log(t+1)~nmi^-2))) +
    theme_bw() + 
    theme(strip.text.x       = element_text(face = "bold.italic"),
          strip.background.x = element_rect(fill = NA)) +
    coord_flip()
  
  # Original plot, based on transect number on the x-axis
  biomass.dens.plot.tx.ns <- ggplot(strata.summ.plot.ns) +
    geom_rect(aes(xmin = start - 0.4, xmax = end + 0.4, 
                  ymin = 0, ymax = max.tx.dens.log, 
                  group = stratum, colour = factor(stratum)),
              fill = 'gray70', alpha = 0.5) +
    geom_hline(yintercept = 0, colour = 'gray20') +
    geom_path(data = nasc.density.summ.ns, 
              aes(transect, log(density + 1), group = scientificName)) +
    geom_point(data = nasc.density.summ.ns, 
               aes(transect, log(density + 1), group = scientificName, fill = dist.bin), 
               shape = 21, size = 3) +
    # Label non-zero biomass density to facilitate picking strata
    geom_text_repel(data = filter(nasc.density.summ.ns,log(density + 1) > 0),
                    aes(transect, log(density + 1), label = transect, group = scientificName),
                    colour = 'blue', size = 3, fontface = 'italic', nudge_y = 1) +
    scale_colour_discrete("Stratum") +
    scale_fill_discrete("Spacing (nmi)") +
    facet_wrap(~scientificName,ncol = 1) +
    xlab("Transect") + 
    scale_y_continuous(expression(Biomass~density~(log(t+1)~nmi^-2)), 
                       limits = c(-0.5,max.tx.dens.log), expand = c(0,0)) +
    theme_bw() +
    theme(strip.background.x = element_blank(),
          strip.text.x       = element_text(face = "italic"))
}

# Save plots
if (diff(range(nasc.density.summ.ns$start.lat)) > 6) {
  ggsave(biomass.dens.plot.lat.ns, 
         filename = here("Figs/fig_biomass_density_transect_lat_ns.png"),
         height = 12, width = 12*n_distinct(strata.summ.nearshore$vessel.name))
  
  ggsave(biomass.dens.plot.tx.ns, 
         filename = here("Figs/fig_biomass_density_transect_tx_ns.png"),
         height = 10, width = 15)  
} else {
  ggsave(biomass.dens.plot.lat.ns, 
         filename = here("Figs/fig_biomass_density_transect_lat_ns.png"),
         height = 8, width = 8*n_distinct(strata.summ.nearshore$vessel.name))
  
  ggsave(biomass.dens.plot.tx.ns, 
         filename = here("Figs/fig_biomass_density_transect_tx_ns.png"),
         height = 10, width = 15) 
}
