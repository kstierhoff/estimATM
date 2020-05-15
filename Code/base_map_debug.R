ggplot2::ggplot() +
  # Plot bathymetry contours
  ggplot2::geom_sf(data = bathy, colour = "gray90", alpha = 0.5) +
  # # Plot high-res land polygons
  ggplot2::geom_sf(data = countries, fill = "gray90", colour = "gray50") +
  ggplot2::geom_sf(data = states, fill = "gray90", colour = "gray50") +
  # Plot landmarks
  ggplot2::geom_point(data = locations, aes(X, Y), size = 2, colour = 'gray50') +
  shadowtext::geom_shadowtext(data  = locations, aes(X, Y, label = name),
                              colour = 'gray20', size = 2, fontface = 'bold',
                              hjust = 0, nudge_x = 0.2, nudge_y = 0.05, angle = 25,
                              bg.colour = "white") +
  # Format axes and titles
  xlab("Longitude") + ylab("Latitude") +
  # ggplot2::coord_sf(crs = 3310) +
  ggplot2::coord_sf(crs = 3310,
                    xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
                    ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
  theme_bw() +
  theme(axis.text.y          = element_text(angle = 90, hjust = 0.5),
        legend.position      = c(0,0),
        legend.justification = c(0,0),
        legend.background    = element_blank(),
        legend.key           = element_blank(),
        plot.title           = element_text(hjust = 0.5),
        panel.grid.major     = element_line(color = "gray90")) +
  ggspatial::annotation_scale()
