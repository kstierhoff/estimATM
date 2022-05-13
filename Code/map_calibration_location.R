# Create calibration site maps #####
# Put calibration location lat/lon in a df, for point plot
cal.map.df <- data.frame(cal.lat.dd, cal.lon.dd)

# get Google map from calibration location
cal.map <- ggmap(get_googlemap(c(cal.lon.dd, cal.lat.dd),
                               maptype = "terrain", zoom = 12))

# get extent of calibration map for inset
cal.map.extent <- cal.map$data

# create inset map
map.inset <- ggplot(ca) +
  geom_sf() +
  geom_path(data = cal.map.extent[c(1, 3, 4, 2, 1),], aes(lon, lat),
            size = 0.5, colour = 'red') + # plot extent of inset map
  theme(axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        axis.ticks      = element_blank(),
        axis.title.x    = element_blank(),
        axis.title.y    = element_blank(),
        plot.background = element_blank())

# create main map using ggmap
map.main <- cal.map +
  geom_point(data = cal.map.df, aes(x = cal.lon.dd,y = cal.lat.dd),
             shape = 23, size = 5, fill = "yellow", colour = "black") +
  xlab("\nLongitude (W)") + ylab("Latitude (N)\n") +
  theme_bw() +
  theme(plot.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

# Create final color map
cal.map.color <- ggdraw() +
  draw_plot(map.main,0,0,1,1) +
  draw_plot(map.inset,0.65,0.65,0.325,0.325)

# Save map image
ggsave(cal.map.color, filename = here("Figs/fig_cal_map.png"),
       height = 10, width = 10, units = "in")
