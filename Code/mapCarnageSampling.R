lbc.sets <- read_csv("Data/Seine/lbc_sets.csv") %>% 
  mutate(datetime = mdy_hm(datetime)) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

nasc.plot.lbc <- filter(nasc.plot.ns, vessel.name == "LBC")

map.bounds.lbc <- nasc.plot.lbc %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

pie.scale.lbc <- 0.02
pie.radius.lbc <- as.numeric(abs(map.bounds.lbc$ymin - map.bounds.lbc$ymax)*pie.scale.lbc)

# Create trawl haul figure
lbc.sets.map <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.plot.lbc, aes(X, Y, size = bin), 
             fill = "black", shape = 21, alpha = 0.5) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = sqrt(nasc.sizes.all*6), labels = nasc.labels.all) +
  # Plot trawl pies
  geom_scatterpie(data = haul.pos, aes(X, Y, group = haul, r = pie.radius.lbc),
                  cols = c("Anchovy", "JackMack", "Jacksmelt",
                           "PacHerring", "PacMack", "Sardine"),
                  color = 'black', alpha = 0.8) +
  # plot Carnage purse seine sets track data
  geom_sf(data = lbc.sets, color = "black", fill = "orange", size = 5, shape = 24) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, sardine.color)) +
  # Plot empty cluster locations
  geom_point(data = haul.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  # Plot panel label
  ggtitle("Long Beach Carnage Sets") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.lbc["xmin"], map.bounds.lbc["xmax"]), 
           ylim = c(map.bounds.lbc["ymin"], map.bounds.lbc["ymax"]))

ggsave(lbc.sets.map, filename = here("Figs/fig_lbc_set_map.png"),
       height = 10, width = 15)
  
