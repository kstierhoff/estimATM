# load(here("Output/nasc_final.Rdata"))

source(here("Doc/settings/settings_2107RL.R"))

nasc.paths <- fs::as_fs_path(c("C:/KLS/CODE/R_packages/estimATM/1507SH/Output/nasc_final.Rdata",
                "C:/KLS/CODE/R_packages/estimATM/1606RL/Output/nasc_final.Rdata",
                "C:/KLS/CODE/R_packages/EstimateCPS/1707RL/Output/nasc_final.Rdata",
                "C:/KLS/CODE/R_packages/estimATM/1807RL/Output/nasc_final.Rdata",
                "C:/KLS/CODE/R_packages/estimATM/1907RL/Output/nasc_final.Rdata",
                "C:/KLS/CODE/R_packages/estimATM/2107RL/Output/nasc_final.Rdata"))

nasc.all <- data.frame()

for (i in seq_along(nasc.paths)) {
  load(nasc.paths[i])  
  
  nasc2 <- nasc %>% 
    select(transect, transect.orig, int, lat, long, sar.dens) %>% 
    mutate(path = i,
           survey = unlist(stringr::str_split(nasc.paths[i], "/"))[6])
  
  nasc.all <- bind_rows(nasc.all, nasc2)
}

nasc.all <- nasc.all %>% 
  mutate(stock = case_when(
    survey == "2107RL" & lat > 37.674 ~ "Northern",
    survey != "2107RL" & lat > 34.7 ~ "Northern",
    TRUE ~ "Southern"
  ))

# Format for plotting
nasc.density <- nasc.all %>%
  # select(transect, transect.orig, int, lat, long, 
  #        anch.dens, her.dens, jack.dens, mack.dens, sar.dens, rher.dens) %>% 
  # When transects have multiple sections (e.g., 041-1, 041-2), interval is not unique
  # So create a int.key for computing density along unique intervals for plotting
  mutate(int.key = paste(transect.orig, int)) %>% 
  # Then summarize by transect and unique interval
  group_by(survey, stock, transect, int.key) %>%
  summarise(
    lat = lat[1],
    long = long[1],
    sar.density = mean(sar.dens)) %>% 
  gather(scientificName, density, -survey, -stock, -transect, -int.key, -lat, -long) %>% 
  mutate(bin       = cut(density, dens.breaks, include.lowest = TRUE),
         bin.level = as.numeric(bin)) %>% 
  atm::project_df(to =  3310)

# Filter biomass density
nasc.density.plot <- nasc.density %>%
  filter(density != 0)

nasc.density.zero <- nasc.density %>%
  filter(density == 0)

# Select legend objects 
dens.levels.all <- sort(unique(nasc.density$bin.level))
dens.labels.all <- dens.labels[dens.levels.all]
dens.sizes.all  <- dens.sizes[dens.levels.all]
dens.colors.all <- dens.colors[dens.levels.all]


ggplot() + 
  geom_point(data = nasc.density.zero, aes(long, lat), size = 0.5)  +
  geom_point(data = nasc.density.plot, aes(long, lat, size = density, colour = stock)) + 
  scale_colour_manual(name = "Stock", values = c("Northern" = "blue", "Southern" = "red")) +
  coord_map() + 
  facet_wrap(~survey, nrow = 1)

ggsave("sardine_density_ts_stock.png",
       height = 6, width = 16)

ggplot() + 
  # geom_point(data = nasc.density.zero, aes(long, lat), size = 0.5)  + 
  # geom_point(data = nasc.density.plot, aes(long, lat, size = density), colour = "red") + 
  # Plot zero nasc data
  geom_point(data = nasc.density.zero, aes(long, lat),
             colour = 'gray50',size = 0.15, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.density.plot, aes(long, lat, size = bin, fill = bin),
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.all, labels = dens.labels.all) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.all, labels = dens.labels.all) +
  # Configure legend guides
  guides(colour = guide_legend(order = 1),
         fill   = guide_legend(order = 2), 
         size   = guide_legend(order = 2)) +
  coord_map() + facet_wrap(~path, nrow = 1)

ggsave("sardine_density_ts.png",
       height = 6, width = 16)

# Map biomass density, strata polygons, and positive trawl clusters
biomass.dens <- base.map +
  # Plot zero nasc data
  geom_point(data = filter(nasc, cps.nasc == 0), aes(X, Y),
             colour = 'gray50',size = 0.15, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.density.plot, aes(X, Y, size = bin, fill = bin),
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.all, labels = dens.labels.all) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.all, labels = dens.labels.all) +
  # Plot positive cluster midpoints
  geom_shadowtext(data = pos.cluster.txt,
                  aes(X, Y, label = cluster), 
                  colour = "blue", bg.colour = "white", size = 2, fontface = "bold") +
  # Configure legend guides
  guides(colour = guide_legend(order = 1),
         fill   = guide_legend(order = 2), 
         size   = guide_legend(order = 2)) +
  coord_sf(crs = crs.proj, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
