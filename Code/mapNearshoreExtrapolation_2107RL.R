# Create nse example map --------------------------------------------
# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- filter(strata.primary,
                  scientificName == spp.ns,
                  stock == stock.ns,
                  stratum == stratum.ns) %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>% 
  filter(transect %in% nasc.density.nse$transect,
         scientificName == spp.ns,
         density != 0) 

nasc.density.nse.sub <- nasc.density.nse %>% 
  filter(scientificName == spp.ns,
         density != 0,
         transect %in% unique(strata.sub$transect))

# Select legend objects 
dens.levels.ns <- sort(unique(nasc.density.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Map backscatter - no transect labels
nasc.map.ns <- base.map +
  geom_sf(data = filter(strata.primary, scientificName == spp.ns,
                        stock == stock.ns, stratum == stratum.ns),
          fill = NA, size = 0.5) +
  geom_sf(data = filter(strata.nse, scientificName == spp.ns,
                        stock == stock.ns, stratum == stratum.ns),
          fill = NA, size = 0.5, colour = 'red') +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) + 
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin), 
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.all, labels = dens.labels.all) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.all, labels = dens.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(1,0.5),
        legend.justification = c(0,0.5)) +
  # theme(legend.position = "right") +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]), 
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Save figures
ggsave(nasc.map.ns, 
       filename = here("Figs/fig_nasc_nse_example.png"),
       width  = map.width*1.5, height = map.height*.75)


# Create nse biomass plots for each species and stock --------------------
# Northern anchovy-Northern ----------------------------------------------------
i = "Engraulis mordax"
j = "Northern"

# Get primary and nse strata for species i
strata.nse.spp <- filter(strata.nse, scientificName == i, stock == j)

strata.primary.spp   <- filter(strata.primary, scientificName == i, stock == j, 
                               stratum %in% unique(strata.nse.spp$stratum))

# Get strata for species i and only the primary survey vessel
strata.sub <- strata.final %>% 
  filter(scientificName == i, 
         stratum %in% unique(strata.nse.spp$stratum))

# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- strata.primary.spp %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

nasc.density.nse.sub <- nasc.density.nse %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

# Select legend objects 
dens.levels.ns <- sort(unique(nasc.density.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Create polygon around strata bounding box
inset.extent <- st_as_sfc(st_bbox(strata.primary.spp))

# Define map boundaries using inset map extent and sampling domain
inset.map.bounds <- st_as_sfc(st_bbox(inset.extent)) %>% 
  st_union(st_as_sfc(st_bbox(transects.sf))) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Create inset map
inset.map <- base.map.bw + 
  geom_sf(data = inset.extent, colour = 'red', fill = NA, size = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  labs(x = NULL, y = NULL) +
  coord_sf(crs = crs.proj,
           xlim = c(inset.map.bounds["xmin"], inset.map.bounds["xmax"]), 
           ylim = c(inset.map.bounds["ymin"], inset.map.bounds["ymax"]))

# Create main map
nse.map <- base.map + 
  # Add strata
  geom_sf(data = strata.primary.spp, aes(colour = factor(stratum)), 
          fill = NA, linetype = 'dashed', alpha = 0.5) +
  geom_sf(data = strata.nse.spp, aes(colour = factor(stratum)), 
          fill = NA, alpha = 0.5) +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) + 
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin), 
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_colour_discrete("Stratum") + 
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.ns, labels = dens.labels.ns) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.ns, labels = dens.labels.ns) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(1,0.5),
        legend.justification = c(0,0.5)) +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]), 
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Get aspect ratio of nse map for setting figure dimensions
ns.aspect <- diff(nse.map$coordinates$limits$x)/diff(nse.map$coordinates$limits$y)

# Combine main and inset maps
nse.map.final <- ggdraw() +
  draw_plot(nse.map) +
  draw_plot(inset.map, 0.31, 0.72, 0.25, 0.25) 

# Save combined maps
ggsave(nse.map.final, 
       filename = paste(here("Figs/fig_biomass_dens_nse_"), i, "-", j, ".png",sep = ""),
       width  = (map.height*ns.aspect)*1.6, height = map.height*.75)

# Northern anchovy-Central -------------------------------------------------------------
i = "Engraulis mordax"
j = "Central"

# Get primary and nse strata for species i
strata.nse.spp <- filter(strata.nse, scientificName == i, stock == j)

strata.primary.spp   <- filter(strata.primary, scientificName == i, stock == j, 
                               stratum %in% unique(strata.nse.spp$stratum))

# Get strata for species i and only the primary survey vessel
strata.sub <- strata.final %>% 
  filter(scientificName == i, 
         stratum %in% unique(strata.nse.spp$stratum))

# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- strata.primary.spp %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

nasc.density.nse.sub <- nasc.density.nse %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

# Select legend objects 
dens.levels.ns <- sort(unique(nasc.density.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Create polygon around strata bounding box
inset.extent <- st_as_sfc(st_bbox(strata.primary.spp))

# Define map boundaries using inset map extent and sampling domain
inset.map.bounds <- st_as_sfc(st_bbox(inset.extent)) %>% 
  st_union(st_as_sfc(st_bbox(transects.sf))) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Create inset map
inset.map <- base.map.bw + 
  geom_sf(data = inset.extent, colour = 'red', fill = NA, size = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  labs(x = NULL, y = NULL) +
  coord_sf(crs = crs.proj,
           xlim = c(inset.map.bounds["xmin"], inset.map.bounds["xmax"]), 
           ylim = c(inset.map.bounds["ymin"], inset.map.bounds["ymax"]))

# Create main map
nse.map <- base.map + 
  # Add strata
  geom_sf(data = strata.primary.spp, aes(colour = factor(stratum)), 
          fill = NA, linetype = 'dashed', alpha = 0.5) +
  geom_sf(data = strata.nse.spp, aes(colour = factor(stratum)), 
          fill = NA, alpha = 0.5) +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) + 
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin), 
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_colour_discrete("Stratum") + 
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.ns, labels = dens.labels.ns) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.ns, labels = dens.labels.ns) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(0,0),
        legend.justification = c(0,0)) +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]), 
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Get aspect ratio of nse map for setting figure dimensions
ns.aspect <- diff(nse.map$coordinates$limits$x)/diff(nse.map$coordinates$limits$y)

# Combine main and inset maps
nse.map.final <- ggdraw() +
  draw_plot(nse.map) +
  draw_plot(inset.map, 0.66, 0.61, 0.35, 0.35) 

# Save combined maps
ggsave(nse.map.final, 
       filename = paste(here("Figs/fig_biomass_dens_nse_"), i, "-", j, ".png",sep = ""),
       width  = (map.height*ns.aspect)*.75, height = map.height*.75)

# Pacific sardine-Northern -------------------------------------------------------------
i = "Sardinops sagax"
j = "Northern"

# Get primary and nse strata for species i
strata.nse.spp <- filter(strata.nse, scientificName == i, stock == j)

strata.primary.spp   <- filter(strata.primary, scientificName == i, stock == j, 
                               stratum %in% unique(strata.nse.spp$stratum))

# Get strata for species i and only the primary survey vessel
strata.sub <- strata.final %>% 
  filter(scientificName == i, 
         stratum %in% unique(strata.nse.spp$stratum))

# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- strata.primary.spp %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

nasc.density.nse.sub <- nasc.density.nse %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

# Select legend objects 
dens.levels.ns <- sort(unique(nasc.density.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Create polygon around strata bounding box
inset.extent <- st_as_sfc(st_bbox(strata.primary.spp))

# Define map boundaries using inset map extent and sampling domain
inset.map.bounds <- st_as_sfc(st_bbox(inset.extent)) %>% 
  st_union(st_as_sfc(st_bbox(transects.sf))) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Create inset map
inset.map <- base.map.bw + 
  geom_sf(data = inset.extent, colour = 'red', fill = NA, size = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  labs(x = NULL, y = NULL) +
  coord_sf(crs = crs.proj,
           xlim = c(inset.map.bounds["xmin"], inset.map.bounds["xmax"]), 
           ylim = c(inset.map.bounds["ymin"], inset.map.bounds["ymax"]))

# Create main map
nse.map <- base.map + 
  # Add strata
  geom_sf(data = strata.primary.spp, aes(colour = factor(stratum)), 
          fill = NA, linetype = 'dashed', alpha = 0.5) +
  geom_sf(data = strata.nse.spp, aes(colour = factor(stratum)), 
          fill = NA, alpha = 0.5) +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) + 
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin), 
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_colour_discrete("Stratum") + 
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.ns, labels = dens.labels.ns) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.ns, labels = dens.labels.ns) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(1,0.5),
        legend.justification = c(0,0.5)) +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]), 
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Get aspect ratio of nse map for setting figure dimensions
ns.aspect <- diff(nse.map$coordinates$limits$x)/diff(nse.map$coordinates$limits$y)

# Combine main and inset maps
nse.map.final <- ggdraw() +
  draw_plot(nse.map) +
  draw_plot(inset.map, 0.57, 0.78, 0.2, 0.2) 

# Save combined maps
ggsave(nse.map.final, 
       filename = paste(here("Figs/fig_biomass_dens_nse_"), i, "-", j, ".png",sep = ""),
       width  = (map.height*ns.aspect)*1.5, height = map.height*.75)

# Pacific sardine-Southern -------------------------------------------------------------
i = "Sardinops sagax"
j = "Southern"

# Get primary and nse strata for species i
strata.nse.spp <- filter(strata.nse, scientificName == i, stock == j)

strata.primary.spp   <- filter(strata.primary, scientificName == i, stock == j,
                               stratum %in% unique(strata.nse.spp$stratum))

# Get strata for species i and only the primary survey vessel
strata.sub <- strata.final %>%
  filter(scientificName == i,
         stratum %in% unique(strata.nse.spp$stratum))

# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- strata.primary.spp %>%
  st_transform(crs = crs.proj) %>%
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>%
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect))

nasc.density.nse.sub <- nasc.density.nse %>%
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect))

# Select legend objects
dens.levels.ns <- sort(unique(nasc.density.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Create polygon around strata bounding box
inset.extent <- st_as_sfc(st_bbox(strata.primary.spp))

# Define map boundaries using inset map extent and sampling domain
inset.map.bounds <- st_as_sfc(st_bbox(inset.extent)) %>% 
  st_union(st_as_sfc(st_bbox(transects.sf))) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Create inset map
inset.map <- base.map.bw +
  geom_sf(data = inset.extent, colour = 'red', fill = NA, size = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  labs(x = NULL, y = NULL) +
  coord_sf(crs = crs.proj,
           xlim = c(inset.map.bounds["xmin"], inset.map.bounds["xmax"]),
           ylim = c(inset.map.bounds["ymin"], inset.map.bounds["ymax"]))

# Create main map
nse.map <- base.map +
  # Add strata
  geom_sf(data = strata.primary.spp, aes(colour = factor(stratum)),
          fill = NA, linetype = 'dashed', alpha = 0.5) +
  geom_sf(data = strata.nse.spp, aes(colour = factor(stratum)),
          fill = NA, alpha = 0.5) +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin),
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin),
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_colour_discrete("Stratum") +
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.ns, labels = dens.labels.ns) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.ns, labels = dens.labels.ns) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(0,0),
        legend.justification = c(0,0)) +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]),
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Get aspect ratio of nse map for setting figure dimensions
ns.aspect <- diff(nse.map$coordinates$limits$x)/diff(nse.map$coordinates$limits$y)

# Combine main and inset maps
nse.map.final <- ggdraw() +
  draw_plot(nse.map) +
  draw_plot(inset.map, 0.65, 0.68, 0.3, 0.3)

# Save combined maps
ggsave(nse.map.final,
       filename = paste(here("Figs/fig_biomass_dens_nse_"), i, "-", j, ".png",sep = ""),
       width  = (map.height*ns.aspect), height = map.height*.75)

# Pacific mackerel-All ---------------------------------------------------------
i = "Scomber japonicus"
j = "All"

# Get primary and nse strata for species i
strata.nse.spp <- filter(strata.nse, scientificName == i, stock == j)

strata.primary.spp   <- filter(strata.primary, scientificName == i, stock == j, 
                               stratum %in% unique(strata.nse.spp$stratum))

# Get strata for species i and only the primary survey vessel
strata.sub <- strata.final %>% 
  filter(scientificName == i, 
         stratum %in% unique(strata.nse.spp$stratum))

# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- strata.primary.spp %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

nasc.density.nse.sub <- nasc.density.nse %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

# Select legend objects 
dens.levels.ns <- sort(unique(nasc.density.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Create polygon around strata bounding box
inset.extent <- st_as_sfc(st_bbox(strata.primary.spp))

# Define map boundaries using inset map extent and sampling domain
inset.map.bounds <- st_as_sfc(st_bbox(inset.extent)) %>% 
  st_union(st_as_sfc(st_bbox(transects.sf))) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Create inset map
inset.map <- base.map.bw + 
  geom_sf(data = inset.extent, colour = 'red', fill = NA, size = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  labs(x = NULL, y = NULL) +
  coord_sf(crs = crs.proj,
           xlim = c(inset.map.bounds["xmin"], inset.map.bounds["xmax"]), 
           ylim = c(inset.map.bounds["ymin"], inset.map.bounds["ymax"]))

# Create main map
nse.map <- base.map + 
  # Add strata
  geom_sf(data = strata.primary.spp, aes(colour = factor(stratum)), 
          fill = NA, linetype = 'dashed', alpha = 0.5) +
  geom_sf(data = strata.nse.spp, aes(colour = factor(stratum)), 
          fill = NA, alpha = 0.5) +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) + 
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin), 
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_colour_discrete("Stratum") + 
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.ns, labels = dens.labels.ns) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.ns, labels = dens.labels.ns) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(0,0),
        legend.justification = c(0,0)) +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]), 
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Get aspect ratio of nse map for setting figure dimensions
ns.aspect <- diff(nse.map$coordinates$limits$x)/diff(nse.map$coordinates$limits$y)

nse.map.final <- ggdraw() +
  draw_plot(nse.map) +
  draw_plot(inset.map, 0.60, 0.70, 0.28, 0.28)

# Save combined maps
ggsave(nse.map.final, 
       filename = paste(here("Figs/fig_biomass_dens_nse_"), i, "-", j, ".png",sep = ""),
       width  = (map.height*ns.aspect)*1.25, height = map.height*.75)

# Jack mackerel-All ---------------------------------------------------------
i = "Trachurus symmetricus"
j = "All"

# Get primary and nse strata for species i
strata.nse.spp <- filter(strata.nse, scientificName == i, stock == j)

strata.primary.spp   <- filter(strata.primary, scientificName == i, stock == j, 
                               stratum %in% unique(strata.nse.spp$stratum))

# Get strata for species i and only the primary survey vessel
strata.sub <- strata.final %>% 
  filter(scientificName == i, 
         stratum %in% unique(strata.nse.spp$stratum))

# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- strata.primary.spp %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) %>% 
  droplevels()

nasc.density.nse.sub <- nasc.density.nse %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) %>% 
  droplevels()

# Select legend objects 
dens.levels.ns <- sort(unique(nasc.density.nse.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Create polygon around strata bounding box
inset.extent <- st_as_sfc(st_bbox(strata.primary.spp))

# Define map boundaries using inset map extent and sampling domain
inset.map.bounds <- st_as_sfc(st_bbox(inset.extent)) %>% 
  st_union(st_as_sfc(st_bbox(transects.sf))) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Create inset map
inset.map <- base.map.bw + 
  geom_sf(data = inset.extent, colour = 'red', fill = NA, size = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  labs(x = NULL, y = NULL) +
  coord_sf(crs = crs.proj,
           xlim = c(inset.map.bounds["xmin"], inset.map.bounds["xmax"]), 
           ylim = c(inset.map.bounds["ymin"], inset.map.bounds["ymax"]))

# Create main map
nse.map <- base.map + 
  # Add strata
  geom_sf(data = strata.primary.spp, aes(colour = factor(stratum)), 
          fill = NA, linetype = 'dashed', alpha = 0.5) +
  geom_sf(data = strata.nse.spp, aes(colour = factor(stratum)), 
          fill = NA, alpha = 0.5) +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) + 
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin), 
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_colour_discrete("Stratum") + 
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.ns, labels = dens.labels.ns) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.ns, labels = dens.labels.ns) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(1,0.5),
        legend.justification = c(0,0.5)) +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]), 
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Get aspect ratio of nse map for setting figure dimensions
ns.aspect <- diff(nse.map$coordinates$limits$x)/diff(nse.map$coordinates$limits$y)

# Combine main and inset maps
nse.map.final <- ggdraw() +
  draw_plot(nse.map) +
  draw_plot(inset.map, 0.55, 0.75, 0.225, 0.225)

# Save combined maps
ggsave(nse.map.final, 
       filename = paste(here("Figs/fig_biomass_dens_nse_"), i, "-", j, ".png",sep = ""),
       width  = (map.height*ns.aspect)*1.5, height = map.height*.75)

# Pacific herring-All ---------------------------------------------------------
i = "Clupea pallasii"
j = "All"

# Get primary and nse strata for species i
strata.nse.spp <- filter(strata.nse, scientificName == i, stock == j)

strata.primary.spp   <- filter(strata.primary, scientificName == i, stock == j, 
                               stratum %in% unique(strata.nse.spp$stratum))

# Get strata for species i and only the primary survey vessel
strata.sub <- strata.final %>% 
  filter(scientificName == i, 
         stratum %in% unique(strata.nse.spp$stratum))

# Set the map boundaries based on the bounding box of the offshore stratum
ns.lims <- strata.primary.spp %>% 
  st_transform(crs = crs.proj) %>% 
  st_bbox()

# Get biomass density in the ROI
nasc.density.sub <- nasc.density %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

nasc.density.nse.sub <- nasc.density.nse %>% 
  filter(scientificName == i,
         density != 0,
         transect %in% unique(strata.sub$transect)) 

# Select legend objects 
dens.levels.ns <- sort(unique(nasc.density.sub$bin.level))
dens.labels.ns <- dens.labels[dens.levels.ns]
dens.sizes.ns  <- dens.sizes[dens.levels.ns]
dens.colors.ns <- dens.colors[dens.levels.ns]

# Create polygon around strata bounding box
inset.extent <- st_as_sfc(st_bbox(strata.primary.spp))

# Define map boundaries using inset map extent and sampling domain
inset.map.bounds <- st_as_sfc(st_bbox(inset.extent)) %>% 
  st_union(st_as_sfc(st_bbox(transects.sf))) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Create inset map
inset.map <- base.map.bw + 
  geom_sf(data = inset.extent, colour = 'red', fill = NA, size = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  labs(x = NULL, y = NULL) +
  coord_sf(crs = crs.proj,
           xlim = c(inset.map.bounds["xmin"], inset.map.bounds["xmax"]), 
           ylim = c(inset.map.bounds["ymin"], inset.map.bounds["ymax"]))

# Create main map
nse.map <- base.map + 
  # Add strata
  geom_sf(data = strata.primary.spp, aes(colour = factor(stratum)), 
          fill = NA, linetype = 'dashed', alpha = 0.5) +
  geom_sf(data = strata.nse.spp, aes(colour = factor(stratum)), 
          fill = NA, alpha = 0.5) +
  # Plot vessel track
  geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) + 
  # Plot NASC data
  geom_point(data = nasc.density.sub, aes(X, Y, size = bin, fill = bin), 
             colour = 'gray50', alpha = 0.75) +
  geom_point(data = nasc.density.nse.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_colour_discrete("Stratum") + 
  scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.sizes.ns, labels = dens.labels.ns) +
  scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                    values = dens.colors.ns, labels = dens.labels.ns) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(legend.position      = c(0,0),
        legend.justification = c(0,0)) +
  coord_sf(crs = crs.proj,
           xlim = c(ns.lims["xmin"], ns.lims["xmax"]), 
           ylim = c(ns.lims["ymin"], ns.lims["ymax"]))

# Get aspect ratio of nse map for setting figure dimensions
ns.aspect <- diff(nse.map$coordinates$limits$x)/diff(nse.map$coordinates$limits$y)

# Combine main and inset maps
nse.map.final <- ggdraw() +
  draw_plot(nse.map) +
  draw_plot(inset.map, 0.68, 0.70, 0.24, 0.24) 

# Save combined maps
ggsave(nse.map.final, 
       filename = paste(here("Figs/fig_biomass_dens_nse_"), i, "-", j, ".png",sep = ""),
       width  = (map.height*ns.aspect)*.75, height = map.height*.75)
