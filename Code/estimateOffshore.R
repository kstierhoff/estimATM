# Estimate offshore biomass -----------------------------------------------
# Remove any existing offshore backscatter data from memory
if (exists("nasc.offshore")) {rm(nasc.offshore)}

# Process offshore backscatter data
if (process.offshore) {
  # List offshore backscatter files
  offshore.files <- dir_ls(here("Data/Backscatter"), recurse = TRUE,
                           regexp = "offshore.rds")
  
  # Import all offshore backscatter data
  for (oo in offshore.files) {
    if (exists("nasc.offshore")) {
      nasc.offshore <- bind_rows(nasc.offshore, readRDS(offshore.files[oo]))
    } else {
      nasc.offshore <- readRDS(offshore.files[oo])
    }
  }
  
  # Remove vessels not to be included in the offshore biomass estimates
  nasc.offshore <- nasc.offshore %>% 
    filter(vessel.name %in% nasc.vessels.offshore) %>% 
    mutate(vessel.orig = case_when(
      is.na(vessel.orig) ~ vessel.name,
      TRUE ~ as.character(vessel.orig)))
  
  # Combine nasc data for all NASC vessels
  if (merge.vessels["OS"]) {
    nasc.offshore <- nasc.offshore %>% 
      mutate(vessel.name = "RL")
  } else {
    nasc.offshore <- nasc.offshore %>% 
      mutate(vessel.orig = vessel.name)
  }
  
  # Format data for processing
  nasc.offshore <- nasc.offshore %>%
    mutate(
      id = seq_along(Interval),
      transect = str_replace(transect, "O", ""),
      transect.name = paste(vessel.name, transect),
      stratum       = 1,
      int = cut(Interval, seq(1, max(Interval) + nasc.summ.interval,
                              nasc.summ.interval),
                labels = F, include.lowest = TRUE))
  
  # Export data for processing using the CTD app
  write_csv(nasc.offshore, here("Output/CTDapp/CTDapp_All_Offshore.csv"))
  save(nasc.offshore, file = here("Output/CTDapp/CTDapp_All_Offshore.Rdata"))
  
  # Apply cps.nasc, or use a fixed integration depth
  if (source.cps.nasc["OS"]) {
    # Use exteranlly supplied cps.nasc with variable integration depth (from CTD.app)
    # Read file and create unique key for joining with nasc.vessel
    cps.nasc.temp <- read.csv(data.cps.nasc["OS"]) %>% 
      mutate(key = paste(lat, long, dist_m),
             datetime = ymd_hms(paste(date, time))) %>% 
      # Remove data from krill files (1807RL)
      filter(!str_detect(tolower(filename), "krill")) 
    
    # Join nasc.vessel and cps.nasc on datetime
    nasc.offshore <- nasc.offshore %>% 
      left_join(select(cps.nasc.temp, datetime, cps.nasc))
    
  } else {
    nasc.offshore <- nasc.offshore %>%
      mutate(cps.nasc = NASC.50)
  }
  
  # Identify intervals that fall outside the primary survey area
  nasc.offshore.diff <- nasc.offshore %>% 
    select(long, lat, id) %>% 
    st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
    st_difference(strata.super.polygons)
  
  # Remove intervals that fall within the primary survey area
  nasc.offshore <- filter(nasc.offshore, id %in% nasc.offshore.diff$id)
  
  # Assign backscatter to trawl clusters ------------------------------------
  # Create varialble for nearest cluster and minumum distance
  cluster.distance.os <- data.frame(cluster = rep(NA, nrow(nasc.offshore)),
                                    cluster.distance = rep(NA, nrow(nasc.offshore)))
  # Configure progress bar
  pb <- tkProgressBar("R Progress Bar", "Cluster Assignment", 0, 100, 0)
  
  # Assign trawl clusters
  for (i in 1:nrow(nasc.offshore)) {
    # Calculate distance between each NASC interval and all trawl clusters
    temp.distance <- distance(nasc.offshore$lat[i], nasc.offshore$long[i], 
                              super.clusters$lat, super.clusters$long, 
                              units = "nm")
    
    # Assign cluster with minimum distance to NASC interval
    cluster.distance.os$cluster[i]          <- super.clusters$cluster[which.min(temp.distance)]
    cluster.distance.os$cluster.distance[i] <- temp.distance[which.min(temp.distance)]
    
    # Update progress bar
    pb.prog <- round(i/nrow(nasc.offshore)*100)
    info <- sprintf("%d%% done", pb.prog)
    setTkProgressBar(pb, pb.prog, sprintf("Cluster Assignment (%s)", info), info)
  }
  
  # Close progress bar
  close(pb)
  
  # Add cluster distances to nasc
  nasc.offshore <- bind_cols(nasc.offshore, cluster.distance.os) %>% 
    project_df(to = crs.proj)
  
  # Save nasc cluster vector
  save(cluster.distance.os, file = here("Output/nasc_cluster_distance_offshore.Rdata"))
  
  # Save results of processing
  save(nasc.offshore, file = here("Data/Backscatter/nasc_offshore.Rdata"))
  
} else {
  # Load processed data
  load(here("Data/Backscatter/nasc_offshore.Rdata"))
}

# Summarise nasc for plotting ---------------------------------------------
nasc.plot.os <- nasc.offshore %>% 
  select(filename, transect, transect.name, int, lat, long, cps.nasc) %>% 
  group_by(filename, transect, transect.name, int) %>% 
  summarise(
    lat  = lat[1],
    long = long[1],
    NASC = mean(cps.nasc)) %>% 
  # Create bins for defining point size in NASC plots%>% 
  mutate(bin       = cut(NASC, nasc.breaks, include.lowest = TRUE),
         bin.level =  as.numeric(bin)) %>% 
  ungroup() %>% 
  project_df(to = crs.proj)

# Summarize numbers of intervals per trawl cluster
nasc.offshore.summ <- nasc.offshore %>% 
  group_by(cluster) %>% 
  tally() %>% 
  filter(n >= 4)

# Map trawl clusters -------------------------------------------------------
# Create hulls around positive clusters
nasc.super.clusters.os <- nasc.offshore %>% 
  filter(cluster %in% nasc.offshore.summ$cluster) %>% 
  plyr::ddply("cluster", find_hull) %>% 
  select(long, lat, cluster) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(cluster) %>% 
  summarise(do_union = F) %>% 
  st_cast("POLYGON")

if (save.figs) {
  nasc.cluster.plot.os <- base.map +
    # Plot nasc data
    geom_point(data = nasc.offshore, aes(X, Y), size = 0.5, show.legend = FALSE) +
    # Plot convex hull around NASC clusters
    geom_sf(data = nasc.super.clusters.os, colour = 'black', alpha = 0.5, show.legend = FALSE) +
    # geom_point(data = nasc.offshore, aes(X, Y, colour = factor(cluster)),
    #            show.legend = FALSE) +
    # # Plot convex hull around NASC clusters
    # geom_sf(data = nasc.super.clusters.os, aes(fill = factor(cluster)),
    #         colour = 'black', alpha = 0.5, show.legend = FALSE) +
    scale_fill_discrete(name = "Cluster") +
    scale_colour_discrete(name = "Cluster") +
    # Plot cluster midpoints
    geom_shadowtext(data = cluster.mid, aes(X, Y, label = cluster),
                    colour = 'gray20', bg.colour = "white", size = 2) +
    # Plot positive trawl cluster midpoints
    geom_shadowtext(data = filter(cluster.mid, cluster %in% super.clusters$cluster), 
                    aes(X, Y, label = cluster), 
                    size = 2, colour = "blue", bg.colour = "white", fontface = "bold") +
    # Plot panel label
    # ggtitle("Integrated NASC Clusters-Offshore") +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # save trawl plot
  ggsave(nasc.cluster.plot.os,
         filename = here("Figs/fig_nasc_cluster_map_os.png"),
         width = map.width, height = map.height)
  
  save(nasc.cluster.plot.os, file = here("Output/nasc_cluster_plot_os.Rdata"))
}

# Map trawl species proportions -------------------------------------------------------
# Select and rename trawl data for pie charts
cluster.pie.os <- cluster.pie %>% 
  filter(cluster %in% unique(nasc.offshore.summ$cluster)) 

cluster.pos.os <- filter(cluster.pie.os, AllCPS > 0) %>% 
  arrange(desc(X))

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(cluster.pos.os) > 0) {
  cluster.pos.os <- cluster.pos.os %>% 
    replace(. == 0, 0.0000001) 
}

# Filter for empty trawls
cluster.zero.os <- filter(cluster.pie.os, AllCPS == 0)

if (save.figs) {
  # Create trawl figure
  trawl.catch.plot.os <- base.map +
    # Plot nasc data
    geom_point(data = nasc.offshore, aes(X, Y, group = transect),
               size = 0.5, colour = "gray50", alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = cluster.pos.os, 
                    aes(X, Y, group = cluster, r = pie.radius),
                    cols = c("Anchovy","JackMack","Jacksmelt",
                             "PacHerring","PacMack","Sardine"),
                    color = 'black', alpha = 0.8) +
    # Configure trawl scale
    scale_fill_manual(name = 'Species',
                      labels = c("Anchovy", "J. Mackerel", "Jacksmelt", 
                                 "P. herring", "P. mackerel", "Sardine"),
                      values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                                 pac.herring.color, pac.mack.color, sardine.color)) +
    # Plot empty trawl locations
    geom_point(data = cluster.zero.os, aes(X, Y), 
               size = 2, shape = 21, fill = 'black', colour = 'white') +
    # Plot panel label
    ggtitle("CPS Species Proportions in Trawls") +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # Combine nasc.cluster.plot and trawl.proportion.plot for report
  nasc.trawl.cluster.wt.os <- plot_grid(nasc.cluster.plot.os, trawl.catch.plot.os,
                               nrow = 1, labels = c("a)", "b)"))
  
  ggsave(nasc.trawl.cluster.wt.os,
         filename = here("Figs/fig_nasc_trawl_cluster_wt_os.png"),
         width = map.width*2, height = map.height)
}

# Join NASC and cluster length frequency data frames by cluster ----------------
nasc.offshore <- nasc.offshore %>% 
  left_join(select(clf, -lat, -long, -X, -Y), by = c("cluster" = "cluster"))

# Save results
save(nasc.offshore, file = here("Output/cps_nasc_prop_os.Rdata"))

# Summarise nasc data
nasc.summ.os <- nasc.offshore %>% 
  group_by(vessel.orig, transect.name, transect) %>% 
  summarise(
    start     = min(datetime),
    end       = max(datetime),
    duration  = difftime(end, start, units = "hours"),
    n_int     = length(Interval),
    distance  = length(Interval)*nasc.interval/1852,
    lat       = lat[which.min(long)],
    long      = long[which.min(long)],
    mean_nasc = mean(cps.nasc)) %>% 
  arrange(vessel.orig, start) %>% 
  rename(vessel.name = vessel.orig) %>% 
  ungroup()

save(nasc.summ.os, file = here("Output/nasc_summ_tx_os.Rdata"))

# Apportion offshore backscatter ------------------------------------------
# If enforcing max cluster distance, set proportions for each species to zero
# where cluster.distance > max.cluster.dist
if (limit.cluster.dist["OS"]) {
  nasc.offshore$prop.anch[nasc.offshore$cluster.dist > max.cluster.dist] <- 0
  nasc.offshore$prop.sar[nasc.offshore$cluster.dist  > max.cluster.dist] <- 0
  nasc.offshore$prop.jack[nasc.offshore$cluster.dist > max.cluster.dist] <- 0
  nasc.offshore$prop.mack[nasc.offshore$cluster.dist > max.cluster.dist] <- 0
  nasc.offshore$prop.her[nasc.offshore$cluster.dist  > max.cluster.dist] <- 0  
}

# Create data frame for plotting acoustic proportions by species
nasc.prop.all.os <- nasc.offshore %>%
  mutate(`Engraulis mordax`      = cps.nasc*prop.anch,
         `Sardinops sagax`       = cps.nasc*prop.sar,
         `Trachurus symmetricus` = cps.nasc*prop.jack,
         `Scomber japonicus`     = cps.nasc*prop.mack,
         `Clupea pallasii`       = cps.nasc*prop.her) 

# Prepare nasc.prop.all for facet plotting
nasc.prop.spp.os <- nasc.prop.all.os %>% 
  select(X, Y, `Engraulis mordax`, `Sardinops sagax`, `Trachurus symmetricus`,
         `Scomber japonicus`, `Clupea pallasii`) %>% 
  gather(scientificName, nasc, -X, -Y)

if (save.figs) {
  # Create a base map with relative CPS backscatter
  map.prop.all.os <- base.map + 
    # Plot backscatter for all CPS nasc
    geom_point(data = nasc.prop.all.os, aes(X, Y, size = cps.nasc), 
               colour = "gray20") +
    # Plot proportion of backscatter from each species present
    geom_point(data = filter(nasc.prop.spp.os, nasc > 0),
               aes(X, Y, size = nasc), colour = "red") +
    # Facet by species
    facet_wrap(~scientificName, nrow = 2) +
    theme(strip.background.x = element_blank(),
          strip.text.x       = element_text(face = "italic")) +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # Save map
  ggsave(here("Figs/fig_nasc_acoustic_proportions_os.png"), map.prop.all.os,
         width = map.width*3, height = map.height*2)
  
  # Save plot objects
  save(map.prop.all, file = here("Output/acoustic_proportion_maps_os.Rdata"))
} else {
  # Load plot objects
  load(here("Output/acoustic_proportion_maps_os.Rdata"))
}

# Calculate offshore acoustic biomass density -----------------------------
nasc.offshore <- nasc.offshore %>% 
  mutate(anch.dens = cps.nasc*prop.anch / (4*pi*sigmawg.anch) / 1000,
         her.dens  = cps.nasc*prop.her  / (4*pi*sigmawg.her)  / 1000,
         jack.dens = cps.nasc*prop.jack / (4*pi*sigmawg.jack) / 1000,
         mack.dens = cps.nasc*prop.mack / (4*pi*sigmawg.mack) / 1000,
         sar.dens  = cps.nasc*prop.sar  / (4*pi*sigmawg.sar)  / 1000)

# Check proportions after restricting cluster distance
# hist(nasc.offshore$anch.dens[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$her.dens[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$jack.dens[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$mack.dens[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$sar.dens[nasc.offshore$cluster.distance > 30])
# 
# hist(nasc.offshore$prop.anch[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$prop.her[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$prop.jack[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$prop.mack[nasc.offshore$cluster.distance > 30])
# hist(nasc.offshore$prop.sar[nasc.offshore$cluster.distance > 30])

# Format for plotting
if (limit.cluster.dist["OS"]) {
  nasc.density.os <- nasc.offshore %>%
    filter(cluster.distance <= 30) %>%
    select(lat, long, anch.dens, her.dens, jack.dens, mack.dens,
           sar.dens, transect, transect.name, int, cluster, cluster.distance) %>%
    group_by(transect, transect.name, int) %>%
    summarise(
      lat                     = lat[1],
      long                    = long[1],
      all.clusters            = glue_collapse(unique(cluster), sep = ", "),
      mean.dist               = mean(cluster.distance),
      `Engraulis mordax`      = mean(anch.dens),
      `Clupea pallasii`       = mean(her.dens),
      `Trachurus symmetricus` = mean(jack.dens),
      `Scomber japonicus`     = mean(mack.dens),
      `Sardinops sagax`       = mean(sar.dens)) %>%
    ungroup() %>%
    gather(scientificName, density, -transect, -transect.name, -int, -lat, -long, -mean.dist, -all.clusters) %>%
    mutate(bin       = cut(density,dens.breaks, include.lowest = TRUE),
           bin.level = as.numeric(bin))
} else {
  nasc.density.os <- nasc.offshore %>%
    select(lat, long, anch.dens, her.dens, jack.dens, mack.dens,
           sar.dens, transect, transect.name, int, cluster, cluster.distance) %>%
    group_by(transect, transect.name, int) %>%
    summarise(
      lat                     = lat[1],
      long                    = long[1],
      all.clusters            = glue_collapse(unique(cluster), sep = ", "),
      mean.dist               = mean(cluster.distance),
      `Engraulis mordax`      = mean(anch.dens),
      `Clupea pallasii`       = mean(her.dens),
      `Trachurus symmetricus` = mean(jack.dens),
      `Scomber japonicus`     = mean(mack.dens),
      `Sardinops sagax`       = mean(sar.dens)) %>%
    ungroup() %>%
    gather(scientificName, density, -transect, -transect.name, -int, -lat, -long, -mean.dist, -all.clusters) %>%
    mutate(bin       = cut(density,dens.breaks, include.lowest = TRUE),
           bin.level = as.numeric(bin))
}

# nasc.density.os <- nasc.offshore %>%
#   select(lat, long, anch.dens, her.dens, jack.dens, mack.dens, 
#          sar.dens, transect, transect.name, int, cluster, cluster.distance) %>% 
#   group_by(transect, transect.name, int) %>% 
#   summarise(
#     lat                     = lat[1],
#     long                    = long[1],
#     all.clusters            = glue_collapse(unique(cluster), sep = ", "),
#     mean.dist               = mean(cluster.distance),
#     `Engraulis mordax`      = mean(anch.dens),
#     `Clupea pallasii`       = mean(her.dens),
#     `Trachurus symmetricus` = mean(jack.dens),
#     `Scomber japonicus`     = mean(mack.dens),
#     `Sardinops sagax`       = mean(sar.dens)) %>% 
#   # ungroup() %>% 
#   gather(scientificName, density, -transect, -transect.name, -int, -lat, -long, -mean.dist, -all.clusters) %>% 
#   mutate(bin       = cut(density,dens.breaks, include.lowest = TRUE),
#          bin.level = as.numeric(bin))

# Create acoustic transect labels for maps
tx.labels.tmp.os <- nasc.offshore %>% 
  ungroup() %>%
  group_by(transect) %>%
  summarise(
    vessel.name = vessel.name[1],
    transect.name = transect.name[1],
    start.lat = lat[which.max(long)],
    start.long = max(long),
    end.lat = lat[which.min(long)],
    end.long = min(long),
    brg = 90 - swfscMisc::bearing(end.lat, end.long, start.lat, start.long)[1])

tx.end.labels.os <- tx.labels.tmp.os %>% 
  filter(start.lat < 48.54116) %>% 
  select(vessel.name, transect, transect.name, lat = end.lat, long = end.long, brg) 

tx.start.labels.os <- tx.labels.tmp.os %>% 
  filter(start.lat >= 48.54116) %>% 
  select(vessel.name, transect, transect.name, lat = start.lat, long = start.long, brg) %>% 
  rbind(tx.end.labels.os)

tx.labels.os <- project_df(tx.start.labels.os, to = crs.proj)

# Summarise biomass density by transect and species
nasc.density.summ.os <- nasc.density.os %>% 
  group_by(scientificName, transect, transect.name) %>% 
  summarise(density = sum(density)) %>% 
  filter(scientificName %in% unique(lengths$scientificName)) %>% 
  left_join(select(tx.labels.tmp.os, transect.name, start.lat, start.long)) %>% 
  mutate(positive = density > 0)

# Save biomass density data
save(nasc.density.os, nasc.density.summ.os, 
     file = here("Output/nasc_biomass_density_os.Rdata"))

# Select legend objects 
dens.levels.all.os <- sort(unique(nasc.density.os$bin.level))
dens.labels.all.os <- dens.labels[dens.levels.all.os]
dens.sizes.all.os  <- dens.sizes[dens.levels.all.os]
dens.colors.all.os <- dens.colors[dens.levels.all.os]

# Stratify ----------------------------------------------------------------
# Get transect spacing for plotting ---------------------------------------
# Get the midpoint (mean) lat/long of each transect
tx.mid.os <- nasc.offshore %>% 
  group_by(vessel.name, transect, transect.name) %>% 
  summarise(
    lat  = mean(lat, na.rm = TRUE),
    long = mean(long, na.rm = TRUE))

# Create a data frame for results
tx.nn.os <- data.frame()

for (i in unique(tx.mid.os$transect)) {
  # Get the mid lat/long for each transect
  tx.nn.i      <- filter(tx.mid.os, transect == i)
  # Get NASC data for all other transects
  nasc.temp <- filter(tx.mid.os, transect != i, vessel.name %in% tx.nn.i$vessel.name)
  # Calculate distance between transect midpoint and all other NASC values
  nn.dist   <- swfscMisc::distance(tx.nn.i$lat, tx.nn.i$long,
                                   nasc.temp$lat, nasc.temp$long)
  # Get the transect info and spacing based on shortest distance
  nn.tx     <- nasc.temp$transect[which.min(nn.dist)]
  min.dist  <- nn.dist[which.min(nn.dist)]
  nn.lat    <- nasc.temp$lat[which.min(nn.dist)]
  nn.long   <- nasc.temp$long[which.min(nn.dist)]
  # Add to results
  tx.nn.os     <- bind_rows(tx.nn.os, 
                         data.frame(tx.nn.i, nn.tx, min.dist, nn.lat, nn.long))
}

# Bin transects by spacing
tx.nn.os <- tx.nn.os %>% 
  mutate(dist.bin = cut(tx.nn.os$min.dist, tx.spacing.bins),
         spacing  = tx.spacing.dist[as.numeric(dist.bin)],
         dist.cum = cumsum(spacing)) %>% 
  arrange(transect)

# Save nearest neighbor distance info
save(tx.nn.os, file = here("Output/transect_spacing_os.Rdata"))

# Summarise biomass density by transect and species
nasc.density.summ.os <- nasc.density.summ.os %>% 
  left_join(select(tx.nn.os, transect.name, dist.cum, dist.bin))

# Draw pseudo-transects --------------------------------------------------------
# Get transect ends, calculate bearing, and add transect spacing
tx.ends.os <- nasc.offshore %>% 
  group_by(transect.name, transect, vessel.name) %>% 
  summarise(
    lat.i  = lat[which.max(long)],
    long.i = max(long),
    lat.o  = lat[which.min(long)],
    long.o = min(long)
  ) %>% 
  left_join(select(tx.nn.os, transect.name, spacing)) %>% 
  mutate(
    brg = swfscMisc::bearing(lat.i, long.i,
                             lat.o, long.o)[1])

# Get original inshore transect ends -------------------------------------------
tx.i.os <- tx.ends.os %>% 
  select(-lat.o, -long.o) %>% 
  rename(lat = lat.i, long = long.i) %>% 
  mutate(
    grp = "original",
    loc = "inshore",
    order = 2)

# Get N and S inshore waypoints ------------------------------------------------
# Calculate inshore/north transects
tx.i.n.os <- tx.ends.os %>% 
  select(-lat.o, -long.o) %>% 
  rename(lat = lat.i, long = long.i) %>% 
  mutate(
    lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
    long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
    grp = "north",
    loc = "inshore",
    order = 1)

# Calculate inshore/south transects
tx.i.s.os <- tx.ends.os %>% 
  select(-lat.o, -long.o) %>% 
  rename(lat = lat.i, long = long.i) %>% 
  mutate(
    lat  = destination(lat, long, brg - 90, spacing/2, units = "nm")["lat"],
    long = destination(lat, long, brg - 90, spacing/2, units = "nm")["lon"],
    grp = "south",
    loc = "inshore",
    order = 3)

# Combine all inshore transects
tx.i.os <- tx.i.os %>%
  bind_rows(tx.i.n.os) %>%
  bind_rows(tx.i.s.os) %>%
  arrange(transect, desc(order))

# Get original offshore transect ends ------------------------------------------
tx.o.os <- tx.ends.os %>% 
  select(-lat.i, -long.i) %>% 
  rename(lat = lat.o, long = long.o) %>% 
  mutate(
    grp = "original",
    loc = "offshore",
    order = 2)

# Get N and S offshore waypoints -----------------------------------------------
# Calculate offshore/north transects
tx.o.n.os <- tx.ends.os %>% 
  select(-lat.i, -long.i) %>% 
  rename(lat = lat.o, long = long.o) %>% 
  mutate(
    lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
    long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
    grp = "north",
    loc = "offshore",
    order = 1)

# Calculate inshore/south transects
tx.o.s.os <- tx.ends.os %>% 
  select(-lat.i, -long.i) %>% 
  rename(lat = lat.o, long = long.o) %>% 
  mutate(
    lat  = destination(lat, long, brg - 90, spacing/2, units = "nm")["lat"],
    long = destination(lat, long, brg - 90, spacing/2, units = "nm")["lon"],
    grp = "south",
    loc = "offshore",
    order = 3)

# Combine all offshore transects
tx.o.final.os <- tx.o.os %>% 
  bind_rows(filter(tx.o.n.os)) %>% 
  bind_rows(filter(tx.o.s.os)) %>% 
  arrange(desc(transect), order)

# Assemble the final data frame with all waypoints -----------------------------
strata.points.os <- tx.i.os %>% 
  bind_rows(tx.o.final.os)   %>%
  mutate(key = paste(transect.name, grp)) 

# Convert to points
strata.points.os.sf <- st_as_sf(strata.points.os, coords = c("long","lat"), crs = 4326) 

# Create polygons
strata.super.polygons.os <- strata.points.os.sf %>% 
  ungroup() %>% 
  # group_by(vessel.name) %>% 
  summarise(do_union = F) %>% 
  st_cast("POLYGON") %>% 
  st_make_valid() %>% 
  st_difference(st_union(bathy_5m_poly)) %>% 
  st_difference(filter(strata.super.polygons, vessel.name == "RL")) %>% 
  mutate(area = st_area(.))

# Convert to lines
tx.lines.os.sf <- strata.points.os.sf %>% 
  group_by(transect, grp) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING")

# Determine transect order by min latitude/longitude
use.nums <- FALSE

if (use.nums) {
  # Calculate transect order per vessel
  tx.order.os <- nasc.offshore %>%
    # filter(str_detect(vessel.name, i)) %>% 
    group_by(transect.name) %>% 
    summarise(max.lat  = max(lat),
              min.long = max(long)) %>% 
    mutate(rank.lat  = rank(max.lat), 
           rank.long = rev(rank(min.long)), 
           diff      = rank.lat - rank.long,
           transect.num = as.numeric(str_extract(transect.name,"[[:digit:]]+")),
           rank.final = rank(transect.num)) %>% 
    arrange(rank.final) %>% 
    mutate(transect = transect.num)
  
} else {
  # Calculate transect order per vessel
  tx.order.os <- nasc.offshore %>%
    # filter(str_detect(vessel.name, i)) %>% 
    group_by(transect.name) %>% 
    summarise(max.lat  = max(lat),
              min.long = max(long)) %>% 
    mutate(rank.lat  = rank(max.lat), 
           rank.long = rev(rank(min.long)), 
           diff      = rank.lat - rank.long,
           transect.num = as.numeric(str_extract(transect.name,"[[:digit:]]+")),
           rank.final = rank.lat) %>% 
    arrange(rank.final) %>% 
    mutate(transect = rank.final)
}

# Add transect numbers to NASC data frame
nasc.offshore <- left_join(select(nasc.offshore, -transect), 
                           select(tx.order.os, transect.name, transect))

# Add transect numbers to NASC density summary data frame
nasc.density.summ.os <- nasc.density.summ.os %>% 
  ungroup() %>% 
  select(-transect) %>% 
  left_join(select(tx.order.os, transect.name, transect))

nasc.density.os <- nasc.density.os %>% 
  ungroup() %>% 
  select(-transect) %>% 
  left_join(select(tx.order.os, transect.name, transect))

# Add transect numbers to strata points data frame
strata.points.os <- strata.points.os %>% 
  ungroup() %>% 
  select(-transect) %>% 
  left_join(select(tx.order.os, transect.name, transect))

# Offshore strata
strata.manual.os <- bind_rows(
  data.frame(
    scientificName = "Clupea pallasii",
    stratum = 1,
    transect = 1:n_distinct(nasc.offshore$transect)),
  data.frame(
    scientificName = "Engraulis mordax",
    stratum = 1,
    transect = 1:n_distinct(nasc.offshore$transect)),
  data.frame(
    scientificName = "Sardinops sagax",
    stratum = 1,
    transect = 1:n_distinct(nasc.offshore$transect)),
  data.frame(
    scientificName = "Scomber japonicus",
    stratum = 1,
    transect = 1:n_distinct(nasc.offshore$transect)),
  data.frame(
    scientificName = "Trachurus symmetricus",
    stratum = 1,
    transect = 1:n_distinct(nasc.offshore$transect))
)

# Create stratum polygons -------------------------------------------------
# Define sampling strata
if (stratify.manually.os) {
  # Use manually defined strata
  strata.final.os <- strata.manual.os %>%
    mutate(stratum.orig = stratum)
  
} else {
  # Define strata automatically
  strata.final.os <- data.frame()

  # Define strata boundaries and transects for each species
  for (i in unique(nasc.density.summ.os$scientificName)) {
    # Select positive transects and calculate differences between transect numbers
    # diffs >= 2 define stratum breaks
    temp.spp <- filter(nasc.density.summ.os, scientificName == i) %>%
      filter(positive == TRUE) %>%
      mutate(diff = c(1, diff(transect))) %>%
      ungroup()

    if (nrow(temp.spp) > 1) {
      # Find the start of each positive stratum
      spp.starts <- temp.spp %>%
        filter(diff >= max.diff)

      # If the start of the stratum == 1, stratum start is 1, else min transect number
      survey.start <- ifelse(min(temp.spp$transect) == 1, 1, min(temp.spp$transect) - 1)

      # A vector of stratum starts
      stratum.start <- c(survey.start, spp.starts$transect - 1)

      # If the end of the stratum is the last transect in the survey,
      # select the last, else the last transect + 1
      survey.end <- ifelse(max(temp.spp$transect) == max(nasc.density.summ.os$transect),
                           max(nasc.density.summ.os$transect),
                           max(temp.spp$transect) + 1)

      # A vector of stratum ends
      stratum.end <- c(temp.spp$transect[which(temp.spp$diff >= max.diff) - 1] + 1,
                       survey.end)
      
      strata.cull <- temp.spp %>% 
        mutate(tx.bins = cut(transect, c(stratum.start, survey.end), include.lowest = TRUE, right = TRUE)) %>% 
        group_by(tx.bins) %>% 
        tally()

      # Combine starts and ends in to a data frame for plotting and generating stratum vectors
      strata.spp <- data.frame(scientificName = i,
                               stratum = seq(1, length(stratum.start)),
                               start = stratum.start,
                               end = stratum.end) %>%
        mutate(n.tx = end - start + 1)

      # Create stratum vectors
      strata.df <- data.frame()
      
      for (j in 1:nrow(strata.spp)) {
        # Create a vector of transects from start to end
        transect <- seq(strata.spp$start[j], strata.spp$end[j])

        # Combine results
        strata.df <- bind_rows(strata.df,
                               data.frame(scientificName = i,
                                          stratum = j,
                                          transect))
      }

      # Add vessel name and distance bin to strata.df for final cuts
      strata.df <- strata.df %>%
        left_join(filter(select(nasc.density.summ.os, scientificName, transect, dist.bin),
                         scientificName == i)) %>%
        mutate(stratum.key = factor(paste(stratum, dist.bin)))

      # Summarise strata by key, remove strata with less than 3 transects,
      # and reassign stratum numbers
      strata.df.summ <- strata.df %>%
        group_by(stratum.key) %>%
        summarise(n.tx = n()) %>%
        filter(n.tx >= nTx.min) %>%
        mutate(stratum = seq(1, n()))

      # Remove strata with less than minimum number of transects
      strata.df <- strata.df %>%
        rename(stratum.orig = stratum) %>%
        filter(stratum.key %in% strata.df.summ$stratum.key) %>%
        left_join(select(strata.df.summ, stratum.key, stratum))

      # Combine with stratum vectors for other species
      strata.final.os <- bind_rows(strata.final.os, strata.df)
    }
  }
}

# Create acoustic transect labels for offshore maps
tx.labels.os <- nasc.offshore %>% 
  group_by(transect) %>% 
  summarise(
    vessel.name = vessel.name[1],
    transect.name = transect.name[1],
    start.lat = lat[which.max(long)],
    start.long = max(long),
    end.lat = lat[which.min(long)],
    end.long = min(long),
    brg = 90 - swfscMisc::bearing(end.lat, end.long, start.lat, start.long)[1])

# Add start latitude and longitude to strata table
strata.final.os <- strata.final.os %>%
  left_join(select(tx.labels.os, -end.lat, -end.long, -brg)) %>%
  filter(!is.na(vessel.name))

# Create final strata and calculate area
# Create df for transect-level stock info
nasc.stock.os <- data.frame()

# Ungroup tx.ends.os so it joins properly with strata.points.os  
tx.ends.os <- ungroup(tx.ends.os)

if (stock.break.source == "primary") {
  strata.points.os <- strata.points.os %>% 
    ungroup() %>% 
    left_join(ungroup(select(tx.ends.os, transect.name, lat.stock = lat.i)))
} else {
  strata.points.os <- strata.points.os %>% 
    mutate(lat.stock = lat)
}

if (exists("strata.offshore")) rm(strata.offshore)

for (i in unique(strata.final.os$scientificName)) {
  # Select each strata per species
  strata.sub <- filter(strata.final.os, scientificName == i) %>% 
    select(transect.name, stratum)  
  
  # Define strata to stock
  nasc.stock.temp <- strata.points.os %>% 
    filter(loc == "inshore") %>%
    left_join(strata.sub) %>% 
    mutate(stock = case_when(
      i == "Engraulis mordax" & lat.stock >= stock.break.anch ~ "Northern",
      i == "Engraulis mordax" & lat.stock <  stock.break.anch ~ "Central",
      i == "Sardinops sagax"  & lat.stock >= stock.break.sar  ~ "Northern",
      i == "Sardinops sagax"  & lat.stock <  stock.break.sar  ~ "Southern",
      i %in% c("Clupea pallasii","Scomber japonicus","Trachurus symmetricus") ~ "All"),
      scientificName = i) %>% 
    select(transect.name, transect, stock, scientificName) %>% 
    distinct() #%>% filter(!is.na(stock))
  
  # Combine results
  nasc.stock.os <- bind_rows(nasc.stock.os, nasc.stock.temp)
  
  for (j in sort(unique(strata.sub$stratum))) {
    # Create offshore stratum polygons ----------------------------------------
    # Add stratum numbers and stock designation to strata.points
    primary.poly.temp <- strata.points.os %>% 
      left_join(strata.sub) %>%
      left_join(nasc.stock.temp) %>% 
      filter(stratum == j) %>%
      mutate(scientificName = i) %>% 
      ungroup()
    
    # Select the southern-most inshore point for j-th stratum
    primary.poly.j.s <- primary.poly.temp %>%
      filter(loc == "inshore") %>% 
      slice(1)
    
    # Select the northern-most inshore point for j-th stratum
    primary.poly.j.n <- primary.poly.temp %>%
      filter(loc == "inshore") %>% 
      slice(n())
    
    # Select only the original inshore waypoints for j-th stratum
    primary.poly.j.i <- primary.poly.temp %>% 
      filter(loc == "inshore", grp == "original") %>% 
      mutate(scientificName = i)
    
    # Create the final polygon
    primary.poly.j <- primary.poly.temp %>% 
      filter(loc == "offshore") %>% 
      bind_rows(primary.poly.j.s) %>%
      bind_rows(primary.poly.j.i) %>%
      bind_rows(primary.poly.j.n) %>% 
      st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
      group_by(stratum, stock) %>% 
      summarise(do_union = F) %>% 
      st_cast("POLYGON") %>% 
      # st_difference() %>% 
      mutate(scientificName = i)
    
    # Combine with other polygons ----------------------------------------
    if (exists("strata.offshore")) {
      strata.offshore <- rbind(strata.offshore, primary.poly.j)
    } else {
      strata.offshore <- primary.poly.j
    }
  }
}

# Ungroup to make work with filtering and plotting with mapview()
strata.offshore <- ungroup(strata.offshore)

# Save strata polygons
save(strata.offshore, 
     file = here("Output/strata_offshore_raw.Rdata"))

# Clip primary polygons using the 5 m isobathy polygon -------------------------
# Summarise strata transects
strata.summ.os <- strata.final.os %>% 
  # mutate(vessel.name = "RL") %>% 
  group_by(scientificName, stratum) %>% 
  summarise(
    start     = min(transect),
    end       = max(transect)) %>% 
  arrange(scientificName, stratum)

strata.offshore <- strata.offshore %>% 
  st_make_valid() %>% 
  st_difference(st_union(bathy_5m_poly)) %>% 
  st_difference(filter(strata.super.polygons, vessel.name == "RL")) %>% 
  ungroup() %>% 
  mutate(area = st_area(.)) 

# Save clipped primary polygons
save(strata.offshore, 
     file = here("Output/strata_offshore_final.Rdata"))

# Convert polygons to points and add coordinates -------------------------------
strata.offshore.points  <- strata.offshore %>% 
  st_cast("MULTIPOINT") %>%
  st_cast("POINT") %>%
  mutate(
    long = as.data.frame(st_coordinates(.))$X,
    lat = as.data.frame(st_coordinates(.))$Y,
    grp = paste(scientificName, stock, stratum)) %>% 
  st_set_geometry(NULL)

# Save final strata points
save(strata.offshore.points,  
     file = here("Output/strata_points_os.Rdata"))

# Write offshore stata points to CSV
write.csv(strata.offshore.points,  
          file = here("Output/strata_points_os.csv"),
          quote = F, row.names = F)

# Summarize nasc.strata by stock
strata.summ.offshore <- strata.offshore %>% 
  select(scientificName, stratum, stock, area) %>%
  mutate(area = as.numeric(area)) %>% 
  st_set_geometry(NULL)

if (save.figs) {
  # Add strata polygons to acoustic proportions map
  map.stratum.all.os <- base.map + 
    # Plot final strata
    geom_sf(data = strata.offshore, aes(colour = factor(stratum), 
                                       fill = stock), alpha = 0.5) +
    # Plot proportion of backscatter from each species present
    geom_point(data = nasc.prop.all.os, aes(X, Y, size = cps.nasc), 
               colour = "gray20", show.legend = F) +
    geom_point(data = filter(nasc.prop.spp.os, nasc > 0),
               aes(X, Y, size = nasc), colour = "red", show.legend = F) +
    scale_colour_discrete("Stratum") +
    scale_fill_manual("Stock", 
                      values = c("All" = "yellow", "Central" = "green", 
                                 "Northern" = "blue", "Southern" = "orange")) +
    # Facet by species
    facet_wrap(~scientificName, nrow = 2) +
    theme(strip.background.x = element_blank(),
          strip.text.x       = element_text(face = "italic"),
          legend.background  = element_blank()) +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # Save figure
  ggsave(here("Figs/fig_nasc_acoustic_proportions_strata_os.png"), map.stratum.all.os,
         width = map.width*2.6, height = map.height*1.5)
}

# Convet nasc.density to sf and project
nasc.density.os <- ungroup(nasc.density.os) %>% 
  project_df(to = crs.proj)

# Summarize positive clusters to filter clusters from removed strata
pos.cluster.summ <- pos.clusters %>% 
  group_by(scientificName, cluster) %>% 
  summarise(nIndiv = sum(num)) %>% 
  filter(nIndiv >= nIndiv.min)

pos.clusters.os <- pos.clusters

nasc.os.clusters <- sort(unique(nasc.offshore$cluster))

if (save.figs) {
  # Create a list for saving biomass density plots
  biomass.dens.figs.os <- list()
  
  # Plot biomass density
  for (i in unique(strata.offshore$scientificName)) {
    for (j in unique(filter(strata.offshore, scientificName == i)$stock)) {
      # Filter and subset nasc.stock.os per species and stratum
      nasc.stock.os.temp <- filter(nasc.stock.os, scientificName == i, stock == j) %>% 
        select(-transect, -scientificName)
      
      # Filter biomass density
      nasc.density.plot.os <- nasc.density.os %>%
        left_join(nasc.stock.os.temp) %>% 
        filter(density != 0, scientificName == i, 
               stock == j, transect.name %in% strata.final.os$transect.name[strata.final.os$scientificName == i])
      
      # Filter positive clusters
      pos.cluster.txt <- pos.clusters.os %>% 
        filter(scientificName == i, stock == j,
               cluster %in% nasc.os.clusters) %>% 
        ungroup() %>% 
        project_df(to = crs.proj)
      
      # Map biomass density, strata polygons, and positive trawl clusters
      biomass.dens.os <- base.map +
        geom_sf(data = filter(strata.offshore, scientificName == i, stock == j),
                aes(colour = factor(stratum)), fill = NA, size = 1) +
        scale_colour_discrete('Stratum') + 
        # Plot vessel track
        geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) +
        # Plot zero nasc data
        geom_point(data = filter(nasc.offshore, cps.nasc == 0), aes(X, Y),
                   colour = 'gray50', size = 0.15, alpha = 0.5) +
        # Plot NASC data
        geom_point(data = nasc.density.plot.os, aes(X, Y, size = bin, fill = bin),
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
        # geom_shadowtext(data = cluster.pie.os,
        #                 aes(X, Y, label = cluster), 
        #                 colour = "red", bg.colour = "white", size = 2) +
        # Configure legend guides
        guides(colour = guide_legend(order = 1),
               fill   = guide_legend(order = 2), 
               size   = guide_legend(order = 2)) +
        coord_sf(crs = crs.proj, 
                 xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
                 ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
      
      # Check plot
      # ggplot() +
      #   # Plot zero nasc data
      #   geom_point(data = filter(nasc.offshore, cps.nasc == 0), aes(X, Y),
      #              colour = 'gray50', size = 0.15, alpha = 0.5) +
      #   # Plot NASC data
      #   geom_point(data = nasc.density.plot.os, aes(X, Y, size = bin, fill = bin),
      #              shape = 21, alpha = 0.75) +
      #   # Configure size and colour scales
      #   scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
      #                     values = dens.sizes.all, labels = dens.labels.all) +
      #   scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
      #                     values = dens.colors.all, labels = dens.labels.all) +
      #   coord_sf(crs = crs.proj, 
      #            xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
      #            ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
      
      # Save figures
      ggsave(biomass.dens.os, 
             filename = paste(here("Figs/fig_biomass_dens_os_"), i, "-", j, ".png",sep = ""),
             width  = map.width,height = map.height)
      
      # Save plot to list
      biomass.dens.figs.os[[i]][[j]] <- biomass.dens.os
    }
  }
  
  # Save map objects
  save(biomass.dens.figs.os, file = here("Output/biomass_dens_os_map_all.Rdata"))  
} else {
  # Load map objects
  load(here("Output/biomass_dens_os_map_all.Rdata"))
}

# Create blank plots for missing species
for (i in cps.spp) {
  if (is.null(biomass.dens.figs.os[[i]])) {
    df <- data.frame()
    biomass.dens.temp <- ggplot(df) + geom_point() + 
      xlim(0,10) + ylim(0,10) + 
      annotate('text', 5, 5, label = 'No Data', size = 6, fontface = 'bold') +
      theme_bw()  
    ggsave(biomass.dens.temp, 
           filename = paste(here("Figs/fig_biomass_dens_os_"), i, ".png", sep = ""))
  }
}

# Save final nasc data frame used for point and bootstrap estimates
save(nasc.offshore, file = here("Output/nasc_offshore_final.Rdata"))

# Create new list for computing biomass estimates
point.estimates.offshore <- data.frame()
nasc.summ.strata.os      <- data.frame()

# Calculate point estimates for each species
for (i in unique(strata.final.os$scientificName)) {
  # Subset strata for species i
  strata.temp <- filter(strata.final.os, scientificName == i) %>% 
    select(transect.name, stratum)
  
  # Add stratum numbers to nasc
  nasc.os.temp <- nasc.offshore %>%
    select(-stratum) %>% 
    left_join(strata.temp) %>% 
    filter(!is.na(stratum))
  
  # Check proportions after restricting cluster distance
  # hist(nasc.os.temp$anch.dens[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$her.dens[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$jack.dens[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$mack.dens[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$sar.dens[nasc.os.temp$cluster.distance > 30])
  # 
  # hist(nasc.os.temp$prop.anch[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$prop.her[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$prop.jack[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$prop.mack[nasc.os.temp$cluster.distance > 30])
  # hist(nasc.os.temp$prop.sar[nasc.os.temp$cluster.distance > 30])
  
  # Summarise nasc by stratum
  nasc.os.temp.summ <- nasc.os.temp %>% 
    group_by(stratum) %>% 
    summarise(
      n_samples = n(),
      mean_nasc = mean(cps.nasc)) %>% 
    mutate(scientificName = i) %>% 
    select(scientificName, everything())
  
  # Combine nasc summaries
  nasc.summ.strata.os <- bind_rows(nasc.summ.strata.os, 
                                nasc.os.temp.summ)
  
  # Create data frame with stratum and area (m^2)
  stratum.info.offshore <- strata.offshore %>% 
    filter(scientificName == i) %>% 
    select(stratum, area) %>%
    mutate(area = as.numeric(area)) %>% 
    st_set_geometry(NULL)
  
  # Compute point estimates
  # Currently has na.rm = TRUE for calculting biomass
  point.estimates.offshore <- bind_rows(point.estimates.offshore,
                                         data.frame(scientificName = i,
                                                    estimate_point(nasc.os.temp, stratum.info.offshore, species = i)))
}

save(point.estimates.offshore, 
     file = here("Output/biomass_point_estimates_os.Rdata"))

# Save strata nasc summaries to CSV
write_csv(nasc.summ.strata.os, here("Output/nasc_strata_summary_os.csv"))

# Add stock designations to point estimates
point.estimates.offshore <- left_join(point.estimates.offshore, strata.summ.offshore)

# Summarize point estimates (by stocks)
pe.os <- point.estimates.offshore %>%
  group_by(scientificName, stock) %>%
  summarise(
    area    = sum(area),
    biomass.total = sum(biomass.total)) %>%
  bind_rows(point.estimates.offshore) %>%
  mutate(area = area * 2.915533e-07) %>%
  arrange(scientificName, stock, stratum) %>%
  replace_na(list(stratum = "All")) %>%
  select(scientificName, stock, stratum, area, biomass.total) %>%
  rename(
    Species            = scientificName,
    Stock              = stock,
    Stratum            = stratum,
    Area               = area,
    biomass.mean.point = biomass.total)

# Save point estimates
save(pe.os, file = here("Output/biomass_point_estimates_os_final.Rdata"))
write_csv(pe.os, here("Output/biomass_point_estimates_os_final.csv"))

# Bootstrap estimates -----------------------------------------------------
# Generate multiple bootstrap biomass estimates
if (do.bootstrap) {
  # Create data frame for biomass estimates
  bootstrap.estimates.os <- data.frame()
  # Create data frame for abundance estimates by length
  abundance.estimates.os <- data.frame()
  # Create data frame for stratum summaries
  survey.summary.os <- data.frame()
  # Create data frame for catch summaries
  catch.summary.os <- data.frame()
  # Create data frame for stratum summaries
  stratum.summary.os <- data.frame()
  
  # Configure progress bar
  pb1 <- tkProgressBar("R Progress Bar", 
                       "Multiple Bootstrap Estimation (Offshore) - Species", 0, 100, 0)
  spp.counter <- 1
  
  for (i in unique(strata.offshore$scientificName)) {
    # Get vector of lengths from clf.df column names
    L.cols  <- grep("L\\d", names(cluster.final[[i]]))
    L.vec   <- sort(as.numeric(str_extract(names(cluster.final[[i]][L.cols]),"\\d{1,2}")))
    
    # Create data frame with stratum and area (m^2)
    strata.info.offshore <- strata.offshore %>% 
      filter(scientificName == i) %>% 
      select(stratum, area) %>%
      mutate(area = as.numeric(area)) %>% 
      st_set_geometry(NULL)
    
    # Subset strata for species i
    strata.temp <- filter(strata.final.os, scientificName == i) %>% 
      select(transect.name, stratum) %>% 
      left_join(filter(strata.summ.offshore, scientificName == i)) %>% 
      filter(!is.na(area))
    
    # Add stratum numbers to nasc and remove transects outside of defined strata
    nasc.temp <- nasc.offshore %>%
      select(-stratum) %>% 
      left_join(select(strata.temp, transect.name, stratum, stock)) %>% 
      filter(!is.na(stratum))

    # Summarize nasc.temp to get strata to merge with pos.clusters below
    nasc.temp.summ <- nasc.temp %>% 
      group_by(stratum, cluster) %>% 
      summarise(n = n_distinct(cluster))
    
    # Summarize length data to get number of individuals
    lf.summ.cluster <- lf.final %>% 
      filter(scientificName == i) %>% 
      group_by(cluster) %>% 
      summarise(counts = sum(counts))
    
    # Summarize stratum clusters for all CPS
    stratum.cluster.cps <- nasc.temp %>% 
      group_by(cluster, stratum) %>% 
      summarise(nIntervals = n()) %>% 
      left_join(lf.summ.cluster)
    
    # Summarize positive clusters per species
    pos.cluster.spp <- pos.clusters.os %>%
      filter(cluster %in% nasc.temp$cluster & scientificName == i) %>% 
      inner_join(select(nasc.temp.summ, -n)) %>% 
      as.data.frame()
    
    # Summarize positive clusters per strata
    stratum.cluster.spp <- pos.cluster.spp %>% 
      group_by(scientificName, stratum) %>% 
      summarise(nClusters = n_distinct(cluster))
    
    # Summarize stratum statistics
    survey.summ.temp <- nasc.temp %>% 
      filter(!is.na(stock)) %>% 
      group_by(stratum, stock) %>% 
      summarise(
        nTransects     = n_distinct(transect),
        Distance       = length(Interval)*100/1852) %>% 
      mutate(Species = i) %>% 
      left_join(stratum.cluster.spp) %>% 
      rename(Stratum = stratum)

    # Summarize catch statistics by stratum
    catch.summ.temp <- n.summ.haul %>% 
      left_join(select(stratum.cluster.cps, cluster, stratum)) %>% 
      filter(scientificName == i, !is.na(stratum)) %>%
      group_by(scientificName, stratum) %>% 
      summarise(nIndiv = sum(num)) %>% 
      as.data.frame()
    
    # Configure progress bar
    pb2 <- tkProgressBar("R Progress Bar", "Multiple Bootstrap Estimation (Offshore) - Stratum", 0, 100, 0)
    
    # Initialize species counter
    stratum.counter <- 1
    
    # Estimate biomass for each stratum
    for (j in unique(nasc.temp$stratum)) {
      # Extract stratum area
      stratum.area <- as.numeric(strata.offshore$area[strata.offshore$scientificName == i & 
                                                         strata.offshore$stratum == j])
      # Calculate biomass using bootstrap function ----
      set.seed(1) # Set seed for repeatable results
      boot.df <- estimate_bootstrap(nasc.temp, cluster.final[[i]], j, 
                                    stratum.area = stratum.area, 
                                    species = i, do.lf = do.lf, 
                                    boot.number = boot.num)$data.frame

      # Extract biomass estimates; remove first (point) estimate
      boot.temp <- data.frame(Species = i, Stratum = j, Area = stratum.area,
                              Sample = seq(1,boot.num), boot.df[2:nrow(boot.df), ])

      # Combine results
      bootstrap.estimates.os <- bind_rows(bootstrap.estimates.os, boot.temp)

      # Calculate abundance by length class using bootstrap function ----
      abund.vec <- estimate_bootstrap(nasc.offshore, cluster.final[[i]], j, 
                                      stratum.area = stratum.area, 
                                      species = i, do.lf = do.lf, 
                                      boot.number = 0)$abundance.vector
      # Extract abundance estimates
      abundance.temp <- data.frame(Species = i, Stratum = j,
                                   SL = L.vec, freq = abund.vec)
      # Combine results
      abundance.estimates.os <- bind_rows(abundance.estimates.os, abundance.temp)
      
      # Update the progress bar
      pb.prog2 <- round(stratum.counter/n_distinct(nasc.temp$stratum)*100)
      info2 <- sprintf("%d%% done", pb.prog2)
      
      setTkProgressBar(pb2, pb.prog2, sprintf("Bootstrap - Stratum (%s)", info2), info2)
      # Update stratum counter
      stratum.counter <- stratum.counter + 1      
    }
    # Close the stratum counter
    close(pb2)
    
    # Update the progress bar
    pb.prog1 <- round(spp.counter/length(bootstrap.est.spp)*100)
    info1    <- sprintf("%d%% done", pb.prog1)
    
    setTkProgressBar(pb1, pb.prog1, sprintf("Bootstrap - Species (%s)", info1), info1)
    
    # Update the species counter
    spp.counter     <- spp.counter + 1
    # Combine survey summary by species
    survey.summary.os  <- bind_rows(survey.summary.os, survey.summ.temp)
    # Combine survey summary by species
    catch.summary.os   <- bind_rows(catch.summary.os, catch.summ.temp)
    # Combine stratum summary
    stratum.summary.os <- bind_rows(stratum.summary.os, pos.cluster.spp)
  }
  
  # Close the species counter
  close(pb1)
  
  # Save bootstrap results
  save(bootstrap.estimates.os, abundance.estimates.os, survey.summary.os, 
       catch.summary.os, stratum.summary.os,
       file = (here("Output/biomass_bootstrap_est_os.Rdata")))
  
} else{
  # Save bootstrap results
  load(here("Output/biomass_bootstrap_est_os.Rdata"))
  
}

# Rename scientificName column
catch.summary.os <- catch.summary.os %>% 
  left_join(strata.summ.offshore) %>%
  rename(Stock = stock)

# Summarise abundance across strata
abund.summ.os <- abundance.estimates.os %>%
  left_join(strata.summ.offshore, by = c("Species" = "scientificName",
                                          "Stratum" = "stratum")) %>%
  group_by(Species, Stock = stock, SL) %>% 
  summarise(abundance = sum(freq)) %>% 
  mutate(TL = SL)  

# Calculate estimated biomass from from TL and estimated.wg --------------------
# CURRENTLY USING ESTIMATED.WG ESTIMATED FROM TOTAL LENGTH
# MUST UPDATE TO USE ESTIMATED.WG FROM STANDARD LENGTH
abund.summ.os <- abund.summ.os %>% 
  mutate(estimated.wg = estimate_ts(Species, TL)$estimated.wg,
         biomass      = abundance * estimated.wg,
         Region       = "Offshore")

# Create and format abundance vs. length table for all species
L.abund.table.os <- abund.summ.os %>%
  ungroup() %>% 
  select(Species, Stock, Region, SL, Abundance = abundance) %>% 
  kable(format = knitr.format, booktabs = TRUE, escape = F, longtable = TRUE,
        digits = c(0),
        format.args = list(big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"), 
                full_width = F) %>%
  row_spec(0, align = c("c")) %>% 
  collapse_rows(columns = c(1)) %>% 
  scroll_box(height = "500px")

# Save results
save(L.abund.table.os, abund.summ.os, file = here("Output/abundance_table_all_os.Rdata"))

# Add stock designations to bootstrap estimates
bootstrap.estimates.os <- bootstrap.estimates.os %>% 
  left_join(strata.summ.offshore, by = c("Species" = "scientificName",
                                          "Stratum" = "stratum")) %>% 
  rename(Stock = stock)

# Summarize results from bootstrap per species and stratum
be.stratum.os <- bootstrap.estimates.os %>% 
  group_by(Species, Stock, Stratum) %>% 
  summarise(
    n.samples         = n(),
    Area              = Area[1]*2.915533e-07, # Convert m^2 to nmi^2
    biomass.mean.boot = mean(biomass)*10^3,
    biomass.sd        = sd(biomass)*10^3,
    biomass.se        = biomass.sd/sqrt(n.samples),
    abund.mean.boot   = mean(abundance),
    abund.sd          = sd(abundance),
    abund.se          = abund.sd/sqrt(n.samples),
    lower.ci.B        = quantile(biomass,probs = 0.025)*10^3,
    upper.ci.B        = quantile(biomass,probs = 0.975)*10^3) %>% 
  mutate(pct.tot.B    = biomass.mean.boot/sum(biomass.mean.boot)*100) %>%
  left_join(survey.summary.os, by = c("Species","Stratum")) %>%
  left_join(catch.summary.os,  by = c("Species" = "scientificName",
                                      "Stratum" = "stratum", "Stock")) %>% 
  arrange(Species, Stratum) %>%
  select(
    Species, Stock, Stratum, Area, nTransects, Distance, nClusters, nIndiv, 
    biomass.mean.boot, lower.ci.B, upper.ci.B, biomass.sd, pct.tot.B)

# Save results
save(be.stratum.os, 
     file = here("Output/biomass_bootstrap_estimates_stratum_os.Rdata"))

# Add stock designations to stratum summary
stratum.summary.os <- stratum.summary.os %>% 
  left_join(strata.summ.offshore) %>% 
  rename(Stock = stock)

# Summarize clusters for each species for entire survey
cluster.summary.total.os <- stratum.summary.os %>%
  group_by(scientificName, Stock) %>% 
  summarise(nClusters = n_distinct(cluster)) %>% 
  rename(Species = scientificName)

# Summarize sampling for entire survey
survey.summary.total.os <- survey.summary.os %>% 
  group_by(Species, Stock = stock) %>% 
  summarise(
    Strata     = n(),
    nTransects = sum(nTransects),
    Distance   = sum(Distance)) %>% 
  left_join(cluster.summary.total.os)

# Summarise catch for entire survey
catch.summary.total.os <- catch.summary.os %>% 
  group_by(Species = scientificName, Stock) %>% 
  summarise(nIndiv = sum(nIndiv))

# Summarize results from bootstrap per species across all samples
be.sample.os <- bootstrap.estimates.os %>% 
  group_by(Species, Stock, Sample) %>% 
  summarise(area    = sum(Area)*2.915533e-07,
            biomass = sum(biomass))

# Summarize results from bootstrap per species across all strata
be.survey.os <- be.sample.os %>%
  group_by(Species, Stock) %>%
  summarise(
    Area = area[1],
    biomass.mean.boot = mean(biomass) * 10^3,
    biomass.sd = sd(biomass) * 10^3,
    lower.ci.B = quantile(biomass, probs = 0.025) * 10^3,
    upper.ci.B = quantile(biomass, probs = 0.975) * 10^3) %>%
  left_join(survey.summary.total.os) %>%
  left_join(catch.summary.total.os) %>%
  arrange(Species) %>%
  select(
    Species, Stock, Area, nTransects, Distance, nClusters, nIndiv, 
    biomass.mean.boot, lower.ci.B, upper.ci.B, biomass.sd
  )

# Save results
write.csv(be.survey.os, 
          file  = here("Output/biomass_bootstrap_estimates_survey_os.csv"),
          quote = F,row.names = F)

save(be.survey.os, file = here("Output/biomass_bootstrap_estimates_survey_os.Rdata"))

# Combine stratum and survey estimates
be.os <- be.stratum.os %>% 
  bind_rows(be.survey.os) %>% 
  arrange(Species, Stock, Stratum) %>% 
  replace_na(list(Stratum = "All", pct.tot.B = 100)) %>%
  left_join(select(pe.os, -Area)) %>%
  rename(Biomass = biomass.mean.point) %>% 
  mutate(biomass.cv = (biomass.sd / Biomass)*100) %>% 
  select(Species, Stock, Stratum, Area, nTransects, Distance, nClusters, nIndiv,
         Biomass, lower.ci.B, upper.ci.B, biomass.sd, biomass.cv)

be.os[atm:::is.nan.df(be.os)] <- NA

# Save results
write.csv(be.os,
          file = here("Output/biomass_bootstrap_estimates_final_os.csv"),
          quote = F, row.names = F)

save(be.os, 
     file = here("Output/biomass_bootstrap_estimates_final_os.Rdata"))

# Create data frame for database export
be.os %>% 
  mutate(region = "Offshore") %>% 
  bind_rows(be.db.export) -> be.db.export

# Get rows with estimates from all strata
be.stratum.all.os <- which(be.os$Stratum == "All")

# Bootstrap estimates of biomass by species
biomass.histogram.survey.os <- ggplot(be.sample.os, aes(biomass*1e3, fill = Stock)) + 
  geom_histogram(alpha = 0.75) + facet_wrap(Species ~ Stock, scales = "free") + 
  geom_vline(data = filter(pe.os, Stratum == "All"),aes(xintercept = biomass.mean.point)) +
  geom_vline(data = be.survey.os, aes(xintercept = lower.ci.B), linetype = 'dashed') +
  geom_vline(data = be.survey.os, aes(xintercept = upper.ci.B), linetype = 'dashed') +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c(All = "gray50", Central = "orange", 
                               Northern = "navyblue", Southern = "firebrick")) +
  ylab("Count") + xlab(expression(Biomass~(t))) +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

# Save figure
ggsave(biomass.histogram.survey.os,
       filename = here("Figs/fig_biomass_histogram_survey_os.png"),
       height = 8, width = 14)

# Create plots of length-disaggregated abundance and biomass
# Create list for storing plots
L.disagg.plots.os <- list()

# Plot length-disaggrated abundance and biomass by length class for each species
for (i in unique(abund.summ.os$Species)) {
  for (j in unique(abund.summ.os$Stock[abund.summ.os$Species == i])) {
    # Get y-axis limits for abundance and biomass plots
    y.max.abund   <- max(abund.summ.os$abundance[abund.summ.os$Species == i & abund.summ.os$Stock == j]) * 1.1
    y.max.biomass <- max(abund.summ.os$biomass[abund.summ.os$Species == i & abund.summ.os$Stock == j]) * 1.1
    
    if (!is.nan(y.max.abund)) {
      # Create x-axis breaks from TL vector
      max.x         <- max(abund.summ.os$TL[abund.summ.os$Species == i & abund.summ.os$Stock == j])
      x.breaks      <- seq(0, max.x, max.x/10)
      
      # Plot length-disaggregated abundance for each species
      L.abund.os <- ggplot(filter(abund.summ.os, Species == i, Stock == j), aes(TL,abundance)) + 
        geom_bar(stat = 'identity',fill = 'gray50',colour = 'gray20') + 
        scale_x_continuous("Length (cm)", breaks = x.breaks) + 
        scale_y_continuous('Abundance (n)', limits = c(0, y.max.abund),
                           expand = c(0,0), labels = fancy_sci) +
        # facet_wrap(~Stock, nrow = 1) +
        theme_bw() +
        theme(strip.background.x = element_blank(),
              strip.text.x = element_text(face = "bold"))
      
      # Plot length-disaggregated biomass for each species
      L.biomass.os <- ggplot(filter(abund.summ.os, Species == i, Stock == j), aes(TL, biomass)) + 
        geom_bar(stat = 'identity', fill = 'gray50', colour = 'gray20') + 
        scale_x_continuous("Length (cm)", breaks = x.breaks) + 
        scale_y_continuous('Biomass (t)', limits = c(0, y.max.biomass),
                           expand = c(0,0), labels = fancy_sci) +
        # facet_wrap(~Stock, nrow = 1) +
        theme_bw() + 
        theme(strip.background.x = element_blank(),
              strip.text.x = element_text(face = "bold"))
      
      # Arrange all plots
      L.disagg.all.os <- plot_grid(L.abund.os, L.biomass.os, ncol = 1, align = 'h')
      
      # Save plot
      ggsave(L.disagg.all.os, 
             filename = paste0(here("Figs/fig_L_disagg_os_"), i, "-", j, ".png"), 
             height = 6, width = 6)
      
      # Add plot to list
      L.disagg.plots.os[[i]][[j]] <- L.disagg.all.os 
    }
  }
}

# Create blank plots for missing species
for (i in cps.spp) {
  if (is.null(L.disagg.plots.os[[i]])) {
    df <- data.frame()
    L.disagg.temp.os <- ggplot(df) + geom_point() + 
      xlim(0,10) + ylim(0,10) + 
      annotate('text',5,5,label = 'No Data', size = 6, fontface = 'bold') +
      theme_bw() + ggtitle(i)  
    ggsave(L.disagg.temp.os, 
           filename = paste0(here("Figs/fig_L_disagg_os_"), i, ".png"))
  }
}

## Examine backscatter data for outliers --------------------------------------
# Select top 100 nasc values and look for outliers
big.nasc.os <- nasc.offshore %>%
  arrange(desc(cps.nasc)) %>%
  mutate(cps.nasc = cps.nasc/19,
         rank = seq(n()),
         label = paste0('Transect: ', transect.name,
                        ' - Distance: ', round(dist_m), " m"),
         popup = paste0('<b>Transect: </b>', transect.name, '<br/>',
                        '<b>Time: </b>', min(datetime), "-", max(datetime), ' UTC<br/>',
                        '<b>Distance: </b>', round(dist_m), ' m<br/>',
                        '<b>NASC: </b>', round(NASC), ' m<sup>2</sup> nmi<sup>-2</sup>')) %>% 
  top_n(100, cps.nasc) %>% 
  select(rank, cps.nasc, label, popup, datetime, dist_m, sounder, 
         lat, long, transect.name, vessel.name) 

# Create outlier plot
nasc.outlier.plot.os <- ggplot(big.nasc.os, aes(rank, cps.nasc, ids = label)) +
  geom_point(aes(colour = vessel.name)) +
  geom_text_repel(data = top_n(big.nasc.os, 20, cps.nasc), 
                  aes(rank, cps.nasc, label = label), size = 2) +
  scale_color_discrete("Vessel") +
  xlab("\nRank") + ylab(expression(italic(s)[A]/19)) +
  theme_bw() +
  theme(legend.position      = c(0.95,0.95),
        legend.justification = c(1,1))

if (save.figs) {
  # Save figure
  ggsave(nasc.outlier.plot.os, filename = here("Figs/fig_nasc_outliers_os.png"), 
         height = 3, width = 5)
}

# Plot sA for CPS -------------------------------------------
# Create vessel paths for plotting
nav.paths.os <- nasc.offshore %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(vessel.orig) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  filter(vessel.orig != survey.vessel.primary)

# Select plot levels for backscatter data
nasc.levels.all <- unique(nasc.plot.os$bin.level)
nasc.labels.all <- nasc.labels[sort(nasc.levels.all)]
nasc.sizes.all  <- nasc.sizes[sort(nasc.levels.all)]
nasc.colors.all <- nasc.colors[sort(nasc.levels.all)]

if (save.figs) {
  # Map backscatter
  nasc.map.cps.os <- base.map +
    # Plot transects data
    geom_sf(data = filter(transects.sf, Type == "Offshore"), 
            size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # plot ship track data
    geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
    geom_sf(data = nav.paths.os, colour = "gray50", size = 0.5, alpha = 0.5) +
    # Plot NASC data
    geom_point(data = nasc.plot.os, aes(X, Y, size = bin, fill = bin), 
               shape = 21, alpha = 0.75) +
    # Configure size and colour scales
    scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                      values = nasc.sizes.all,labels = nasc.labels.all) +
    scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                      values = nasc.colors.all,labels = nasc.labels.all) +
    # Configure legend guides
    guides(fill = guide_legend(), size = guide_legend()) +
    # Plot title
    # ggtitle("CPS Backscatter-Offshore") +
    coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # Save nasc plot
  ggsave(nasc.map.cps.os,
         filename = here("Figs/fig_backscatter_cps_os.png"),
         width = map.width, height = map.height) 
  
  save(nasc.map.cps.os, file = here("Output/nasc_plot_cps_os.Rdata"))
}
