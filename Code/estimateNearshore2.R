# Estimate nearshore biomass -----------------------------------------------
# Remove any existing nearshore backscatter data from memory
if (exists("nasc.nearshore")) {rm(nasc.nearshore)}

# Process offshore backscatter data
if (process.nearshore) {
  # List nearshore backscatter files
  nearshore.files <- dir_ls(here("Data/Backscatter"), recurse = TRUE,
                            regexp = "nearshore.rds")
  
  # Import all offshore backscatter data
  for (nn in nearshore.files) {
    if (exists("nasc.nearshore")) {
      nasc.nearshore <- bind_rows(nasc.nearshore, readRDS(nearshore.files[nn]))
    } else {
      nasc.nearshore <- readRDS(nearshore.files[nn])
    }
  }
  
  # Remove vessels not to be included in the offshore biomass estimates
  nasc.nearshore <- nasc.nearshore %>% 
    filter(vessel.name %in% nasc.vessels.nearshore) %>%
    mutate(
      id            = seq_along(Interval),
      transect      = str_replace(transect, "N", ""),
      transect.name = paste(vessel.name, transect),
      cps.nasc      = NASC.50,
      stratum       = 1,
      int = cut(Interval, seq(1, max(Interval) + nasc.summ.interval,
                              nasc.summ.interval),
                labels = F, include.lowest = TRUE)) %>% 
    mutate(transect = as.numeric(transect))
  
  # Combine nasc data for all NASC vessels
  if (merge.vessels["NS"]) {
    nasc.nearshore <- nasc.nearshore %>% 
      mutate(vessel.name = "RL")
  } else {
    nasc.nearshore <- nasc.nearshore %>% 
      mutate(vessel.orig = vessel.name)
  }
  
  # Export data for processing using the CTD app
  write_csv(nasc.nearshore, here("Output/CTDapp/CTDapp_All_Nearshore.csv"))
  
  # Summarise nasc for plotting ---------------------------------------------
  nasc.plot.ns <- nasc.nearshore %>% 
    select(filename, vessel.name, transect, transect.name, int, lat, long, cps.nasc) %>% 
    group_by(filename, vessel.name, transect.name, transect, int) %>% 
    summarise(
      lat  = lat[1],
      long = long[1],
      NASC = mean(cps.nasc)) %>% 
    # Create bins for defining point size in NASC plots%>% 
    mutate(bin       = cut(NASC, nasc.breaks, include.lowest = TRUE),
           bin.level =  as.numeric(bin)) %>% 
    ungroup() %>% 
    project_df(to = crs.proj)
  
  # Apply cps.nasc, or use a fixed integration depth
  if (source.cps.nasc["NS"]) {
    # Use exteranlly supplied cps.nasc with variable integration depth (from CTD.app)
    # Read file and create unique key for joining with nasc.vessel
    cps.nasc.temp <- read.csv(data.cps.nasc["NS"]) %>% 
      mutate(key = paste(lat, long, dist_m),
             datetime = ymd_hms(paste(date, time))) %>% 
      # Remove data from krill files (1807RL)
      filter(!str_detect(tolower(filename), "krill")) 
    
    # Join nasc.vessel and cps.nasc on datetime
    nasc.nearshore <- nasc.nearshore %>% 
      left_join(select(cps.nasc.temp, datetime, cps.nasc))
    
  } else {
    nasc.nearshore <- nasc.nearshore %>%
      mutate(cps.nasc = NASC.50)
  }

  # Assign backscatter to trawl clusters ------------------------------------
  # Create varialble for nearest cluster and minumum distance
  cluster.distance.ns <- data.frame(cluster = rep(NA, nrow(nasc.nearshore)),
                                    cluster.distance = rep(NA, nrow(nasc.nearshore)))
  # Configure progress bar
  pb <- tkProgressBar("R Progress Bar", "Cluster Assignment", 0, 100, 0)
  
  # Assign trawl clusters
  for (i in 1:nrow(nasc.nearshore)) {
    # Calculate distance between each NASC interval and all trawl clusters
    temp.distance <- distance(nasc.nearshore$lat[i], nasc.nearshore$long[i], 
                              super.clusters$lat, super.clusters$long, 
                              units = "nm")
    
    # Assign cluster with minimum distance to NASC interval
    cluster.distance.ns$cluster[i]          <- super.clusters$cluster[which.min(temp.distance)]
    cluster.distance.ns$cluster.distance[i] <- temp.distance[which.min(temp.distance)]
    
    # Update progress bar
    pb.prog <- round(i/nrow(nasc.nearshore)*100)
    info <- sprintf("%d%% done", pb.prog)
    setTkProgressBar(pb, pb.prog, sprintf("Cluster Assignment (%s)", info), info)
  }
  
  # Close progress bar
  close(pb)
  
  # Add cluster distances to nasc
  nasc.nearshore <- bind_cols(nasc.nearshore, cluster.distance.ns) %>% 
    project_df(to = crs.proj)
  
  # Save nasc cluster vector
  save(cluster.distance.ns, file = here("Output/nasc_cluster_distance_nearshore.Rdata"))
  
  # Save results of processing
  save(nasc.nearshore, file = here("Data/Backscatter/nasc_nearshore.Rdata"))
  
} else {
  # Load processed data
  load(here("Data/Backscatter/nasc_nearshore.Rdata"))
}

# Summarise nasc for plotting ---------------------------------------------
nasc.plot.ns <- nasc.nearshore %>% 
  select(filename, vessel.name, transect, transect.name, int, lat, long, cps.nasc) %>% 
  group_by(filename, vessel.name, transect.name, transect, int) %>% 
  summarise(
    lat  = lat[1],
    long = long[1],
    NASC = mean(cps.nasc)) %>% 
  # Create bins for defining point size in NASC plots%>% 
  mutate(bin       = cut(NASC, nasc.breaks, include.lowest = TRUE),
         bin.level =  as.numeric(bin)) %>% 
  ungroup() %>% 
  project_df(to = crs.proj)

# Plot data coverage
# ggplot(nasc.plot.ns, aes(long, lat, colour = vessel.name)) + geom_point() + facet_wrap(~vessel.name) + coord_map()

# Summarize numbers of intervals per trawl cluster
nasc.nearshore.summ <- nasc.nearshore %>% 
  group_by(cluster) %>% 
  tally() %>% 
  filter(n >= 4) %>% 
  ungroup()

# Map trawl clusters -------------------------------------------------------
# Create hulls around positive clusters
nasc.super.clusters.ns <- nasc.nearshore %>% 
  filter(cluster %in% nasc.nearshore.summ$cluster) %>% 
  plyr::ddply("cluster", find_hull) %>% 
  select(long, lat, cluster) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(cluster) %>% 
  summarise(do_union = F) %>% 
  st_cast("POLYGON") %>% 
  ungroup()

if (save.figs) {
  nasc.cluster.plot.ns <- base.map +
    # Plot nasc data
    geom_point(data = nasc.nearshore, aes(X, Y, colour = factor(cluster)),
               size = 0.5, show.legend = FALSE) +
    # Plot convex hull around NASC clusters
    geom_sf(data = nasc.super.clusters.ns, aes(fill = factor(cluster)),
            colour = 'black', alpha = 0.5, show.legend = FALSE) +
    scale_fill_discrete(name = "Cluster") +
    scale_colour_discrete(name = "Cluster") +
    # Plot cluster midpoints
    geom_text(data = cluster.mid, aes(X, Y, label = cluster),
              colour = 'gray20', size = 2) +
    # Plot positive trawl cluster midpoints
    geom_shadowtext(data = filter(cluster.mid, cluster %in% super.clusters$cluster), 
                    aes(X, Y, label = cluster), 
                    size = 2, colour = "black", bg.colour = "white") +
    # Plot panel label
    ggtitle("Integrated NASC Clusters-Nearshore") +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # save trawl plot
  ggsave(nasc.cluster.plot.ns,
         filename = here("Figs/fig_nasc_cluster_map_ns.png"),
         width = map.width, height = map.height)
  
  save(nasc.cluster.plot.ns, file = here("Output/nasc_cluster_plot_ns.Rdata"))
}

# Map trawl species proportions -------------------------------------------------------
# Select and rename trawl data for pie charts
cluster.pie.ns <- cluster.pie %>% 
  filter(cluster %in% unique(nasc.nearshore.summ$cluster)) 

cluster.pos.ns <- filter(cluster.pie.ns, AllCPS > 0) %>% 
  arrange(desc(X))

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(cluster.pos.ns) > 0) {
  cluster.pos.ns <- cluster.pos.ns %>% 
    replace(. == 0, 0.0000001) 
}

# Filter for empty trawls
cluster.zero.ns <- filter(cluster.pie.ns, AllCPS == 0)

if (save.figs) {
  # Create trawl figure
  trawl.catch.plot.ns <- base.map +
    # Plot nasc data
    geom_point(data = nasc.nearshore, aes(X, Y, group = transect),
               size = 0.5, colour = "gray50", alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = cluster.pos.ns, 
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
    geom_point(data = cluster.zero.ns, aes(X, Y), 
               size = 2, shape = 21, fill = 'black', colour = 'white') +
    # Plot panel label
    ggtitle("CPS Species Proportions in Trawls") +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # Combine nasc.cluster.plot and trawl.proportion.plot for report
  nasc.trawl.cluster.wt.ns <- plot_grid(nasc.cluster.plot.ns, trawl.catch.plot.ns,
                                        nrow = 1, labels = c("a)", "b)"))
  
  ggsave(nasc.trawl.cluster.wt.ns,
         filename = here("Figs/fig_nasc_trawl_cluster_wt_ns.png"),
         width = map.width*2, height = map.height)
}

# Join NASC and cluster length frequency data frames by cluster ----------------
nasc.nearshore <- nasc.nearshore %>% 
  left_join(select(clf, -lat, -long, -X, -Y), by = c("cluster" = "cluster"))

# Save results
save(nasc.nearshore, file = here("Output/cps_nasc_prop_ns.Rdata"))

# Apportion offshore backscatter ------------------------------------------
# Create data frame for plotting acoustic proportions by species
nasc.prop.all.ns <- nasc.nearshore %>%
  mutate(`Engraulis mordax`      = cps.nasc*prop.anch,
         `Sardinops sagax`       = cps.nasc*prop.sar,
         `Trachurus symmetricus` = cps.nasc*prop.jack,
         `Scomber japonicus`     = cps.nasc*prop.mack,
         `Clupea pallasii`       = cps.nasc*prop.her) 

# Prepare nasc.prop.all for facet plotting
nasc.prop.spp.ns <- nasc.prop.all.ns %>% 
  select(X, Y, `Engraulis mordax`, `Sardinops sagax`, `Trachurus symmetricus`,
         `Scomber japonicus`, `Clupea pallasii`) %>% 
  gather(scientificName, nasc, -X, -Y)

if (save.figs) {
  # Create a base map with relative CPS backscatter
  map.prop.all.ns <- base.map + 
    # Plot backscatter for all CPS nasc
    geom_point(data = nasc.prop.all.ns, aes(X, Y, size = cps.nasc), 
               colour = "gray20") +
    # Plot proportion of backscatter from each species present
    geom_point(data = filter(nasc.prop.spp.ns, nasc > 0),
               aes(X, Y, size = nasc), colour = "red") +
    # Facet by species
    facet_wrap(~scientificName, nrow = 2) +
    theme(strip.background.x = element_blank(),
          strip.text.x       = element_text(face = "italic")) +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # Save map
  ggsave(here("Figs/fig_nasc_acoustic_proportions_ns.png"), map.prop.all.ns,
         width = map.width*3, height = map.height*2)
  
  # Save plot objects
  save(map.prop.all, file = here("Output/acoustic_proportion_maps_ns.Rdata"))
  
} else {
  # Load plot objects
  load(here("Output/acoustic_proportion_maps_ns.Rdata"))
}

# Calculate offshore acoustic biomass density -----------------------------
nasc.nearshore <- nasc.nearshore %>% 
  mutate( 
    anch.dens = cps.nasc*prop.anch / (4*pi*sigmawg.anch) / 1000,
    her.dens  = cps.nasc*prop.her  / (4*pi*sigmawg.her)  / 1000,
    jack.dens = cps.nasc*prop.jack / (4*pi*sigmawg.jack) / 1000,
    mack.dens = cps.nasc*prop.mack / (4*pi*sigmawg.mack) / 1000,
    sar.dens  = cps.nasc*prop.sar  / (4*pi*sigmawg.sar)  / 1000)

# Format for plotting
nasc.density.ns <- nasc.nearshore %>%
  select(lat, long, anch.dens, her.dens, jack.dens, mack.dens, 
         sar.dens, transect, transect.name, int, cluster) %>% 
  group_by(transect, transect.name, int) %>% 
  summarise(
    lat = lat[1],
    long = long[1],
    `Engraulis mordax`      = mean(anch.dens),
    `Clupea pallasii`       = mean(her.dens),
    `Trachurus symmetricus` = mean(jack.dens),
    `Scomber japonicus`     = mean(mack.dens),
    `Sardinops sagax`       = mean(sar.dens)) %>% 
  gather(scientificName, density, -transect, -transect.name, -int, -lat, -long) %>% 
  mutate(bin       = cut(density,dens.breaks, include.lowest = TRUE),
         bin.level = as.numeric(bin)) %>% 
  ungroup()

# Create acoustic transect labels for maps (tx.labels.ns)
tx.labels.tmp.ns <- nasc.nearshore %>% 
  group_by(transect.name) %>%
  summarise(
    vessel.name = vessel.name[1],
    transect    = transect[1],
    start.lat = lat[which.max(long)],
    start.long = max(long),
    end.lat = lat[which.min(long)],
    end.long = min(long),
    brg = 90 - swfscMisc::bearing(end.lat, end.long, start.lat, start.long)[1])

tx.start.labels.ns <- tx.labels.tmp.ns %>% 
  filter(start.lat >= 48.54116) %>% 
  select(vessel.name, transect, transect.name, lat = start.lat, long = start.long, brg)

tx.end.labels.ns <- tx.labels.tmp.ns %>% 
  filter(start.lat < 48.54116) %>% 
  select(vessel.name, transect, transect.name, lat = end.lat, long = end.long, brg) 

tx.labels.ns <- tx.start.labels.ns %>% 
  rbind(tx.end.labels.ns) %>% 
  project_df(to = crs.proj)

# # Map transect labels
# ggplot(tx.labels.ns, aes(long, lat, label = transect)) +
#   geom_text() +
#   facet_wrap(~vessel.name, scales = "free")

# Summarise biomass density by transect and species (nasc.density.summ.ns)
nasc.density.summ.ns <- nasc.density.ns %>% 
  group_by(scientificName, transect, transect.name) %>% 
  summarise(density = sum(density)) %>% 
  filter(scientificName %in% unique(lengths$scientificName)) %>% 
  left_join(select(tx.labels.tmp.ns, transect.name, start.lat, start.long)) %>% 
  mutate(positive = density > 0) %>% 
  ungroup()

# Save biomass density data
save(nasc.density.ns, nasc.density.summ.ns, 
     file = here("Output/nasc_biomass_density_ns.Rdata"))

# Select biomass density legend objects 
dens.levels.all.ns <- sort(unique(nasc.density.ns$bin.level))
dens.labels.all.ns <- dens.labels[dens.levels.all.ns]
dens.sizes.all.ns  <- dens.sizes[dens.levels.all.ns]
dens.colors.all.ns <- dens.colors[dens.levels.all.ns]

# Stratify transects ------------------------------------------------------
# Get transect spacing for plotting (tx.nn.ns) ----------------------------
# Get the midpoint (mean) lat/long of each transect
tx.mid.ns <- nasc.nearshore %>% 
  group_by(vessel.name, transect, transect.name) %>% 
  summarise(
    lat  = mean(lat, na.rm = TRUE),
    long = mean(long, na.rm = TRUE))

# Create a data frame for results
tx.nn.ns <- data.frame()

for (i in unique(tx.mid.ns$transect.name)) {
  # Get the mid lat/long for each transect
  tx.nn.i      <- filter(tx.mid.ns, transect.name == i)
  # Get NASC data for all other transects
  nasc.temp <- filter(tx.mid.ns, transect.name != i, vessel.name %in% tx.nn.i$vessel.name)
  # Calculate distance between transect midpoint and all other NASC values
  nn.dist   <- swfscMisc::distance(tx.nn.i$lat, tx.nn.i$long,
                                   nasc.temp$lat, nasc.temp$long)
  # Get the transect info and spacing based on shortest distance
  nn.tx     <- nasc.temp$transect.name[which.min(nn.dist)]
  min.dist  <- nn.dist[which.min(nn.dist)]
  nn.lat    <- nasc.temp$lat[which.min(nn.dist)]
  nn.long   <- nasc.temp$long[which.min(nn.dist)]
  # Add to results
  tx.nn.ns     <- bind_rows(tx.nn.ns, 
                            data.frame(tx.nn.i, nn.tx, min.dist, nn.lat, nn.long))
}

# Bin transects by spacing
tx.nn.ns <- tx.nn.ns %>% 
  mutate(dist.bin = cut(tx.nn.ns$min.dist, tx.spacing.bins),
         spacing  = tx.spacing.dist[as.numeric(dist.bin)],
         dist.cum = cumsum(spacing)) %>% 
  arrange(transect.name)

if (!is.na(tx.spacing.ns)) {
  tx.nn.ns <- tx.nn.ns %>% 
    mutate(spacing = tx.spacing.ns,
           dist.bin = cut(spacing, tx.spacing.bins),
           dist.cum = cumsum(spacing))
}

# Save nearest neighbor distance info
save(tx.nn.ns, file = here("Output/transect_spacing_ns.Rdata"))

# Summarise biomass density by transect and species
nasc.density.summ.ns <- nasc.density.summ.ns %>% 
  left_join(select(tx.nn.ns, vessel.name, transect.name, dist.cum, dist.bin))

# Draw pseudo-transects ---------------------------------------------------
# Get transect ends, calculate bearing, and add transect spacing
# Used to draw transects above and below planned transects, with appropriate spacing

# For each vessel (v) and region (r), get transect ends
# If the region is an island, use transect waypoints to draw strata

# Remove nearshore strata, if exists
if (exists("tx.ends.ns")) rm(tx.ends.ns) # inshore- and offshore-most intervals
if (exists("strata.ns")) rm(strata.ns) 
if (exists("strata.points.ns")) rm(strata.points.ns) 

# Assign transects to region based on latitude
# Because some duplicate transect numbers exist, can't do a straight join
nasc.region <- data.frame()

for (v in unique(nasc.nearshore$vessel.name)) {
  # Get latitude range for backscatter data
  nasc.nearshore.summ <- nasc.nearshore %>% 
    filter(vessel.name == v) %>% 
    summarise(lat.min = min(lat) - 0.1,
              lat.max = max(lat) + 0.1)
  
  # Extract only waypoints in the survey region
  region.wpts <- wpts %>% 
    filter(Type == "Nearshore") %>% 
    group_by(Transect, Type, Region) %>% 
    arrange(Type, Region, Transect) %>% 
    filter(between(lat, nasc.nearshore.summ$lat.min, nasc.nearshore.summ$lat.max)) %>% 
    # mutate(transect = sprintf("%03d", Transect)) %>% 
    mutate(transect = as.numeric(Transect),
           transect.name = paste(v, sprintf("%03d", transect))) %>% 
    ungroup()
  
  # ggplot(region.wpts, aes(long, lat, colour = Region)) + geom_point() + coord_map()
  
  # Get waypoint depth and add to region.wpts
  region.wpts$depth <- get.depth(noaa.bathy, 
                                 region.wpts$long, 
                                 region.wpts$lat, 
                                 locator = F, distance = F)$depth 
  
  # Add region to nasc by vessel
  nasc.region.temp <- nasc.nearshore %>% 
    filter(vessel.name == v) %>%  
    left_join(select(region.wpts, transect, region = Region)) %>% 
    mutate(key = paste(vessel.name, region))
  
  # For each region, draw strata polygon using NASC intervals or deepest waypoints
  for (k in unique(nasc.region.temp$key)) {
    # If an island region, create inner and outer polygons, combine, and compute area
    if (str_detect(k, "Island")) {
      # Get inshore nearshore waypoints
      tx.i.ns <- region.wpts %>%
        group_by(transect, transect.name) %>%
        slice(which.min(abs(depth))) %>% 
        mutate(grp = "original",
               loc = "inshore",
               key = paste(v, Region)) %>% 
        filter(key == k) %>%
        ungroup() 
      
      # Get offshore nearshore waypoints
      tx.o.ns <- region.wpts %>%
        group_by(transect, transect.name) %>%
        slice(which.max(abs(depth))) %>% 
        mutate(grp = "original",
               loc = "offshore",
               key = paste(v, Region)) %>% 
        filter(key == k) %>%
        ungroup() 
      
      # Create data frame with transect ends
      tx.ends.ns.k <- tx.i.ns %>% 
        mutate(vessel.name = v) %>% 
        select(transect.name, transect, vessel.name, 
               lat.i = lat, long.i = long) %>% 
        bind_cols(select(tx.o.ns, lat.o = lat, long.o = long))
      
      # Combine transect waypoints for strata k
      strata.points.ns.k <- tx.i.ns %>% 
        bind_rows(tx.o.ns)   %>%
        mutate(vessel.name = v,
               region = k,
               key = paste(transect.name, grp)) 
      
      # Draw polygons around shallowest transect waypoint for island strata
      ns.poly.i <- tx.i.ns %>% 
        st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
        summarise(do_union = F) %>% 
        st_cast("LINESTRING") %>% 
        st_cast("POLYGON") 
      
      # Draw polygons around deepest transect waypoint for island strata
      ns.poly.o <- tx.o.ns %>%
        st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
        summarise(do_union = F) %>% 
        st_cast("LINESTRING") %>% 
        st_cast("POLYGON") 
      
      # Combine polygons
      ns.poly.k <- ns.poly.o %>% 
        st_difference(ns.poly.i) %>% 
        st_make_valid() %>%
        ungroup() %>% 
        mutate(vessel.name = v,
               area = as.numeric(st_area(.)),
               region = k)
      
    } else {
      # Else, use the same method as typical mainland transects that relies on NASC extents
      
      # Draw pseudo-transects --------------------------------------------------------
      # Get transect ends, calculate bearing, and add transect spacing
      tx.ends.ns.k <- nasc.region.temp %>%
        filter(key == k) %>%
        group_by(transect.name, transect, vessel.name) %>%
        summarise(
          lat.i  = lat[which.max(long)],
          long.i = max(long),
          lat.o  = lat[which.min(long)],
          long.o = min(long)
        ) %>%
        left_join(select(tx.nn.ns, transect.name, spacing)) %>%
        mutate(
          brg = swfscMisc::bearing(lat.i, long.i,
                                   lat.o, long.o)[1])
      
      # Get original inshore transect ends -------------------------------------------
      # Select original inshore waypoints
      tx.i.ns <- tx.ends.ns.k %>% 
        select(-lat.o, -long.o) %>% 
        rename(lat = lat.i, long = long.i) %>% 
        mutate(
          grp = "original",
          loc = "inshore",
          order = 2)
      
      # Get N and S inshore waypoints ------------------------------------------------
      # Calculate inshore/north transects
      tx.i.n.ns <- tx.ends.ns.k %>% 
        select(-lat.o, -long.o) %>% 
        rename(lat = lat.i, long = long.i) %>% 
        mutate(
          lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
          long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
          grp = "north",
          loc = "inshore",
          order = 1)
      
      # Calculate inshore/south transects
      tx.i.s.ns <- tx.ends.ns.k %>% 
        select(-lat.o, -long.o) %>% 
        rename(lat = lat.i, long = long.i) %>% 
        mutate(
          lat  = destination(lat, long, brg - 90, spacing/2, units = "nm")["lat"],
          long = destination(lat, long, brg - 90, spacing/2, units = "nm")["lon"],
          grp = "south",
          loc = "inshore",
          order = 3)
      
      # Combine all inshore transects
      if (v == "LBC" & survey.name == "1907RL") {
        # Transect numbers were reversed in 2019
        tx.i.ns <- tx.i.ns %>%
          bind_rows(tx.i.n.ns) %>%
          bind_rows(tx.i.s.ns) %>%
          arrange(desc(transect), desc(order))
      } else {
        tx.i.ns <- tx.i.ns %>%
          bind_rows(tx.i.n.ns) %>%
          bind_rows(tx.i.s.ns) %>%
          arrange(transect, desc(order))  
      }
      
      # Get original offshore transect ends ------------------------------------------
      tx.o.ns <- tx.ends.ns.k %>% 
        select(-lat.i, -long.i) %>% 
        rename(lat = lat.o, long = long.o) %>% 
        mutate(
          grp = "original",
          loc = "offshore",
          order = 2)
      
      # Get N and S offshore waypoints -----------------------------------------------
      # Calculate offshore/north transects
      tx.o.n.ns <- tx.ends.ns.k %>% 
        select(-lat.i, -long.i) %>% 
        rename(lat = lat.o, long = long.o) %>% 
        mutate(
          lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
          long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
          grp = "north",
          loc = "offshore",
          order = 3)
      
      # Calculate offshore/south transects
      tx.o.s.ns <- tx.ends.ns.k %>% 
        select(-lat.i, -long.i) %>% 
        rename(lat = lat.o, long = long.o) %>% 
        mutate(
          lat  = destination(lat, long, brg - 90, spacing/2, units = "nm")["lat"],
          long = destination(lat, long, brg - 90, spacing/2, units = "nm")["lon"],
          grp = "south",
          loc = "offshore",
          order = 1)
      
      tx.o.final.ns <- data.frame()
      
      for (v in unique(tx.o.ns$vessel.name)) {
        if (v == "SD1024") {
          tx.o.tmp <- filter(tx.o.ns, vessel.name == v)
        } else {
          tx.o.tmp <- filter(tx.o.ns, vessel.name == v) %>% 
            bind_rows(filter(tx.o.n.ns, vessel.name == v)) %>% 
            bind_rows(filter(tx.o.s.ns, vessel.name == v))
        }
        tx.o.final.ns <- bind_rows(tx.o.final.ns, tx.o.tmp)
      }
      
      # Combine all inshore transects
      if (v == "LBC" & survey.name == "1907RL") {
        # Transect numbers were reversed in 2019
        tx.o.ns <- tx.o.final.ns %>% 
          arrange(transect, desc(order))
      } else {
        tx.o.ns <- tx.o.final.ns %>% 
          arrange(desc(transect), desc(order))
      }
      
      # Assemble the final data frame with all waypoints -----------------------------
      strata.points.ns.k <- tx.i.ns %>% 
        bind_rows(tx.o.ns)   %>%
        mutate(key = paste(transect.name, grp),
               region = k) 
      
      # ggplot(strata.points.ns.k, aes(long, lat)) + geom_polygon() + coord_map()
      
      # Create polygons
      ns.poly.k <- strata.points.ns.k %>% 
        st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
        group_by(vessel.name, region) %>% 
        summarise(do_union = F) %>% 
        st_cast("POLYGON") %>% 
        st_make_valid() %>% 
        st_difference(st_union(bathy_5m_poly)) %>% 
        mutate(area = st_area(.)) %>% 
        ungroup()
      
      # Convert to lines
      ns.lines.sf <- strata.points.ns.k %>% 
        st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
        group_by(transect, grp) %>% 
        summarise(do_union = F) %>% 
        st_cast("LINESTRING") %>% 
        ungroup()
      
    }
    
    # Combine with other strata points ---------------------------------
    if (exists("tx.ends.ns")) {
      # Combine waypoints with all strata points
      tx.ends.ns <- bind_rows(tx.ends.ns, tx.ends.ns.k)
    } else {
      tx.ends.ns <- tx.ends.ns.k
    }
    
    # Combine with other strata points ---------------------------------
    if (exists("strata.points.ns")) {
      # Combine waypoints with all strata points
      strata.points.ns <- bind_rows(strata.points.ns, strata.points.ns.k)
    } else {
      strata.points.ns <- strata.points.ns.k
    }
    
    # Combine with other polygons ----------------------------------------
    if (exists("strata.ns")) {
      strata.ns <- rbind(strata.ns, ns.poly.k)
    } else {
      strata.ns <- ns.poly.k
    }
  }
  
  # Combine nasc used to create strata polygons
  nasc.region <- bind_rows(nasc.region, nasc.region.temp)
}

# Summarize transects by region, to be used later for stratification
nasc.summ.region <- nasc.region %>% 
  group_by(vessel.name, transect.name, region) %>% 
  tally() %>% 
  ungroup()

# Offshore strata
strata.manual.ns <- bind_rows(
  data.frame(
    scientificName = "Clupea pallasii",
    stratum = 1,
    transect = 1:n_distinct(nasc.nearshore$transect)),
  data.frame(
    scientificName = "Engraulis mordax",
    stratum = 1,
    transect = 1:n_distinct(nasc.nearshore$transect)),
  data.frame(
    scientificName = "Sardinops sagax",
    stratum = 1,
    transect = 1:n_distinct(nasc.nearshore$transect)),
  data.frame(
    scientificName = "Scomber japonicus",
    stratum = 1,
    transect = 1:n_distinct(nasc.nearshore$transect)),
  data.frame(
    scientificName = "Trachurus symmetricus",
    stratum = 1,
    transect = 1:n_distinct(nasc.nearshore$transect))
)

# Create stratum polygons -------------------------------------------------
# Define sampling strata
if (stratify.manually.ns) {
  # Use manually defined strata
  strata.final.ns <- strata.manual.ns %>%
    mutate(stratum.orig = stratum)
  
} else {
  # Define strata automatically
  strata.final.ns <- data.frame()
  
  # Define strata boundaries and transects for each species
  for (i in unique(nasc.density.summ.ns$scientificName)) {
    for (j in unique(nasc.density.summ.ns$vessel.name)) {
      # Select positive transects and calculate differences between transect numbers
      # diffs >= 2 define stratum breaks
      temp.spp <- nasc.density.summ.ns %>% 
        filter(scientificName == i, vessel.name == j, positive == TRUE) %>% 
        ungroup() %>% 
        mutate(diff = c(1, diff(transect)))
      
      if (nrow(temp.spp) > 0) {
        # Find the start of each positive stratum
        spp.starts <- temp.spp %>%
          # mutate(diff = c(1, diff(transect))) %>%
          filter(diff > max.diff)
        
        # If the start of the stratum == 1, stratum start is 1, else min transect number
        survey.start <- ifelse(min(temp.spp$transect) == 1, 1, min(temp.spp$transect) - 1)
        
        # A vector of stratum starts
        stratum.start <- c(survey.start, spp.starts$transect - 1)
        
        # If the end of the stratum is the last transect in the survey,
        # select the last, else the last transect + 1
        survey.end <- ifelse(max(temp.spp$transect) == max(nasc.density.summ$transect),
                             max(nasc.density.summ$transect),
                             max(temp.spp$transect) + 1)
        
        # A vector of stratum ends
        stratum.end <- c(temp.spp$transect[which(temp.spp$diff > max.diff) - 1] + 1,
                         survey.end)
        
        # Combine starts and ends in to a data frame for plotting and generating stratum vectors
        strata.spp <- data.frame(scientificName = i,
                                 vessel.name = j,
                                 stratum = seq(1, length(stratum.start)),
                                 start = stratum.start,
                                 end = stratum.end) # %>% mutate(n.tx = end - start + 1)
        
        # Create stratum vectors
        strata.df <- data.frame()
        
        for (kk in 1:nrow(strata.spp)) {
          # Create a vector of transects from start to end
          transect <- seq(strata.spp$start[kk], strata.spp$end[kk])
          
          # Combine results
          strata.df <- bind_rows(strata.df,
                                 data.frame(scientificName = i,
                                            stratum = kk,
                                            transect))
        }
        
        # Add vessel name and distance bin to strata.df for final cuts
        strata.df <- strata.df %>%
          left_join(filter(select(nasc.density.summ.ns, scientificName, vessel.name, transect, dist.bin),
                           scientificName == i, vessel.name == j)) %>%
          filter(!is.na(vessel.name)) %>%
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
        strata.final.ns <- bind_rows(strata.final.ns, strata.df)
      }
    }
  }
}

# Add start latitude and longitude to strata table
strata.final.ns <- strata.final.ns %>%
  mutate(transect.name = paste(vessel.name, sprintf("%03d", transect))) %>% 
  left_join(tx.labels.ns) %>%
  filter(!is.na(vessel.name)) %>% 
  ungroup()

# Plot classification
# ggplot(strata.final.ns, aes(long, lat, colour = paste(vessel.name, stratum))) +
#   geom_point() +
#   facet_wrap(~scientificName) +
#   coord_map() +
#   theme_bw()

# Ungroup tx.ends so it joins properly with strata.points.os  
tx.ends.ns <- ungroup(tx.ends.ns)

if (stock.break.source == "primary") {
  strata.points.ns <- strata.points.ns %>% 
    ungroup() %>% 
    left_join(ungroup(select(tx.ends.ns, transect.name, lat.stock = lat.i)))
} else {
  strata.points.ns <- strata.points.ns %>% 
    mutate(lat.stock = lat)
}

# Summarise NASC to get species present
nearshore.spp <- nasc.prop.spp.ns %>% 
  filter(nasc > 0) %>% 
  group_by(scientificName) %>% 
  tally()

# Create final strata and calculate area
# Create df for transect-level stock info
nasc.stock.ns <- data.frame()

if (exists("strata.nearshore")) rm(strata.nearshore)

for (i in unique(nearshore.spp$scientificName)) {
  for (j in unique(strata.final.ns$vessel.name)) {
    # Select each strata per species
    strata.sub <- filter(strata.final.ns, scientificName == i, vessel.name == j) %>% 
      select(transect.name, stratum) %>% 
      ungroup()
    
    # Define strata to stock
    nasc.stock.temp <- strata.points.ns %>% 
      filter(vessel.name == j, loc == "inshore") %>%
      left_join(strata.sub) %>% 
      mutate(stock = case_when(
        i == "Engraulis mordax" & lat.stock >= stock.break.anch ~ "Northern",
        i == "Engraulis mordax" & lat.stock <  stock.break.anch ~ "Central",
        i == "Sardinops sagax"  & lat.stock >= stock.break.sar  ~ "Northern",
        i == "Sardinops sagax"  & lat.stock <  stock.break.sar  ~ "Southern",
        i %in% c("Clupea pallasii","Scomber japonicus","Trachurus symmetricus") ~ "All"),
        scientificName = i) %>% 
      filter(!is.na(stratum)) %>% 
      select(transect.name, stock, scientificName) %>%
      distinct() %>%
      arrange(transect.name) %>% 
      ungroup()
    
    # Combine results
    nasc.stock.ns <- bind_rows(nasc.stock.ns, nasc.stock.temp)  
    
    for (k in sort(unique(strata.sub$stratum))) {
      # Create offshore stratum polygons ----------------------------------------
      # Add stratum numbers and stock designation to strata.points
      primary.poly.temp <- strata.points.ns %>% 
        left_join(strata.sub) %>%
        left_join(select(nasc.stock.temp, transect.name, stock)) %>% 
        left_join(select(nasc.summ.region, transect.name, region)) %>% 
        filter(vessel.name == j, stratum == k) %>%
        mutate(scientificName = i,
               region = case_when(
                 j %in% merge.regions ~ "All",
                 TRUE ~ region)) %>% 
        ungroup()
      
      poly.region <- str_replace(unique(primary.poly.temp$region), paste0(j, " "), "")
      
      # ggplot(primary.poly.temp, aes(long, lat, group = region, colour = region)) + geom_polygon() + coord_map()
      
      if (str_detect(poly.region, "Island")) {
        # If an Island strata
        # Get latitude range for backscatter data
        nasc.nearshore.summ <- nasc.nearshore %>% 
          filter(vessel.name == j) %>% 
          summarise(lat.min = min(lat) - 0.1,
                    lat.max = max(lat) + 0.1)
        
        # Extract only waypoints in the survey region
        region.wpts <- wpts %>% 
          filter(Type == "Nearshore") %>% 
          group_by(Transect, Type, Region) %>% 
          arrange(Type, Region, Transect) %>% 
          filter(between(lat, nasc.nearshore.summ$lat.min, nasc.nearshore.summ$lat.max),
                 str_detect(Region, poly.region)) %>% 
          mutate(transect = as.numeric(Transect),
                 transect.name = paste(j, sprintf("%03d", transect))) %>% 
          ungroup()
        
        # Get waypoint depth and add to region.wpts
        region.wpts$depth <- get.depth(noaa.bathy, 
                                       region.wpts$long, 
                                       region.wpts$lat, 
                                       locator = F, distance = F)$depth 
        
        # Get inshore nearshore waypoints
        tx.i.ns <- region.wpts %>%
          group_by(transect, transect.name) %>%
          slice(which.min(abs(depth))) %>% 
          mutate(grp = "original",
                 loc = "inshore",
                 key = paste(j, Region)) %>% 
          left_join(select(nasc.stock.temp, transect.name, stock)) %>% 
          ungroup() 
        
        # Get offshore nearshore waypoints
        tx.o.ns <- region.wpts %>%
          group_by(transect, transect.name) %>%
          slice(which.max(abs(depth))) %>% 
          mutate(grp = "original",
                 loc = "offshore",
                 key = paste(j, Region)) %>% 
          # filter(key == k) %>%
          ungroup() 
        
        # Create data frame with transect ends
        tx.ends.ns.j <- tx.i.ns %>% 
          mutate(vessel.name = j) %>% 
          select(transect.name, transect, vessel.name, 
                 lat.i = lat, long.i = long) %>% 
          bind_cols(select(tx.o.ns, lat.o = lat, long.o = long))
        
        # Combine transect waypoints for strata k
        strata.points.ns.k <- tx.i.ns %>% 
          bind_rows(tx.o.ns)   %>%
          mutate(vessel.name = j,
                 region = poly.region,
                 key = paste(transect.name, grp)) 
        
        # Draw polygons around shallowest transect waypoint for island strata
        primary.poly.i <- tx.i.ns %>% 
          st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
          summarise(do_union = F) %>% 
          st_cast("LINESTRING") %>% 
          st_cast("POLYGON") %>% 
          ungroup()
        
        # Draw polygons around deepest transect waypoint for island strata
        primary.poly.o <- tx.o.ns %>%
          st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
          summarise(do_union = F) %>% 
          st_cast("LINESTRING") %>% 
          st_cast("POLYGON") %>% 
          ungroup()
        
        # Combine polygons
        primary.poly.k <- primary.poly.o %>% 
          st_difference(primary.poly.i) %>% 
          st_make_valid() %>%
          mutate(stratum = k,
                 stock = unique(primary.poly.temp$stock),
                 scientificName = i,
                 vessel.name = j,
                 area = as.numeric(st_area(.))) %>% 
          ungroup() 
        
      } else {
        # If a mainland strata
        # Select the southern-most inshore point for j-th stratum
        primary.poly.k.s <- primary.poly.temp %>%
          filter(loc == "inshore") %>% 
          # arrange(transect, order) %>%
          slice(1)
        
        # Select the northern-most inshore point for j-th stratum
        primary.poly.k.n <- primary.poly.temp %>%
          filter(loc == "inshore") %>% 
          # arrange(transect, order) %>%
          slice(n())
        
        # Select only the original inshore waypoints for j-th stratum
        primary.poly.k.i <- primary.poly.temp %>% 
          filter(loc == "inshore", grp == "original") %>% 
          arrange(transect) %>%
          mutate(scientificName = i)
        
        # Combine all inshore transects
        if (j == "LBC" & survey.name == "1907RL") {
          # Select the southern-most inshore point for j-th stratum
          primary.poly.k.s <- primary.poly.temp %>%
            filter(loc == "inshore") %>% 
            # arrange(transect, order) %>%
            slice(n())
          
          # Select the northern-most inshore point for j-th stratum
          primary.poly.k.n <- primary.poly.temp %>%
            filter(loc == "inshore") %>% 
            # arrange(transect, order) %>%
            slice(1)
          
          # Select only the original inshore waypoints for j-th stratum
          primary.poly.k.i <- primary.poly.temp %>% 
            filter(loc == "inshore", grp == "original") %>% 
            arrange(transect) %>%
            mutate(scientificName = i)
          
          # Create the final polygon
          primary.poly.k <- primary.poly.temp %>% 
            filter(loc == "offshore") %>% 
            arrange(desc(transect), order) %>% 
            bind_rows(primary.poly.k.s) %>%
            bind_rows(primary.poly.k.i) %>%
            bind_rows(primary.poly.k.n) %>% 
            st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
            group_by(stratum, stock) %>% 
            summarise(do_union = F) %>% 
            st_cast("POLYGON") %>% 
            # st_difference() %>% 
            mutate(scientificName = i, 
                   vessel.name = j,
                   area = as.numeric(st_area(.))) %>% 
            ungroup()
         
        } else {
          # Create the final polygon
          primary.poly.k <- primary.poly.temp %>% 
            filter(loc == "offshore") %>% 
            arrange(desc(transect), desc(order)) %>% 
            bind_rows(primary.poly.k.s) %>%
            bind_rows(primary.poly.k.i) %>%
            bind_rows(primary.poly.k.n) %>% 
            st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
            group_by(stratum, stock) %>% 
            summarise(do_union = F) %>% 
            st_cast("POLYGON") %>% 
            # st_difference() %>% 
            mutate(scientificName = i, 
                   vessel.name = j,
                   area = as.numeric(st_area(.))) %>% 
            ungroup()
        }
      }

      # Combine with other polygons ----------------------------------------
      if (exists("strata.nearshore")) {
        strata.nearshore <- rbind(strata.nearshore, primary.poly.k)
      } else {
        strata.nearshore <- primary.poly.k
      }
    }
  }
}

# ggplot(strata.nearshore, aes(fill = stock)) +
#   geom_sf() +
#   facet_wrap(~scientificName) +
#   theme_bw()
# 
# mapview(filter(strata.nearshore, scientificName == "Sardinops sagax"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Clupea pallasii"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Engraulis mordax"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Scomber japonicus"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Trachurus symmetricus"), zcol = "vessel.name")


# Save strata polygons
save(strata.nearshore, 
     file = here("Output/strata_nearshore_raw.Rdata"))



