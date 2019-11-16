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
  save(nasc.nearshore, file = here("Output/CTDapp/CTDapp_All_Nearshore.Rdata"))
  
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
  
  # Process purse seine data
  if (process.seine) {
    source(here("Code", paste0("processSeine_", survey.name, ".R")))  
    
    if (use.seine.data) {
      # Combine clf and clf.seine
      clf <- clf %>%
        mutate(sample.type = "Trawl") %>% 
        bind_rows(clf.seine)
      
      lf.final <- lf.final %>% 
        bind_rows(lf.final.seine)

      # Combine super.clusters and super.clusters.ns
      super.clusters <- bind_rows(super.clusters, super.clusters.ns)

    }
  }
  
  # Save after processing nearshore
  save(clf, super.clusters, lf.final, file = here("Output/clf_nearshore.Rdata"))
  
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
  load(here("Output/clf_nearshore.Rdata"))
}

# Filter unwanted transects
nasc.nearshore <- nasc.nearshore %>% 
  filter(!transect.name %in% unlist(tx.rm[nasc.vessels.nearshore]))

# Convert nav to spatial
# nav.sf <- st_as_sf(nav, coords = c("long","lat"), crs = crs.geog) 
nav.ns.sf <- st_as_sf(nasc.nearshore, coords = c("long","lat"), crs = crs.geog) 

nav.paths.ns.sf <- nav.ns.sf %>% 
  group_by(vessel.name, transect) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  ungroup()

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
# ggplot(nasc.plot.ns, aes(long, lat, colour = vessel.name, size = NASC)) + geom_point() + facet_wrap(~vessel.name) + coord_map()

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
    geom_point(data = nasc.nearshore, aes(X, Y), size = 0.5, show.legend = FALSE) +
    # Plot convex hull around NASC clusters
    geom_sf(data = nasc.super.clusters.ns, colour = 'black', alpha = 0.5, show.legend = FALSE) +
    # geom_point(data = nasc.nearshore, aes(X, Y, colour = factor(cluster)),
    #            size = 0.5, show.legend = FALSE) +
    # # Plot convex hull around NASC clusters
    # geom_sf(data = nasc.super.clusters.ns, aes(fill = factor(cluster)),
    #         colour = 'black', alpha = 0.5, show.legend = FALSE) +
    scale_fill_discrete(name = "Cluster") +
    scale_colour_manual(name = "Cluster", values = c("Trawl" = "blue", "Purse seine" = "red")) +
    # Plot cluster midpoints
    geom_shadowtext(data = filter(clf, CPS.num == 0), 
                    aes(X, Y, label = cluster),
                    colour = 'gray20', bg.colour = "white", size = 2) +
    # Plot positive trawl cluster midpoints
    geom_text_repel(data = filter(clf, CPS.num > 0, cluster %in% nasc.nearshore$cluster),
                    aes(X, Y, label = cluster, colour = sample.type),
                    size = 2, fontface = 'bold') +
    # geom_shadowtext(data = filter(clf, CPS.num > 0, cluster %in% nasc.nearshore$cluster), 
    #                 aes(X, Y, label = cluster, colour = sample.type), 
    #                 size = 2, bg.colour = "white", fontface = "bold") +
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
if (use.seine.data) {
  cluster.pie <- bind_rows(cluster.pie, set.pie)
}

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
    ggtitle("CPS Species Proportions in Net Samples") +
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

# Summarise transects -----------------------------------------------------
# Summarise nasc data
nasc.summ.ns <- nasc.nearshore %>% 
  group_by(vessel.name, transect.name, transect) %>% 
  summarise(
    start     = min(datetime),
    end       = max(datetime),
    duration  = difftime(end, start, units = "hours"),
    n_int     = length(Interval),
    distance  = length(Interval)*nasc.interval/1852,
    lat       = lat[which.min(long)],
    long      = long[which.min(long)],
    mean_nasc = mean(cps.nasc)) %>% 
  arrange(vessel.name, start) %>% 
  ungroup()

save(nasc.summ.ns, file = here("Output/nasc_summ_tx_ns.Rdata"))

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
  group_by(transect, transect.name, int, cluster) %>% 
  summarise(
    lat = lat[1],
    long = long[1],
    `Engraulis mordax`      = mean(anch.dens),
    `Clupea pallasii`       = mean(her.dens),
    `Trachurus symmetricus` = mean(jack.dens),
    `Scomber japonicus`     = mean(mack.dens),
    `Sardinops sagax`       = mean(sar.dens)) %>% 
  gather(scientificName, density, -transect, -transect.name, -int, -lat, -long, -cluster) %>% 
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

# Map transect labels
# ggplot(tx.labels.ns, aes(long, lat, label = transect)) + geom_text() + facet_wrap(~vessel.name, scales = "free")

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
  # mutate(key = paste(scientificName, stock, vessel.name, stratum)) %>% 
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
      select(vessel.name, transect.name, stock, scientificName, stratum) %>%
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
# mapview(filter(strata.nearshore, scientificName == "Clupea pallasii"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Engraulis mordax"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Sardinops sagax"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Scomber japonicus"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Trachurus symmetricus"), zcol = "vessel.name")

# Save strata polygons
save(strata.nearshore, 
     file = here("Output/strata_nearshore_raw.Rdata"))

if (exists("strata.nearshore.fac")) rm("strata.nearshore.fac")

for (ii in unique(strata.nearshore$scientificName)) {
  strata.ns.fac.tmp <- strata.nearshore %>% 
    filter(scientificName == ii) %>% 
    arrange(stock, vessel.name, stratum) %>% 
    mutate(strata.fac = as.numeric(as.factor(paste(stock, vessel.name, stratum))), 
           key = paste(scientificName, stock, vessel.name, stratum)) %>% 
    select(strata.fac, key) %>% 
    st_set_geometry(NULL)
  
  if (exists("strata.nearshore.fac")) {
    # Combine results
    strata.nearshore.fac <- bind_rows(strata.nearshore.fac, strata.ns.fac.tmp) 
    
  } else {
    # Create new data frame
    strata.nearshore.fac <- strata.ns.fac.tmp
  }
}

# Add new strata factor to nearshore stratum polygons
strata.nearshore <- strata.nearshore %>% 
  ungroup() %>% 
  mutate(key = paste(scientificName, stock, vessel.name, stratum)) %>% 
  select(scientificName, stock, vessel.name, everything(), -stratum) %>% 
  left_join(strata.nearshore.fac, by = "key") %>% 
  rename(stratum = strata.fac)

# Add new strata factor to nearshore stratum polygons
# Create join key in nasc.stock.ns, for use in stratification later
nasc.stock.ns <- nasc.stock.ns %>% 
  mutate(key = paste(scientificName, stock, vessel.name, stratum)) %>% 
  left_join(strata.nearshore.fac)

# ggplot(strata.nearshore, aes(fill = factor(stratum))) +
#   geom_sf() +
#   facet_wrap(~scientificName) +
#   theme_bw()
# 
# mapview(filter(strata.nearshore, scientificName == "Clupea pallasii"), zcol = "stratum")
# mapview(filter(strata.nearshore, scientificName == "Engraulis mordax"), zcol = "stratum")
# mapview(filter(strata.nearshore, scientificName == "Sardinops sagax"), zcol = "stratum")
# mapview(filter(strata.nearshore, scientificName == "Scomber japonicus"), zcol = "stratum")
# mapview(filter(strata.nearshore, scientificName == "Trachurus symmetricus"), zcol = "stratum")

# Clip primary polygons using the 5 m isobathy polygon -------------------------
# Summarise strata transects
strata.summ.ns <- strata.final.ns %>% 
  left_join(nasc.stock.ns) %>% 
  group_by(scientificName, stock, vessel.name, stratum) %>% 
  summarise(
    start     = min(transect),
    end       = max(transect),
    first     = transect.name[which.min(transect)],
    last      = transect.name[which.max(transect)]) %>% 
  arrange(scientificName, stock, vessel.name, stratum) %>% 
  ungroup()

# Remove overlapping intervals --------------------------------------------
# Create a mask for LBC data, used to punch holes in Lasker's super polygons
island.polygons <- strata.ns %>% 
  filter(str_detect(region, "Island")) %>%
  st_union() 

# Remove primary super polygon intersecting island polygons
strata.super.polygons.ns <- strata.super.polygons %>% 
  st_difference(island.polygons) %>% 
  ungroup()

# Remove overlap with 5 m isobath and primary survey strata
# strata.nearshore <- select(strata.nearshore, -vessel.name.1, -area.1)
strata.nearshore <- strata.nearshore %>% 
  st_make_valid() %>% 
  st_difference(st_union(bathy_5m_poly)) %>% 
  st_difference(select(strata.super.polygons.ns, geometry)) %>%
  ungroup() %>% 
  mutate(area = st_area(.)) 

# Convert biomass density to sf ------------------------------
nasc.density.ns.sf <- nasc.density.ns %>% 
  st_as_sf(coords = c("long", "lat"), crs = crs.geog) %>% 
  mutate(
    label = paste0('Species: ', scientificName, "; ", 
                   'Transect: ', transect, "; ",
                   'Density: ', signif(density, 2), ' t/sq.nmi'),
    popup = paste0('<b>Species: </b>', scientificName,  '<br/>',
                   '<b>Transect: </b>', transect, '<br/>',
                   '<b>Density: </b>', signif(density, 2), ' t nmi<sup>-2</sup>')
  )

# mapview(filter(strata.nearshore, scientificName == "Engraulis mordax")) +
#   mapview(filter(strata.nearshore2, scientificName == "Engraulis mordax"),zcol = "stock") +
#   mapview(filter(nasc.density.ns.sf, scientificName == "Engraulis mordax"), cex = "bin.level", zcol = "bin.level")

# mapview(filter(strata.nearshore, scientificName == "Sardinops sagax"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Clupea pallasii"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Engraulis mordax"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Scomber japonicus"), zcol = "vessel.name")
# mapview(filter(strata.nearshore, scientificName == "Trachurus symmetricus"), zcol = "vessel.name")

# Save clipped primary polygons
save(strata.nearshore, 
     file = here("Output/strata_nearshore_final.Rdata"))

# Convert polygons to points and add coordinates -------------------------------
strata.nearshore.points  <- strata.nearshore %>% 
  st_cast("MULTIPOINT") %>%
  st_cast("POINT") %>%
  mutate(
    long = as.data.frame(st_coordinates(.))$X,
    lat = as.data.frame(st_coordinates(.))$Y,
    grp = paste(scientificName, stock, stratum)) %>% 
  st_set_geometry(NULL)

# Save final strata points
save(strata.nearshore.points,  
     file = here("Output/strata_points_nearshore.Rdata"))

# Write nearshore stata points to CSV
write.csv(strata.nearshore.points,  
          file = here("Output/strata_points_nearshore.csv"),
          quote = F, row.names = F)

# Summarize nasc.strata by stock
strata.summ.nearshore <- strata.nearshore %>% 
  # select(scientificName, stratum, stock, area) %>%
  mutate(area = as.numeric(area)) %>% 
  st_set_geometry(NULL)

if (save.figs) {
  # Add strata polygons to acoustic proportions map
  map.stratum.all.ns <- base.map + 
    # Plot proportion of backscatter from each species present
    geom_point(data = nasc.prop.all.ns, aes(X, Y, size = cps.nasc), 
               colour = "gray20", show.legend = F) +
    geom_point(data = filter(nasc.prop.spp.ns, nasc > 0),
               aes(X, Y, size = nasc), colour = "red", show.legend = F) +
    # Plot final strata
    geom_sf(data = strata.nearshore, aes(colour = factor(stratum), 
                                         fill = stock), alpha = 0.5) +
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
  ggsave(here("Figs/fig_nasc_acoustic_proportions_strata_ns.png"), map.stratum.all.ns,
         width = map.width*2.6, height = map.height*1.5)
}

# Project nasc.density
nasc.density.ns <- nasc.density.ns %>% 
  ungroup() %>% 
  project_df(to = crs.proj)

# Summarize positive clusters to filter clusters from removed strata
pos.cluster.summ <- pos.clusters %>% 
  group_by(scientificName, cluster) %>% 
  summarise(nIndiv = sum(num)) %>% 
  ungroup() %>% 
  filter(nIndiv >= nIndiv.min)

pos.clusters.ns <- pos.clusters

nasc.ns.clusters <- sort(unique(nasc.nearshore$cluster))

# Remove overlapping intervals --------------------------------------------
if (process.nearshore) {
  # Subset nearshore backscatter and remove overlap with Lasker
  nasc.ns.sub <- nasc.nearshore %>%
    st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
    st_difference(strata.super.polygons.ns)
  
  # Subset nearshore biomass density and remove overlap with Lasker
  nasc.density.ns.sub <- nasc.density.ns %>% 
    mutate(id = seq_along(transect)) %>% 
    st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
    st_difference(strata.super.polygons.ns)  
  
  save(nasc.ns.sub, nasc.density.ns.sub,
       file = here("Output/nasc_nearshore_unique.Rdata"))
} else {
  load(here("Output/nasc_nearshore_unique.Rdata"))
}

# Filter nearshore backscatter using nasc.ns.sub
nasc.nearshore <- nasc.nearshore %>% 
  filter(id %in% nasc.ns.sub$id)

# Filter nearshore biomass density using nasc.density.ns.sub
nasc.density.ns <- nasc.density.ns %>% 
  mutate(id = seq_along(transect)) %>% 
  filter(id %in% nasc.density.ns.sub$id)

# Plot biomass density for each species and stock -------------------------
if (save.figs) {
  # Create a list for saving biomass density plots
  biomass.dens.figs.ns <- list()
  
  # Plot biomass density
  for (i in unique(strata.nearshore$scientificName)) {
    for (j in unique(filter(strata.nearshore, scientificName == i)$stock)) {
      # Filter biomass density
      nasc.density.plot.ns <- nasc.density.ns %>%
        left_join(filter(nasc.stock.ns, scientificName == i, stock == j)) %>% 
        filter(density != 0, scientificName == i, 
               stock == j, transect %in% strata.final.ns$transect[strata.final.ns$scientificName == i])
      
      # Filter positive clusters
      pos.cluster.txt <- filter(clf, cluster %in% nasc.density.plot.ns$cluster) 
      
      # pos.cluster.txt <- pos.clusters.ns %>% 
      #   filter(scientificName == i, stock == j,
      #          cluster %in% nasc.ns.clusters) %>% 
      #   ungroup() %>% 
      #   project_df(to = crs.proj)
      
      
      # Select biomass density legend objects 
      dens.levels.all.ns <- sort(unique(nasc.density.plot.ns$bin.level))
      dens.labels.all.ns <- dens.labels[dens.levels.all.ns]
      dens.sizes.all.ns  <- dens.sizes[dens.levels.all.ns]
      dens.colors.all.ns <- dens.colors[dens.levels.all.ns]
      
      # Map biomass density, strata polygons, and positive trawl clusters
      biomass.dens.ns <- base.map +
        geom_sf(data = filter(strata.nearshore, scientificName == i, stock == j),
                aes(colour = factor(stratum)), fill = NA, size = 1) +
        scale_colour_discrete('Stratum') + 
        # Plot vessel track
        # geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) +
        # Plot zero nasc data
        geom_point(data = filter(nasc.nearshore, cps.nasc == 0), aes(X, Y),
                   colour = 'gray50', size = 0.15, alpha = 0.5) +
        # Plot NASC data
        geom_point(data = nasc.density.plot.ns, aes(X, Y, size = bin, fill = bin),
                   shape = 21, alpha = 0.75) +
        # Configure size and colour scales
        scale_size_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                          values = dens.sizes.all.ns, labels = dens.labels.all.ns) +
        scale_fill_manual(name = bquote(atop(Biomass~density, ~'(t'~'nmi'^-2*')')),
                          values = dens.colors.all.ns, labels = dens.labels.all.ns) +
        # Plot positive cluster midpoints
        geom_shadowtext(data = pos.cluster.txt,
                        aes(X, Y, label = cluster), 
                        colour = "blue", bg.colour = "white", size = 2, fontface = "bold") +
        # Configure legend guides
        guides(fill = guide_legend(), size = guide_legend()) +
        coord_sf(crs = crs.proj, 
                 xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
                 ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
      
      # Save figures
      ggsave(biomass.dens.ns, 
             filename = paste(here("Figs/fig_biomass_dens_ns_"), i, "-", j, ".png",sep = ""),
             width  = map.width,height = map.height)
      
      # Save plot to list
      biomass.dens.figs.ns[[i]][[j]] <- biomass.dens.ns
    }
  }
  
  # Save map objects
  save(biomass.dens.figs.ns, file = here("Output/biomass_dens_ns_map_all.Rdata"))  
  
} else {
  load(here("Output/biomass_dens_ns_map_all.Rdata"))
}

# Create blank plots for missing species
for (i in cps.spp) {
  if (is.null(biomass.dens.figs.ns[[i]])) {
    df <- data.frame()
    biomass.dens.temp <- ggplot(df) + geom_point() + 
      xlim(0,10) + ylim(0,10) + 
      annotate('text', 5, 5, label = 'No Data', size = 6, fontface = 'bold') +
      theme_bw()  
    ggsave(biomass.dens.temp, 
           filename = paste(here("Figs/fig_biomass_dens_ns_"), i, ".png", sep = ""))
  }
}

# Save final nasc data frame used for point and bootstrap estimates
save(nasc.nearshore, file = here("Output/nasc_nearshore_final.Rdata"))

# Add stock and revised strata designations to strata.final.ns
strata.final.ns <- strata.final.ns %>% 
  left_join(select(nasc.stock.ns, scientificName, transect.name, stock)) %>% 
  mutate(key = paste(scientificName, stock, vessel.name, stratum)) %>% 
  select(-stratum) %>% 
  left_join(strata.nearshore.fac) %>% 
  rename(stratum = strata.fac)

# Create new list for computing biomass estimates
point.estimates.ns  <- data.frame()
nasc.summ.strata.ns <- data.frame()

# Calculate point estimates for each species
for (i in unique(strata.final.ns$scientificName)) {
  for (j in unique(strata.final.ns$vessel.name[strata.final.ns$scientificName == i])) {
    # Subset strata for species i
    strata.temp <- filter(strata.final.ns, scientificName == i, vessel.name == j) %>% 
      # left_join(nasc.stock.ns)
      select(transect.name, stratum, stock)
    
    if (nrow(strata.temp) > 0) {
      # Add stratum numbers to nasc
      nasc.ns.temp <- nasc.nearshore %>%
        select(-stratum) %>% 
        left_join(strata.temp) %>% 
        filter(!is.na(stratum))
      
      # ggplot(nasc.ns.temp, aes(long, lat, size = cps.nasc, colour = factor(stratum))) + geom_point() + coord_map()
      
      # Summarise nasc by stratum
      nasc.ns.temp.summ <- nasc.ns.temp %>% 
        group_by(stratum) %>% 
        summarise(
          n_samples = n(),
          mean_nasc = mean(cps.nasc)) %>% 
        mutate(scientificName = i, vessel.name = j) %>% 
        select(scientificName, vessel.name, everything())
      
      # Combine nasc summaries
      nasc.summ.strata.ns <- bind_rows(nasc.summ.strata.ns, 
                                       nasc.ns.temp.summ)
      
      # Create data frame with stratum and area (m^2)
      stratum.info.nearshore <- strata.nearshore %>% 
        filter(scientificName == i, vessel.name == j) %>% 
        select(stratum, area) %>%
        mutate(area = as.numeric(area)) %>% 
        st_set_geometry(NULL)
      
      # Compute point estimates
      # Currently has na.rm = TRUE for calculting biomass
      point.estimates.ns <- bind_rows(point.estimates.ns,
                                      data.frame(scientificName = i, vessel.name = j,
                                                 estimate_point(nasc.ns.temp, stratum.info.nearshore, species = i)))  
    }
  }
}

save(point.estimates.ns, 
     file = here("Output/biomass_point_estimates_ns.Rdata"))

# Save strata nasc summaries to CSV
write_csv(nasc.summ.strata.ns, here("Output/nasc_strata_summary_ns.csv"))

# Add stock designations to point estimates
point.estimates.ns <- left_join(point.estimates.ns, strata.summ.nearshore)

# Summarize point estimates (by stocks)
pe.ns <- point.estimates.ns %>%
  group_by(scientificName, stock) %>%
  summarise(
    area    = sum(area),
    biomass.total = sum(biomass.total)) %>%
  bind_rows(point.estimates.ns) %>%
  mutate(area = area * 2.915533e-07) %>%
  arrange(scientificName, stock, stratum) %>%
  replace_na(list(stratum = "All", vessel.name = "All")) %>%
  select(Species = scientificName, Stock = stock, Vessel = vessel.name, 
         Stratum = stratum, Area = area, biomass.mean.point = biomass.total)

# Save point estimates
save(pe.ns, file = here("Output/biomass_point_estimates_ns_final.Rdata"))
write_csv(pe.ns, here("Output/biomass_point_estimates_ns_final.csv"))


## Resume here
## Sort out combination of lf.df for species that don't occur in seine catch.

# Summarize positive clusters per species
if (use.seine.data) {
  pos.clusters.ns <- n.summ.haul %>% 
    bind_rows(n.summ.set) %>% 
    left_join(select(clf, cluster, lat, long, X, Y)) %>% 
    ungroup() %>% 
    mutate(stock = case_when(
      scientificName == "Engraulis mordax" & lat >= stock.break.anch ~ "Northern",
      scientificName == "Engraulis mordax" & lat <  stock.break.anch ~ "Central",
      scientificName == "Sardinops sagax"  & lat >= stock.break.sar  ~ "Northern",
      scientificName == "Sardinops sagax"  & lat <  stock.break.sar  ~ "Southern",
      scientificName %in% c("Clupea pallasii","Scomber japonicus","Trachurus symmetricus") ~ "All"))  
} else {
  pos.clusters.ns <- pos.clusters
}

# Bootstrap estimates -----------------------------------------------------
# Generate multiple bootstrap biomass estimates
if (do.bootstrap) {
  # Create data frame for biomass estimates
  bootstrap.estimates.ns <- data.frame()
  # Create data frame for abundance estimates by length
  abundance.estimates.ns <- data.frame()
  # Create data frame for stratum summaries
  survey.summary.ns <- data.frame()
  # Create data frame for catch summaries
  catch.summary.ns <- data.frame()
  # Create data frame for stratum summaries
  stratum.summary.ns <- data.frame()
  
  # Configure progress bar
  pb1 <- tkProgressBar("Bootstrap - Species", 
                       "Bootstrap Estimation (NS) - Species", 0, 100, 0)
  spp.counter <- 1
  
  for (i in unique(strata.nearshore$scientificName)) {
    
    if (use.seine.data) {
      cluster.final.ns <- cluster.final[[i]] %>% 
        mutate(sample.type = "Trawl") %>% 
        bind_rows(cluster.final.seine[[i]])
    } else {
      cluster.final.ns <- cluster.final[[i]] %>% 
        mutate(sample.type = "Trawl")
    }
    
    # Get vector of lengths from clf.df column names
    L.cols  <- grep("L\\d", names(cluster.final.ns))
    L.vec   <- sort(as.numeric(str_extract(names(cluster.final.ns[L.cols]),"\\d{1,2}")))
    
    # Configure progress bar
    pb3 <- tkProgressBar("Bootstrap - Vessel", 
                         "Bootstrap Estimation (NS) - Vessel", 0, 100, 0)
    vessel.counter <- 1
    
    for (j in unique(strata.nearshore$vessel.name[strata.nearshore$scientificName == i])) {
      # Create data frame with stratum and area (m^2)
      strata.info.nearshore <- strata.nearshore %>% 
        filter(scientificName == i, vessel.name == j) %>% 
        select(vessel.name, stratum, area) %>%
        mutate(area = as.numeric(area)) %>% 
        st_set_geometry(NULL)
      
      # Subset strata for species i
      strata.temp <- filter(strata.final.ns, scientificName == i, vessel.name == j) %>% 
        select(vessel.name, transect, stratum) %>% 
        left_join(filter(strata.summ.nearshore, scientificName == i, vessel.name == j)) %>% 
        filter(!is.na(area))
      
      # Add stratum numbers to nasc and remove transects outside of defined strata
      nasc.temp <- nasc.nearshore %>%
        filter(vessel.name == j) %>% 
        select(-stratum) %>% 
        left_join(strata.temp) %>% 
        filter(!is.na(stratum))
      
      # ggplot(nasc.temp, aes(long, lat, size = cps.nasc, colour = factor(stratum))) + geom_point() + coord_map()
      
      # Summarize nasc.temp to get strata to merge with pos.clusters below
      nasc.temp.summ <- nasc.temp %>% 
        group_by(vessel.name, stratum, cluster) %>% 
        summarise(n = n_distinct(cluster)) %>% 
        ungroup()
      
      # Summarize length data to get number of individuals
      lf.summ.cluster <- lf.final %>% 
        filter(scientificName == i) %>% 
        group_by(cluster) %>% 
        summarise(counts = sum(counts)) %>% 
        ungroup()
      
      # Summarize stratum clusters for all CPS
      stratum.cluster.cps <- nasc.temp %>% 
        group_by(cluster, stratum) %>% 
        summarise(nIntervals = n()) %>% 
        left_join(lf.summ.cluster) %>% 
        ungroup() %>% 
        replace(is.na(.), 0)
      
      pos.cluster.spp <- pos.clusters.ns %>%
        filter(cluster %in% nasc.temp$cluster, scientificName == i) %>% 
        inner_join(select(nasc.temp.summ, -n)) %>% 
        as.data.frame()
      
      # Summarize positive clusters per strata
      stratum.cluster.spp <- pos.cluster.spp %>% 
        group_by(scientificName, stratum) %>% 
        summarise(nClusters = n_distinct(cluster)) %>% 
        mutate(vessel.name = j) %>% 
        ungroup()
      
      # Summarize stratum statistics
      survey.summ.temp <- nasc.temp %>% 
        group_by(vessel.name, stratum, stock) %>% 
        summarise(
          nTransects     = n_distinct(transect),
          Distance       = length(Interval)*100/1852) %>% 
        ungroup() %>% 
        mutate(Species = i) %>% 
        left_join(stratum.cluster.spp) %>% 
        rename(Stratum = stratum)
      
      # Summarize catch statistics by stratum
      catch.summ.temp <- n.summ.haul %>% 
        left_join(select(stratum.cluster.cps, cluster, stratum)) %>% 
        filter(scientificName == i, !is.na(stratum)) %>%
        group_by(scientificName, stratum) %>% 
        summarise(nIndiv = sum(num)) %>%
        mutate(vessel.name = j) %>% 
        ungroup() %>% 
        as.data.frame()
      
      # Configure progress bar
      pb2 <- tkProgressBar("Bootstrap - Stratum", 
                           "Bootstrap Estimation (NS) - Stratum", 0, 100, 0)
      # Initialize species counter
      stratum.counter <- 1
      
      # Estimate biomass for each stratum
      for (k in unique(nasc.temp$stratum)) {
        # Extract stratum area
        stratum.area <- strata.nearshore %>% 
          filter(scientificName == i, vessel.name == j, stratum == k) %>% 
          pull(area) %>% 
          as.numeric()
        
        # Calculate biomass using bootstrap function ----
        set.seed(1) # Set seed for repeatable results
        
        boot.df <- estimate_bootstrap(nasc.temp, cluster.final.ns, k, 
                                      stratum.area = stratum.area, 
                                      species = i, do.lf = do.lf, 
                                      boot.number = boot.num)$data.frame
        
        # Extract biomass estimates; remove first (point) estimate
        boot.temp <- data.frame(Species = i, Vessel = j, Stratum = k, Area = stratum.area,
                                Sample = seq(1, boot.num), boot.df[2:nrow(boot.df), ])
        
        # Combine results
        bootstrap.estimates.ns <- bind_rows(bootstrap.estimates.ns, boot.temp)
        
        # Calculate abundance by length class using bootstrap function ----
        abund.vec <- estimate_bootstrap(nasc.temp, cluster.final.ns, k, 
                                        stratum.area = stratum.area, 
                                        species = i, do.lf = do.lf, 
                                        boot.number = 0)$abundance.vector
        # Extract abundance estimates
        abundance.temp <- data.frame(Species = i, Vessel = j, Stratum = k,
                                     SL = L.vec, freq = abund.vec)
        # Combine results
        abundance.estimates.ns <- bind_rows(abundance.estimates.ns, abundance.temp)
        
        # Update the progress bar
        pb.prog2 <- round(stratum.counter/n_distinct(nasc.temp$stratum)*100)
        info2 <- sprintf("%d%% done", pb.prog2)
        
        setTkProgressBar(pb2, pb.prog2, sprintf("Bootstrap - Stratum (%s)", info2), info2)
        # Update stratum counter
        stratum.counter <- stratum.counter + 1      
      }
      # Close the stratum counter
      close(pb2)
      
      # Update the vessel progress bar
      pb.prog3 <- round(vessel.counter/n_distinct(strata.nearshore$vessel.name)*100)
      info3 <- sprintf("%d%% done", pb.prog3)
      
      setTkProgressBar(pb3, pb.prog3, sprintf("Bootstrap - Vessel (%s)", info3), info3)
      # Update stratum counter
      vessel.counter <- vessel.counter + 1
      
      # Combine survey summary by species
      survey.summary.ns  <- bind_rows(survey.summary.ns, survey.summ.temp)
      # Combine survey summary by species
      catch.summary.ns   <- bind_rows(catch.summary.ns, catch.summ.temp)
      # Combine stratum summary
      stratum.summary.ns <- bind_rows(stratum.summary.ns, pos.cluster.spp)
    }
    # Close the vessel counter
    close(pb3)
    
    # Update the species progress bar
    pb.prog1 <- round(spp.counter/length(bootstrap.est.spp)*100)
    info1    <- sprintf("%d%% done", pb.prog1)
    
    setTkProgressBar(pb1, pb.prog1, sprintf("Bootstrap - Species (%s)", info1), info1)
    
    # Update the species counter
    spp.counter     <- spp.counter + 1
  }
  
  # Close the species counter
  close(pb1)
  
  # # Replace NaNs in abundance summaries with zeros
  # abundance.estimates.ns[atm:::is.nan.df(abundance.estimates.ns)] <- 0
  # abundance.estimates.ns[is.nan(abundance.estimates.ns)] <- 0
  # 
  # Save bootstrap results
  save(bootstrap.estimates.ns, abundance.estimates.ns, survey.summary.ns, 
       catch.summary.ns, stratum.summary.ns,
       file = (here("Output/biomass_bootstrap_est_ns.Rdata")))
  
} else{
  # Save bootstrap results
  load(here("Output/biomass_bootstrap_est_ns.Rdata"))
  
}

# Rename scientificName column
catch.summary.ns <- catch.summary.ns %>% 
  left_join(strata.summ.nearshore) %>%
  rename(Stock = stock)

# Summarise abundance across strata
abund.summ.ns <- abundance.estimates.ns %>%
  left_join(strata.summ.nearshore, by = c("Species" = "scientificName",
                                         "Stratum" = "stratum")) %>%
  group_by(Species, vessel.name, Stock = stock, SL) %>% 
  summarise(abundance = sum(freq)) %>% 
  mutate(TL = SL)  

# Calculate estimated biomass from from TL and estimated.wg --------------------
# CURRENTLY USING ESTIMATED.WG ESTIMATED FROM TOTAL LENGTH
# MUST UPDATE TO USE ESTIMATED.WG FROM STANDARD LENGTH
abund.summ.ns <- abund.summ.ns %>% 
  mutate(estimated.wg = estimate_ts(Species, TL)$estimated.wg,
         biomass      = abundance * estimated.wg)

# Add stock designations to bootstrap estimates
bootstrap.estimates.ns <- bootstrap.estimates.ns %>% 
  left_join(strata.summ.nearshore, by = c("Species" = "scientificName",
                                         "Stratum" = "stratum")) %>% 
  rename(Stock = stock)

# Remove rows where stock is NA
survey.summary.ns <- survey.summary.ns %>% 
  filter(!is.na(stock))

# Summarize results from bootstrap per species and stratum
be.stratum.ns <- bootstrap.estimates.ns %>% 
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
  left_join(survey.summary.ns, by = c("Species","Stratum")) %>%
  left_join(catch.summary.ns,  by = c("Species" = "scientificName",
                                      "Stratum" = "stratum", "Stock")) %>% 
  arrange(Species, Stratum) %>%
  select(
    Species, Stock, Stratum, Area, nTransects, Distance, nClusters, nIndiv, 
    biomass.mean.boot, lower.ci.B, upper.ci.B, biomass.sd, pct.tot.B)

# Save results
save(be.stratum.ns, 
     file = here("Output/biomass_bootstrap_estimates_stratum_ns.Rdata"))

# Add stock designations to stratum summary
stratum.summary.ns <- stratum.summary.ns %>% 
  left_join(strata.summ.nearshore) %>% 
  rename(Stock = stock)

# Summarize clusters for each species for entire survey
cluster.summary.total.ns <- stratum.summary.ns %>%
  group_by(scientificName, Stock) %>% 
  summarise(nClusters = n_distinct(cluster)) %>% 
  rename(Species = scientificName)

# Summarize sampling for entire survey
survey.summary.total.ns <- survey.summary.ns %>% 
  group_by(Species, Stock = stock) %>% 
  summarise(
    Strata     = n(),
    nTransects = sum(nTransects),
    Distance   = sum(Distance)) %>% 
  left_join(cluster.summary.total.ns)

# Summarise catch for entire survey
catch.summary.total.ns <- catch.summary.ns %>% 
  group_by(Species = scientificName, Stock) %>% 
  summarise(nIndiv = sum(nIndiv))

# Summarize results from bootstrap per species across all samples
be.sample.ns <- bootstrap.estimates.ns %>% 
  group_by(Species, Stock, Sample) %>% 
  summarise(area    = sum(Area)*2.915533e-07,
            biomass = sum(biomass))

# Summarize results from bootstrap per species across all strata
be.survey.ns <- be.sample.ns %>%
  group_by(Species, Stock) %>%
  summarise(
    Area = area[1],
    biomass.mean.boot = mean(biomass) * 10^3,
    biomass.sd = sd(biomass) * 10^3,
    lower.ci.B = quantile(biomass, probs = 0.025) * 10^3,
    upper.ci.B = quantile(biomass, probs = 0.975) * 10^3) %>%
  left_join(survey.summary.total.ns) %>%
  left_join(catch.summary.total.ns) %>%
  arrange(Species) %>%
  select(
    Species, Stock, Area, nTransects, Distance, nClusters, nIndiv, 
    biomass.mean.boot, lower.ci.B, upper.ci.B, biomass.sd
  )

# Save results
write.csv(be.survey.ns, 
          file  = here("Output/biomass_bootstrap_estimates_survey_ns.csv"),
          quote = F,row.names = F)

save(be.survey.ns, file = here("Output/biomass_bootstrap_estimates_survey_ns.Rdata"))

# Combine stratum and survey estimates
be.ns <- be.stratum.ns %>% 
  bind_rows(be.survey.ns) %>% 
  arrange(Species, Stock, Stratum) %>% 
  replace_na(list(Stratum = "All", pct.tot.B = 100)) %>%
  left_join(select(pe.ns, -Area)) %>%
  rename(Biomass = biomass.mean.point) %>% 
  mutate(biomass.cv = (biomass.sd / Biomass)*100) %>% 
  select(Species, Stock, Stratum, Area, nTransects, Distance, nClusters, nIndiv,
         Biomass, lower.ci.B, upper.ci.B, biomass.sd, biomass.cv)

be.ns[atm:::is.nan.df(be.ns)] <- NA

# Save results
write.csv(be.ns,
          file = here("Output/biomass_bootstrap_estimates_final_ns.csv"),
          quote = F, row.names = F)

save(be.ns, 
     file = here("Output/biomass_bootstrap_estimates_final_ns.Rdata"))

# Get rows with estimates from all strata
be.stratum.all.ns <- which(be.ns$Stratum == "All")

# Bootstrap estimates of biomass by species
biomass.histogram.survey.ns <- ggplot(be.sample.ns, aes(biomass*1e3, fill = Stock)) + 
  geom_histogram(alpha = 0.75) + facet_wrap(Species ~ Stock, scales = "free") + 
  geom_vline(data = filter(pe.ns, Stratum == "All"),aes(xintercept = biomass.mean.point)) +
  geom_vline(data = be.survey.ns, aes(xintercept = lower.ci.B), linetype = 'dashed') +
  geom_vline(data = be.survey.ns, aes(xintercept = upper.ci.B), linetype = 'dashed') +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c(All = "gray50", Central = "orange", 
                               Northern = "navyblue", Southern = "firebrick")) +
  ylab("Count") + xlab(expression(Biomass~(t))) +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

# Save figure
ggsave(biomass.histogram.survey.ns,
       filename = here("Figs/fig_biomass_histogram_survey_ns.png"),
       height = 8, width = 14)
