# Remove any existing nearshore backscatter data from memory
if (exists("nasc.nearshore")) {rm(nasc.nearshore)}

# Estimate nearshore biomass -----------------------------------------------
# List nearshore backscatter files
nearshore.files <- dir_ls(here("Data/Backscatter"), recurse = TRUE,
                         regexp = "nearshore.rds")

# Import all offshore backscatter data
for (o in nearshore.files) {
  if (exists("nasc.nearshore")) {
    nasc.nearshore <- bind_rows(nasc.nearshore, readRDS(nearshore.files[o]))
  } else {
    nasc.nearshore <- readRDS(nearshore.files[o])
  }
}

# Remove vessels not to be included in the offshore biomass estimates
nasc.nearshore <- nasc.nearshore %>% 
  filter(vessel.name %in% nasc.vessels.nearshore) %>%
  mutate(
    transect = str_replace(transect, "N", ""),
    transect.name = paste(vessel.name, transect),
    cps.nasc      = NASC.50,
    stratum       = 1,
    int = cut(Interval, seq(1, max(Interval) + nasc.summ.interval,
                            nasc.summ.interval),
              labels = F, include.lowest = TRUE)) %>% 
  mutate(transect = as.numeric(transect))

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

# Save nasc cluster vector
save(cluster.distance.ns, file = here("Output/nasc_cluster_distance_nearshore.Rdata"))

# Add cluster distances to nasc
nasc.nearshore <- bind_cols(nasc.nearshore, cluster.distance.ns) %>% 
  project_df(to = crs.proj)

nasc.nearshore.summ <- nasc.nearshore %>% 
  group_by(cluster) %>% 
  tally() %>% 
  filter(n >= 4)

# Map trawl clusters -------------------------------------------------------
# Create hulls around positive clusters
nasc.super.clusters.ns <- nasc.nearshore %>% 
  filter(cluster %in% nasc.nearshore.summ$cluster) %>% 
  plyr::ddply("cluster", find_hull) %>% 
  select(long, lat, cluster) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(cluster) %>% 
  summarise(do_union = F) %>% 
  st_cast("POLYGON")

if (save.figs) {
  nasc.cluster.plot.ns <- base.map +
    # Plot nasc data
    geom_point(data = nasc.nearshore, aes(X, Y, colour = factor(cluster)),
               show.legend = FALSE) +
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
         bin.level = as.numeric(bin))

# Create acoustic transect labels for maps
tx.labels.ns <- nasc.nearshore %>% 
  ungroup() %>%
  group_by(transect.name) %>%
  summarise(
    vessel.name = vessel.name[1],
    transect = transect.name[1],
    start.lat = lat[which.max(long)],
    start.long = max(long),
    end.lat = lat[which.min(long)],
    end.long = min(long),
    brg = 90 - swfscMisc::bearing(end.lat,end.long,start.lat,start.long)[1]) %>% 
  mutate(lat  = start.lat,
         long = start.long) %>% 
  project_df(to = crs.proj)

# Summarise biomass density by transect and species
nasc.density.summ.ns <- nasc.density.ns %>% 
  group_by(scientificName, transect, transect.name) %>% 
  summarise(density = sum(density)) %>% 
  filter(scientificName %in% unique(lengths$scientificName)) %>% 
  left_join(select(tx.labels.ns, transect.name, lat, long)) %>% 
  # left_join(select(tx.nn.ns, transect, vessel.name, dist.cum, dist.bin)) %>% 
  mutate(positive = density > 0)

# Save biomass density data
save(nasc.density.ns, nasc.density.summ.ns, 
     file = here("Output/nasc_biomass_density_ns.Rdata"))

# Select legend objects 
dens.levels.all.ns <- sort(unique(nasc.density.ns$bin.level))
dens.labels.all.ns <- dens.labels[dens.levels.all.ns]
dens.sizes.all.ns  <- dens.sizes[dens.levels.all.ns]
dens.colors.all.ns <- dens.colors[dens.levels.all.ns]

# Stratify ----------------------------------------------------------------
# Get transect spacing for plotting --------------------------
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
  arrange(transect)

if (!is.na(tx.spacing.ns)) {
  tx.nn.ns <- tx.nn.ns %>% 
    mutate(spacing = tx.spacing.ns,
           dist.cum = cumsum(spacing))
  }

# Save nearest neighbor distance info
save(tx.nn.ns, file = here("Output/transect_spacing_ns.Rdata"))

# Summarise biomass density by transect and species
nasc.density.summ.ns <- nasc.density.summ.ns %>% 
  left_join(select(tx.nn.ns, transect.name, dist.cum, dist.bin))

# Draw pseudo-transects --------------------------------------------------------
# Get transect ends, calculate bearing, and add transect spacing

# DEFINE METHOD OF DRAWING PSEUDO-TRANSECTS
# USE SQUARE ENDS FOR TYPICAL TRANSECTS
# USE CONNECTING ENDS FOR TRANSECTS AROUND ISLANDS

# Assign transects to region based on latitude
# Because some duplicate transect numbers exist, can't do a straight join
nasc.region <- data.frame()

# Remove nearshore strata, if exists
if (exists("strata.ns")) rm(strata.ns) 

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
    mutate(transect = as.numeric(Transect)) %>% 
    ungroup()
  
  # Add region to nasc by vessel
  nasc.region.temp <- nasc.nearshore %>% 
    filter(vessel.name == v) %>%  
    left_join(select(region.wpts, transect, region = Region)) %>% 
    mutate(key = paste(vessel.name, region))
  
  # For each region, draw strata polygon using NASC intervals or deepest waypoints
  for (k in unique(nasc.region.temp$key)) {
    # If an island region, create inner and outer polygons, combine, and compute area
    if (str_detect(k, "Island")) {
      # Draw polygons around shallowest transect waypoint for island strata
      ns.poly.i <- region.wpts %>%
        group_by(transect) %>%
        slice(which.min(abs(depth))) %>% 
        mutate(key = paste(v, Region)) %>% 
        filter(key == k) %>%
        ungroup() %>% 
        st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
        summarise(do_union = F) %>% 
        st_cast("LINESTRING") %>% 
        st_cast("POLYGON") 
      
      # Draw polygons around deepest transect waypoint for island strata
      ns.poly.o <- region.wpts %>%
        group_by(transect) %>%
        slice(which.max(abs(depth))) %>% 
        mutate(key = paste(v, Region)) %>% 
        filter(key == k) %>%
        ungroup() %>% 
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
               area = as.numeric(st_area(.)))
      
    } else {
      # If not, use same method used for drawing mainland polygons
      
      # Draw pseudo-transects --------------------------------------------------------
      # Get transect ends, calculate bearing, and add transect spacing
      tx.ends.ns <- nasc.region.temp %>% 
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
      tx.i <- tx.ends.ns %>% 
        select(-lat.o, -long.o) %>% 
        rename(lat = lat.i, long = long.i) %>% 
        mutate(
          grp = "original",
          loc = "inshore",
          order = 2)
      
      # Get N and S inshore waypoints ------------------------------------------------
      # Calculate inshore/north transects
      tx.i.n <- tx.ends.ns %>% 
        select(-lat.o, -long.o) %>% 
        rename(lat = lat.i, long = long.i) %>% 
        mutate(
          lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
          long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
          grp = "north",
          loc = "inshore",
          order = 1)
      
      # Calculate inshore/south transects
      tx.i.s <- tx.ends.ns %>% 
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
        tx.i <- tx.i %>%
          bind_rows(tx.i.n) %>%
          bind_rows(tx.i.s) %>%
          arrange(desc(transect), desc(order))
      } else {
        tx.i <- tx.i %>%
          bind_rows(tx.i.n) %>%
          bind_rows(tx.i.s) %>%
          arrange(transect, desc(order))  
      }
      
      # Get original offshore transect ends ------------------------------------------
      tx.o <- tx.ends.ns %>% 
        select(-lat.i, -long.i) %>% 
        rename(lat = lat.o, long = long.o) %>% 
        mutate(
          grp = "original",
          loc = "offshore",
          order = 2)
      
      # Get N and S offshore waypoints -----------------------------------------------
      # Calculate offshore/north transects
      tx.o.n <- tx.ends.ns %>% 
        select(-lat.i, -long.i) %>% 
        rename(lat = lat.o, long = long.o) %>% 
        mutate(
          lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
          long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
          grp = "north",
          loc = "offshore",
          order = 3)
      
      # Calculate offshore/south transects
      tx.o.s <- tx.ends.ns %>% 
        select(-lat.i, -long.i) %>% 
        rename(lat = lat.o, long = long.o) %>% 
        mutate(
          lat  = destination(lat, long, brg - 90, spacing/2, units = "nm")["lat"],
          long = destination(lat, long, brg - 90, spacing/2, units = "nm")["lon"],
          grp = "south",
          loc = "offshore",
          order = 1)
      
      tx.o.final <- data.frame()
      
      for (v in unique(tx.o$vessel.name)) {
        if (v == "SD1024") {
          tx.o.tmp <- filter(tx.o, vessel.name == v)
        } else {
          tx.o.tmp <- filter(tx.o, vessel.name == v) %>% 
            bind_rows(filter(tx.o.n, vessel.name == v)) %>% 
            bind_rows(filter(tx.o.s, vessel.name == v))
        }
        tx.o.final <- bind_rows(tx.o.final, tx.o.tmp)
      }
      
      # Combine all inshore transects
      if (v == "LBC" & survey.name == "1907RL") {
        # Transect numbers were reversed in 2019
        tx.o <- tx.o.final %>% 
          arrange(transect, desc(order))
      } else {
        tx.o <- tx.o.final %>% 
          arrange(desc(transect), desc(order))
      }

      # Assemble the final data frame with all waypoints -----------------------------
      strata.points.ns <- tx.i %>% 
        bind_rows(tx.o)   %>%
        mutate(key = paste(transect.name, grp)) %>% 
        st_as_sf(coords = c("long","lat"), crs = crs.geog) 
      
      # Create polygons
      ns.poly.k <- strata.points.ns %>% 
        group_by(vessel.name) %>% 
        summarise(do_union = F) %>% 
        st_cast("POLYGON") %>% 
        st_make_valid() %>% 
        st_difference(st_union(bathy_5m_poly)) %>% 
        mutate(area = st_area(.))
      
      # Convert to lines
      ns.lines.sf <- strata.points.ns %>% 
        group_by(transect, grp) %>% 
        summarise(do_union = F) %>% 
        st_cast("LINESTRING")
      
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

# for (k in unique(nasc.region$key)) {
#   if (str_detect(k, "Island")) {
#     # Draw polygons around deepest transect waypoint for island strata
#     ns.strata.temp <- region.wpts %>%
#       ungroup() %>%
#       mutate(key = paste("LBC", Region)) %>% 
#       # filter(key == k) %>% 
#       st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
#       concaveman(concavity = 10)  
#   } else {
#     # Draw polygons around easternmost and westernmost nasc intervals
#     
#   }
#   ns.strata.temp <- nasc.region %>%
#     filter(key == k) %>% 
#     st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
#     concaveman(concavity = 10)  
# }
# 
# 
# # Do st_intersection to remove Channel Islands, particularly for LCB data
# tx.ends.ns <- nasc.nearshore %>% 
#   group_by(transect.name, transect, vessel.name) %>% 
#   summarise(
#     lat.i  = lat[which.max(long)],
#     long.i = max(long),
#     lat.o  = lat[which.min(long)],
#     long.o = min(long)
#   ) %>% 
#   left_join(select(tx.nn.ns, transect.name, spacing)) %>% 
#   mutate(
#     brg = swfscMisc::bearing(lat.i, long.i,
#                              lat.o, long.o)[1])
# 
# # Get original inshore transect ends -------------------------------------------
# tx.i.ns <- tx.ends.ns %>% 
#   select(-lat.o, -long.o) %>% 
#   rename(lat = lat.i, long = long.i) %>% 
#   mutate(
#     grp = "original",
#     loc = "inshore",
#     order = 2)
# 
# # Get N and S inshore waypoints ------------------------------------------------
# # Calculate inshore/north transects
# tx.i.n.ns <- tx.ends.ns %>% 
#   select(-lat.o, -long.o) %>% 
#   rename(lat = lat.i, long = long.i) %>% 
#   mutate(
#     lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
#     long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
#     grp = "north",
#     loc = "inshore",
#     order = 1)
# 
# # Calculate inshore/south transects
# tx.i.s.ns <- tx.ends.ns %>% 
#   select(-lat.o, -long.o) %>% 
#   rename(lat = lat.i, long = long.i) %>% 
#   mutate(
#     lat  = destination(lat, long, brg - 90, spacing/2, units = "nm")["lat"],
#     long = destination(lat, long, brg - 90, spacing/2, units = "nm")["lon"],
#     grp = "south",
#     loc = "inshore",
#     order = 3)
# 
# # Combine all inshore transects
# tx.i.ns <- tx.i.ns %>%
#   bind_rows(tx.i.n.ns) %>%
#   bind_rows(tx.i.s.ns) %>%
#   arrange(transect, desc(order))
# 
# # Get original offshore transect ends ------------------------------------------
# tx.o.ns <- tx.ends.ns %>% 
#   select(-lat.i, -long.i) %>% 
#   rename(lat = lat.o, long = long.o) %>% 
#   mutate(
#     grp = "original",
#     loc = "offshore",
#     order = 2)
# 
# # Get N and S offshore waypoints -----------------------------------------------
# # Calculate offshore/north transects
# tx.o.n.ns <- tx.ends.ns %>% 
#   select(-lat.i, -long.i) %>% 
#   rename(lat = lat.o, long = long.o) %>% 
#   mutate(
#     lat  = destination(lat, long, brg + 90, spacing/2, units = "nm")["lat"],
#     long = destination(lat, long, brg + 90, spacing/2, units = "nm")["lon"],
#     grp = "north",
#     loc = "offshore",
#     order = 1)
# 
# # Calculate inshore/south transects
# tx.o.s.ns <- tx.ends.ns %>% 
#   select(-lat.i, -long.i) %>% 
#   rename(lat = lat.o, long = long.o) %>% 
#   mutate(
#     lat  = destination(lat, long, brg - 90, spacing/2, units = "nm")["lat"],
#     long = destination(lat, long, brg - 90, spacing/2, units = "nm")["lon"],
#     grp = "south",
#     loc = "offshore",
#     order = 3)
# 
# # Combine all offshore transects
# tx.o.final.ns <- tx.o.ns %>% 
#   bind_rows(filter(tx.o.n.ns)) %>% 
#   bind_rows(filter(tx.o.s.ns)) %>% 
#   arrange(desc(transect), order)
# 
# # Assemble the final data frame with all waypoints -----------------------------
# strata.points.ns <- tx.i.ns %>% 
#   bind_rows(tx.o.final.ns)   %>%
#   mutate(key = paste(transect.name, grp)) 
# 
# # Convert to points
# strata.points.ns.sf <- st_as_sf(strata.points.ns, coords = c("long","lat"), crs = 4326) 
# 
# # Create polygons
# strata.super.polygons.ns <- strata.points.ns.sf %>% 
#   ungroup() %>% 
#   # group_by(vessel.name) %>% 
#   summarise(do_union = F) %>% 
#   st_cast("POLYGON") %>% 
#   st_make_valid() %>% 
#   st_difference(st_union(bathy_5m_poly)) %>% 
#   st_difference(filter(strata.super.polygons, vessel.name == "RL")) %>% 
#   mutate(area = st_area(.))
# 
# # Convert to lines
# tx.lines.ns.sf <- strata.points.ns.sf %>% 
#   group_by(transect, grp) %>% 
#   summarise(do_union = F) %>% 
#   st_cast("LINESTRING")

# # Determine transect order by min latitude/longitude
# tx.order.ns <- data.frame()
# 
# use.nums <- TRUE
# 
# if (use.nums) {
#   # Calculate transect order per vessel
#   tx.order.temp <- nasc.nearshore %>%
#     # filter(str_detect(vessel.name, i)) %>% 
#     group_by(transect.name) %>% 
#     summarise(max.lat  = max(lat),
#               min.long = max(long)) %>% 
#     mutate(rank.lat  = rank(max.lat), 
#            rank.long = rev(rank(min.long)), 
#            diff      = rank.lat - rank.long,
#            transect.num = as.numeric(str_extract(transect.name,"[[:digit:]]+")),
#            rank.final = rank(transect.num)) %>% 
#     arrange(rank.final)
#   
# } else {
#   # Calculate transect order per vessel
#   tx.order.temp <- nasc.nearshore %>%
#     # filter(str_detect(vessel.name, i)) %>% 
#     group_by(transect.name) %>% 
#     summarise(max.lat  = max(lat),
#               min.long = max(long)) %>% 
#     mutate(rank.lat  = rank(max.lat), 
#            rank.long = rev(rank(min.long)), 
#            diff      = rank.lat - rank.long,
#            transect.num = as.numeric(str_extract(transect.name,"[[:digit:]]+")),
#            rank.final = rank.lat) %>% 
#     arrange(rank.final)
# }
# 
# # Assign transect order based on rank latitude
# if (nrow(tx.order.ns) == 0) {
#   # If the first vessel, create transects from 0 to number of transects
#   tx.order.temp$transect <- tx.order.temp$rank.final
# } else {
#   # If not the first vessel, add rank latitude to largest existing transect number
#   tx.order.temp$transect <- tx.order.temp$rank.final + max(tx.order.ns$transect)
# }
# 
# # Combine results from all vessels
# tx.order.ns <- bind_rows(tx.order.ns, tx.order.temp) %>% 
#   arrange(transect)
# 
# # Add transect numbers to NASC data frame
# nasc.nearshore <- left_join(select(nasc.nearshore, -transect), 
#                            select(tx.order.ns, transect.name, transect))
# 
# # Add transect numbers to NASC density summary data frame
# nasc.density.summ.ns <- nasc.density.summ.ns %>% 
#   ungroup() %>% 
#   select(-transect) %>% 
#   left_join(select(tx.order.ns, transect.name, transect))
# 
# nasc.density.ns <- nasc.density.ns %>% 
#   ungroup() %>% 
#   select(-transect) %>% 
#   left_join(select(tx.order.ns, transect.name, transect))
# 
# # Add transect numbers to strata points data frame
# strata.points.ns <- strata.points.ns %>% 
#   ungroup() %>% 
#   select(-transect) %>% 
#   left_join(select(tx.order.ns, transect.name, transect))

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
    # Select positive transects and calculate differences between transect numbers
    # diffs >= 2 define stratum breaks
    temp.spp <- filter(nasc.density.summ.ns, scientificName == i) %>%
      filter(positive == TRUE) %>%
      mutate(diff = c(1, diff(transect))) %>%
      ungroup()

    if (nrow(temp.spp) > 0) {
      # Find the start of each positive stratum
      spp.starts <- temp.spp %>%
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
                               stratum = seq(1,length(stratum.start)),
                               start = stratum.start,
                               end = stratum.end) %>%
        mutate(n.tx = end - start + 1)

      # Create stratum vectors
      strata.df <- data.frame()
      
      for (j in 1:nrow(strata.spp)) {
        # Create a vector of transects from start to end
        transect <- seq(strata.spp$start[j],strata.spp$end[j])

        # Combine results
        strata.df <- bind_rows(strata.df,
                               data.frame(scientificName = i,
                                          stratum = j,
                                          transect))
      }

      # Add vessel name and distance bin to strata.df for final cuts
      strata.df <- strata.df %>%
        left_join(filter(select(nasc.density.summ.ns, scientificName, transect, dist.bin),
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
      strata.final.ns <- bind_rows(strata.final.ns, strata.df)
    }
  }
}

# Create acoustic transect labels for offshore maps
tx.labels.ns <- nasc.nearshore %>% 
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
strata.final.ns <- strata.final.ns %>%
  left_join(select(tx.labels.ns, -end.lat, -end.long, -brg)) %>%
  filter(!is.na(vessel.name))

# Create final strata and calculate area
# Create df for transect-level stock info
nasc.stock.ns <- data.frame()
# 
# strata.final.ns <- strata.manual.ns %>% 
#   mutate(stratum.orig = stratum)

# Summarise NASC to get species present
nearshore.spp <- nasc.prop.spp.ns %>% 
  filter(nasc > 0) %>% 
  group_by(scientificName) %>% 
  tally()

if (exists("strata.offshore")) rm(strata.offshore)

for (i in unique(nearshore.spp$scientificName)) {
  # Select each strata per species
  strata.sub <- filter(strata.final.ns, scientificName == i) %>% 
    select(transect, stratum)  
  
  # Define strata to stock
  nasc.stock.temp <- strata.points.ns %>% 
    filter(loc == "inshore") %>%
    left_join(strata.sub) %>% 
    mutate(stock = case_when(
      i == "Engraulis mordax" & stratum == 2 ~ "Northern",
      i == "Engraulis mordax" & stratum == 1 ~ "Central",
      i == "Sardinops sagax"  & stratum == 1  ~ "Northern",
      i == "Sardinops sagax"  & stratum == 1  ~ "Southern",
      i %in% c("Clupea pallasii","Scomber japonicus","Trachurus symmetricus") ~ "All"),
      scientificName = i) %>% 
    select(transect, stock, scientificName) %>% 
    distinct()
  
  # Combine results
  nasc.stock.ns <- bind_rows(nasc.stock.ns, nasc.stock.temp)
  
  for (j in sort(unique(strata.sub$stratum))) {
    # Create offshore stratum polygons ----------------------------------------
    # Add stratum numbers and stock designation to strata.points
    primary.poly.temp <- strata.points.ns %>% 
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

# Save strata polygons
save(strata.offshore, 
     file = here("Output/strata_offshore_raw.Rdata"))

# Clip primary polygons using the 5 m isobathy polygon -------------------------
# Summarise strata transects
strata.summ.ns <- strata.final.ns %>% 
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
strata.nearshore.points  <- strata.offshore %>% 
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

# Write offshore stata points to CSV
write.csv(strata.nearshore.points,  
          file = here("Output/strata_points_nearshore.csv"),
          quote = F, row.names = F)

# Summarize nasc.strata by stock
strata.summ.offshore <- strata.offshore %>% 
  select(scientificName, stratum, stock, area) %>%
  mutate(area = as.numeric(area)) %>% 
  st_set_geometry(NULL)

if (save.figs) {
  # Add strata polygons to acoustic proportions map
  map.stratum.all.ns <- base.map + 
    # Plot final strata
    geom_sf(data = strata.offshore, aes(colour = factor(stratum), 
                                       fill = stock), alpha = 0.5) +
    # Plot proportion of backscatter from each species present
    geom_point(data = nasc.prop.all.ns, aes(X, Y, size = cps.nasc), 
               colour = "gray20", show.legend = F) +
    geom_point(data = filter(nasc.prop.spp.ns, nasc > 0),
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
  ggsave(here("Figs/fig_nasc_acoustic_proportions_strata_ns.png"), map.stratum.all.ns,
         width = map.width*2.6, height = map.height*1.5)
}

# Convet nasc.density to sf and project
nasc.density.ns <- ungroup(nasc.density.ns) %>% 
  project_df(to = crs.proj)

# Summarize positive clusters to filter clusters from removed strata
pos.cluster.summ <- pos.clusters %>% 
  group_by(scientificName, cluster) %>% 
  summarise(nIndiv = sum(num)) %>% 
  filter(nIndiv >= nIndiv.min)

pos.clusters.ns <- pos.clusters

nasc.ns.clusters <- sort(unique(nasc.nearshore$cluster))

if (save.figs) {
  # Create a list for saving biomass density plots
  biomass.dens.figs.ns <- list()
  
  # Plot anchovy biomass density
  for (i in unique(strata.offshore$scientificName)) {
    for (j in unique(filter(strata.offshore, scientificName == i)$stock)) {
      # Filter biomass density
      nasc.density.plot.ns <- nasc.density.ns %>%
        left_join(filter(nasc.stock.ns, scientificName == i, stock == j)) %>% 
        filter(density != 0, scientificName == i, 
               stock == j, transect %in% strata.final.ns$transect[strata.final.ns$scientificName == i])
      
      # Filter positive clusters
      pos.cluster.txt <- pos.clusters.ns %>% 
        filter(scientificName == i, stock == j,
               cluster %in% nasc.ns.clusters) %>% 
        ungroup() %>% 
        project_df(to = crs.proj)
      
      # Map biomass density, strata polygons, and positive trawl clusters
      biomass.dens.ns <- base.map +
        geom_sf(data = filter(strata.offshore, scientificName == i, stock == j),
                aes(colour = factor(stratum)), fill = NA, size = 1) +
        scale_colour_discrete('Stratum') + 
        # Plot vessel track
        geom_sf(data = nav.paths.sf, colour = 'gray50', size = 0.25, alpha = 0.5) +
        # Plot zero nasc data
        geom_point(data = filter(nasc.nearshore, cps.nasc == 0), aes(X, Y),
                   colour = 'gray50', size = 0.15, alpha = 0.5) +
        # Plot NASC data
        geom_point(data = nasc.density.plot.ns, aes(X, Y, size = bin, fill = bin),
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
        guides(fill = guide_legend(), size = guide_legend()) +
        coord_sf(crs = crs.proj, 
                 xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
                 ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
      
      # Save figures
      ggsave(biomass.dens.ns, 
             filename = paste(here("Figs/fig_biomass_dens_os_"), i, "-", j, ".png",sep = ""),
             width  = map.width,height = map.height)
      
      # Save plot to list
      biomass.dens.figs.ns[[i]][[j]] <- biomass.dens.ns
    }
  }
  # Save map objects
  save(biomass.dens.figs.ns, file = here("Output/biomass_dens_os_map_all.Rdata"))  
} else {
  load(here("Output/biomass_dens_os_map_all.Rdata"))
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
           filename = paste(here("Figs/fig_biomass_dens_os_"), i, ".png", sep = ""))
  }
}

# Save final nasc data frame used for point and bootstrap estimates
save(nasc.nearshore, file = here("Output/nasc_offshore_final.Rdata"))

# Create new list for computing biomass estimates
point.estimates.offshore <- data.frame()
nasc.summ.strata.ns      <- data.frame()

# Calculate point estimates for each species
for (i in unique(strata.final.ns$scientificName)) {
  # Subset strata for species i
  strata.temp <- filter(strata.final.ns, scientificName == i) %>% 
    select(transect, stratum)
  
  # Add stratum numbers to nasc
  nasc.ns.temp <- nasc.nearshore %>%
    left_join(strata.temp) %>% 
    filter(!is.na(stratum))
  
  # Summarise nasc by stratum
  nasc.ns.temp.summ <- nasc.ns.temp %>% 
    group_by(stratum) %>% 
    summarise(
      n_samples = n(),
      mean_nasc = mean(cps.nasc)) %>% 
    mutate(scientificName = i) %>% 
    select(scientificName, everything())
  
  # Combine nasc summaries
  nasc.summ.strata.ns <- bind_rows(nasc.summ.strata.ns, 
                                nasc.ns.temp.summ)
  
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
                                                    estimate_point(nasc.nearshore, stratum.info.offshore, species = i)))
}

save(point.estimates.offshore, 
     file = here("Output/biomass_point_estimates_ns.Rdata"))

# Save strata nasc summaries to CSV
write_csv(nasc.summ.strata.ns, here("Output/nasc_strata_summary_ns.csv"))

# Add stock designations to point estimates
point.estimates.offshore <- left_join(point.estimates.offshore, strata.summ.offshore)

# Summarize point estimates (by stocks)
pe.ns <- point.estimates.offshore %>%
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
save(pe.ns, file = here("Output/biomass_point_estimates_os_final.Rdata"))
write_csv(pe.ns, here("Output/biomass_point_estimates_os_final.csv"))

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
    strata.temp <- filter(strata.final.ns, scientificName == i) %>% 
      select(transect, stratum) %>% 
      left_join(filter(strata.summ.offshore, scientificName == i)) %>% 
      filter(!is.na(area))
    
    # Add stratum numbers to nasc and remove transects outside of defined strata
    nasc.temp <- nasc.nearshore %>%
      left_join(strata.temp) %>% 
      filter(!is.na(stratum))

    # Summarize nasc.temp to get strata to merge with pos.clusters below
    nasc.temp.summ <- nasc.nearshore %>% 
      group_by(stratum, cluster) %>% 
      summarise(n = n_distinct(cluster))
    
    # Summarize length data to get number of individuals
    lf.summ.cluster <- lf.final %>% 
      filter(scientificName == i) %>% 
      group_by(cluster) %>% 
      summarise(counts = sum(counts))
    
    # Summarize stratum clusters for all CPS
    stratum.cluster.cps <- nasc.nearshore %>% 
      group_by(cluster, stratum) %>% 
      summarise(nIntervals = n()) %>% 
      left_join(lf.summ.cluster)
    
    # Summarize positive clusters per species
    pos.cluster.spp <- pos.clusters %>%
      filter(cluster %in% nasc.temp$cluster & scientificName == i) %>% 
      inner_join(select(nasc.temp.summ,-n)) %>% 
      as.data.frame()
    
    # Summarize positive clusters per strata
    stratum.cluster.spp <- pos.cluster.spp %>% 
      group_by(scientificName, stratum) %>% 
      summarise(nClusters = n_distinct(cluster))
    
    # Summarize stratum statistics
    survey.summ.temp <- nasc.temp %>% 
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
      bootstrap.estimates.ns <- bind_rows(bootstrap.estimates.ns, boot.temp)

      # Calculate abundance by length class using bootstrap function ----
      abund.vec <- estimate_bootstrap(nasc.nearshore, cluster.final[[i]], j, 
                                      stratum.area = stratum.area, 
                                      species = i, do.lf = do.lf, 
                                      boot.number = 0)$abundance.vector
      # Extract abundance estimates
      abundance.temp <- data.frame(Species = i, Stratum = j,
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
    
    # Update the progress bar
    pb.prog1 <- round(spp.counter/length(bootstrap.est.spp)*100)
    info1    <- sprintf("%d%% done", pb.prog1)
    
    setTkProgressBar(pb1, pb.prog1, sprintf("Bootstrap - Species (%s)", info1), info1)
    
    # Update the species counter
    spp.counter     <- spp.counter + 1
    # Combine survey summary by species
    survey.summary.ns  <- bind_rows(survey.summary.ns, survey.summ.temp)
    # Combine survey summary by species
    catch.summary.ns   <- bind_rows(catch.summary.ns, catch.summ.temp)
    # Combine stratum summary
    stratum.summary.ns <- bind_rows(stratum.summary.ns, pos.cluster.spp)
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
  left_join(strata.summ.offshore) %>%
  rename(Stock = stock)

# Summarise abundance across strata
abund.summ.ns <- abundance.estimates.ns %>%
  left_join(strata.summ.offshore, by = c("Species" = "scientificName",
                                          "Stratum" = "stratum")) %>%
  group_by(Species, Stock = stock, SL) %>% 
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
  left_join(strata.summ.offshore, by = c("Species" = "scientificName",
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
  left_join(strata.summ.offshore) %>% 
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
