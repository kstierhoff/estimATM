# This script assigns deep backscatter to N. anchovy in areas where daytime
# sampling does not adequately sample deep schools (e.g., nearshore purse seines)

# For vessels specified in deep.nasc.vessels,
# replace cps.nasc with NASC.20, to examine the contribution of deep anchovy schools to the sardine estimates
if (exists("deep.nasc.vessels")) {
  nasc.nearshore <- nasc.nearshore %>%
    # Retain original cps.nasc
    # Create deep backscatter variable
    mutate(cps.nasc.orig = cps.nasc) %>%
    # Compute deep backscatter variable
    # If NASC.20 is greater than cps.nasc (e.g., when backscatter near the surface was removed in nascR),
    # cps.nasc.deep = cps.nasc, else the difference between NASC.20 and cps.nasc
    mutate(cps.nasc.deep = case_when(
      NASC.20 > cps.nasc ~ cps.nasc,
      TRUE ~ cps.nasc - NASC.20)) %>% 
    # Remove deep backscatter from cps.nasc for vessels defined in settings.
    # Deep backscatter will be apportioned separately below and added back to cps.nasc
    # prior to biomass estimation.
    mutate(cps.nasc = case_when(
      cps.nasc.deep >= cps.nasc & vessel.orig %in% deep.nasc.vessels ~ cps.nasc.deep - cps.nasc,
      TRUE ~ cps.nasc))
  
  # ggplot(nasc.nearshore, aes(cps.nasc, NASC.20)) + geom_point(aes(colour = vessel.orig)) + facet_wrap(~vessel.orig)
  # ggplot(nasc.nearshore, aes(cps.nasc, cps.nasc.deep)) + geom_abline(slope = 1, intercept = 0) + geom_point(aes(colour = vessel.orig)) + facet_wrap(~vessel.orig)
}

if (process.seine) {
  # Do deep data
  clf.ns.deep <- clf %>%
    bind_rows(clf.seine.deep) %>% 
    filter(sample.type %in% catch.source.ns)
  
  hlf.ns.deep <- hlf %>% 
    ungroup() %>% 
    bind_rows(clf.seine.deep) %>% 
    filter(sample.type %in% catch.source.ns)  
  
  lf.final.ns.deep <- lf.final %>% 
    bind_rows(lf.final.seine.deep) %>% 
    filter(sample.type %in% catch.source.ns)
  
  # Combine super.clusters and super.clusters.ns.deep
  super.clusters.ns.deep <- bind_rows(super.clusters, super.clusters.ns.deep) %>% 
    filter(sample.type %in% catch.source.ns)
  
  super.hauls.ns.deep    <- bind_rows(super.hauls, super.clusters.ns.deep) %>% 
    filter(sample.type %in% catch.source.ns)
  
  save(super.clusters.ns.deep, super.hauls.ns.deep,
       file = here("Output/super_clusters_hauls_ns_deep.Rdata"))
} else {
  load(here("Output/seine_summaries_deep.Rdata"))
  load(here("Output/cluster_length_frequency_all_seine_deep.Rdata"))
  load(here("Output/cluster_length_frequency_tables_seine_deep.Rdata"))
  load(here("Output/super_clusters_hauls_ns_deep.Rdata"))
}


save(clf.ns.deep, hlf.ns.deep, super.clusters.ns.deep, super.hauls.ns.deep, lf.final.ns.deep, set.pie.deep, 
     file = here("Output/clf_nearshore_deep.Rdata"))

# Assign backscatter to trawl clusters ------------------------------------
cluster.match.ns.deep <- super.clusters.ns.deep %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  filter(sample.type %in% catch.source.ns)

haul.match.ns.deep <- super.hauls.ns.deep %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  filter(sample.type %in% catch.source.ns)

# Find nearest cluster ----------------------------
# Returns a vector of nearest clusters
nearest.cluster.ns.deep <- st_nearest_feature(nasc.match.ns, cluster.match.ns.deep)
nearest.haul.ns.deep <- st_nearest_feature(nasc.match.ns, haul.match.ns.deep)

# Expand clf to match nasc ------------------------
cluster.ns.sp.deep <- cluster.match.ns.deep[nearest.cluster.ns.deep, ] %>% 
  select(geometry) %>% 
  as_Spatial()

haul.ns.sp.deep <- haul.match.ns.deep[nearest.haul.ns.deep, ] %>% 
  select(geometry) %>% 
  as_Spatial()

# Compute distances with {geosphere}
## Must be done in WGS84 projection
nasc.match.ns <- nasc.match.ns %>% 
  mutate(cluster.deep = cluster.match.ns.deep$cluster[nearest.cluster.ns.deep],
         cluster.distance.deep = distGeo(nasc.ns.sp, cluster.ns.sp.deep)*0.000539957,
         haul.deep = haul.match.ns.deep$haul[nearest.haul.ns.deep],
         haul.distance.deep = distGeo(nasc.ns.sp, haul.ns.sp.deep)*0.000539957)

# Add clusters and cluster distances to nasc
nasc.nearshore <- nasc.nearshore %>% 
  bind_cols(select(nasc.match.ns, cluster.deep, cluster.distance.deep, haul.deep, haul.distance.deep)) 

# Load processed data
load(here("Output/clf_nearshore_deep.Rdata"))
load(here("Output/seine_summaries_deep.Rdata"))

# Map trawl clusters -------------------------------------------------------
# Create hull polygons around positive clusters and hauls
nasc.super.clusters.ns.deep <- nasc.nearshore %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(cluster.deep) %>% 
  summarise() %>% 
  st_convex_hull()

nasc.super.hauls.ns.deep <- nasc.nearshore %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(haul.deep) %>% 
  summarise() %>% 
  st_convex_hull()

if (save.figs) {
  nasc.cluster.plot.ns.deep <- base.map +
    # Plot nasc data
    geom_point(data = nasc.nearshore, aes(X, Y), size = 0.5, show.legend = FALSE) +
    # Plot convex hull around NASC clusters
    geom_sf(data = nasc.super.clusters.ns.deep, colour = 'black', alpha = 0.5, show.legend = FALSE) +
    scale_fill_discrete(name = "Cluster") +
    scale_colour_manual(name = "Cluster", values = c("Trawl" = "blue", "Purse seine" = "red")) +
    # Plot cluster midpoints
    geom_shadowtext(data = filter(clf.ns.deep, CPS.num == 0), 
                    aes(X, Y, label = cluster),
                    colour = 'gray20', bg.colour = "white", size = 2) +
    # Plot positive trawl cluster midpoints
    geom_shadowtext(data = filter(clf.ns.deep, CPS.num > 0, cluster %in% nasc.nearshore$cluster.deep),
                    aes(X, Y, label = cluster, colour = sample.type),
                    size = 2, bg.colour = "white", fontface = "bold") +
    # Plot panel label
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  nasc.haul.plot.ns.deep <- base.map +
    # Plot nasc data
    geom_point(data = nasc.nearshore, aes(X, Y), size = 0.5, show.legend = FALSE) +
    # Plot convex hull around NASC clusters
    geom_sf(data = nasc.super.hauls.ns.deep, colour = 'black', alpha = 0.5, show.legend = FALSE) +
    scale_fill_discrete(name = "Haul") +
    scale_colour_manual(name = "Haul", values = c("Trawl" = "blue", "Purse seine" = "red")) +
    # Plot cluster midpoints
    geom_shadowtext(data = filter(hlf.ns.deep, CPS.num == 0), 
                    aes(X, Y, label = haul),
                    colour = 'gray20', bg.colour = "white", size = 2) +
    # Plot positive trawl cluster midpoints
    geom_shadowtext(data = filter(hlf.ns.deep, CPS.num > 0, haul %in% nasc.nearshore$haul.deep),
                    aes(X, Y, label = haul, colour = sample.type),
                    size = 2, bg.colour = "white", fontface = "bold") +
    # Plot panel label
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  # save trawl plot
  ggsave(nasc.cluster.plot.ns.deep,
         filename = here("Figs/fig_nasc_cluster_map_ns_deep.png"),
         width = map.width, height = map.height)
  
  ggsave(nasc.haul.plot.ns.deep,
         filename = here("Figs/fig_nasc_haul_map_ns_deep.png"),
         width = map.width, height = map.height)
  
  save(nasc.cluster.plot.ns.deep, file = here("Output/nasc_cluster_plot_ns_deep.Rdata"))
}

# Map nearshore species proportions -------------------------------------------------------
if (use.seine.data) {
  # Combine pie chart data from trawls and seines
  cluster.pie.deep <- bind_rows(cluster.pie, set.pie.deep)
  
  haul.pie.deep <- set.pie.deep %>% 
    select(-cluster) %>% 
    bind_rows(haul.pie)
}

# Do deep data
cluster.pie.ns.deep <- cluster.pie.deep %>%
  filter(cluster %in% unique(nasc.nearshore$cluster.deep),
         sample.type %in% catch.source.ns)

cluster.pos.ns.deep <- filter(cluster.pie.ns.deep, AllCPS > 0) %>% 
  arrange(desc(X))

# Select only hauls assigned to nasc intervals and included in catch.source.ns
haul.pie.ns.deep <- haul.pie.deep %>%
  filter(haul %in% unique(nasc.nearshore$haul.deep),
         sample.type %in% catch.source.ns)

haul.pos.ns.deep <- filter(haul.pie.ns.deep, AllCPS > 0) %>% 
  arrange(desc(X))

if (nrow(cluster.pos.ns.deep) > 0) {
  cluster.pos.ns.deep <- cluster.pos.ns.deep %>% 
    replace(. == 0, 0.0000001) 
}

if (nrow(haul.pos.ns.deep) > 0) {
  haul.pos.ns.deep <- haul.pos.ns.deep %>% 
    replace(. == 0, 0.0000001) 
}

# Filter for empty trawls
cluster.zero.ns.deep <- filter(cluster.pie.ns.deep, AllCPS == 0)
haul.zero.ns.deep    <- filter(haul.pie.ns.deep, AllCPS == 0)

# Join NASC and cluster length frequency data frames by cluster ----------------
# Create temporary nearshore nasc object for joining
nasc.nearshore.tmp <- nasc.nearshore

if (cluster.source["NS"] == "cluster") {
  # nasc.nearshore <- nasc.nearshore.tmp %>% 
  #   left_join(select(clf.ns, -lat, -long, -X, -Y, -haul), by = c("cluster" = "cluster"))
  
  nasc.nearshore.deep <- nasc.nearshore.tmp %>% 
    left_join(select(clf.ns.deep, -lat, -long, -X, -Y, -haul), by = c("cluster" = "cluster"))
  
} else {
  # nasc.nearshore <- nasc.nearshore.tmp %>% 
  #   left_join(select(hlf.ns, -lat, -long, -X, -Y,-cluster), by = c("haul" = "haul"))
  
  nasc.nearshore.deep <- nasc.nearshore.tmp %>% 
    left_join(select(hlf.ns.deep, -lat, -long, -X, -Y,-cluster), by = c("haul" = "haul"))
}

# Save results
save(nasc.nearshore.deep, file = here("Output/cps_nasc_prop_ns_deep.Rdata"))

# Apportion nearshore backscatter ------------------------------------------
# If enforcing max cluster distance, set proportions for each species to zero
# where cluster.distance > max.cluster.dist
if (limit.cluster.dist["NS"]) {
  nasc.nearshore.deep$prop.anch[nasc.nearshore.deep$cluster.distance.deep > max.cluster.dist] <- 0
  nasc.nearshore.deep$prop.sar[nasc.nearshore.deep$cluster.distance.deep  > max.cluster.dist] <- 0
  nasc.nearshore.deep$prop.jack[nasc.nearshore.deep$cluster.distance.deep > max.cluster.dist] <- 0
  nasc.nearshore.deep$prop.mack[nasc.nearshore.deep$cluster.distance.deep > max.cluster.dist] <- 0
  nasc.nearshore.deep$prop.her[nasc.nearshore.deep$cluster.distance.deep  > max.cluster.dist] <- 0  
}

# For intervals with deep backscatter
nasc.prop.all.ns.deep <- nasc.nearshore.deep %>%
  mutate(`Engraulis mordax`      = cps.nasc.deep*prop.anch,
         `Sardinops sagax`       = cps.nasc.deep*prop.sar,
         `Trachurus symmetricus` = cps.nasc.deep*prop.jack,
         `Scomber japonicus`     = cps.nasc.deep*prop.mack,
         `Clupea pallasii`       = cps.nasc.deep*prop.her,
         `Etrumeus acuminatus`   = cps.nasc.deep*prop.rher) 

# Prepare nasc.prop.all for facet plotting
nasc.prop.spp.ns.deep <- nasc.prop.all.ns.deep %>% 
  select(X, Y, `Engraulis mordax`, `Sardinops sagax`, `Trachurus symmetricus`,
         `Scomber japonicus`, `Clupea pallasii`, `Etrumeus acuminatus`) %>% 
  gather(scientificName, nasc, -X, -Y)

if (save.figs) {
  map.prop.all.ns.deep <- base.map + 
    # Plot backscatter for all CPS nasc
    geom_point(data = nasc.prop.all.ns.deep, aes(X, Y, size = cps.nasc.deep), 
               colour = "gray20") +
    # Plot proportion of backscatter from each species present
    geom_point(data = filter(nasc.prop.spp.ns.deep, nasc > 0),
               aes(X, Y, size = nasc), colour = "red") +
    # Facet by species
    facet_wrap(~scientificName, nrow = 2) +
    theme(strip.background.x = element_blank(),
          strip.text.x       = element_text(face = "italic")) +
    coord_sf(crs = crs.proj, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"]))
  
  ggsave(here("Figs/fig_nasc_acoustic_proportions_ns_deep.png"), map.prop.all.ns.deep,
         width = map.width*3, height = map.height*2)
  
  # Save plot objects
  save(map.prop.all.ns.deep, file = here("Output/acoustic_proportion_maps_ns_deep.Rdata"))
  
} else {
  # Load plot objects
  load(here("Output/acoustic_proportion_maps_ns_deep.Rdata"))
}

# Deep density
nasc.nearshore.deep <- nasc.nearshore.deep %>% 
  mutate( 
    anch.dens = cps.nasc.deep*prop.anch / (4*pi*sigmawg.anch) / 1000,
    her.dens  = cps.nasc.deep*prop.her  / (4*pi*sigmawg.her)  / 1000,
    jack.dens = cps.nasc.deep*prop.jack / (4*pi*sigmawg.jack) / 1000,
    mack.dens = cps.nasc.deep*prop.mack / (4*pi*sigmawg.mack) / 1000,
    sar.dens  = cps.nasc.deep*prop.sar  / (4*pi*sigmawg.sar)  / 1000,
    rher.dens = cps.nasc.deep*prop.rher / (4*pi*sigmawg.rher) / 1000)

# Add deep density to shallow density
nasc.nearshore <- nasc.nearshore %>% 
  mutate(anch.dens = anch.dens + nasc.nearshore.deep$anch.dens,
         her.dens  = her.dens  + nasc.nearshore.deep$her.dens,
         jack.dens = jack.dens + nasc.nearshore.deep$jack.dens,
         mack.dens = mack.dens + nasc.nearshore.deep$mack.dens,
         sar.dens  = sar.dens  + nasc.nearshore.deep$sar.dens,
         rher.dens = rher.dens + nasc.nearshore.deep$rher.dens)

# Remove point estimates, if they exist
if(exists("point.estimates.ns.deep")) rm(point.estimates.ns.deep)
