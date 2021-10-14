nasc.dist.sf <- nasc %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

super.clusters.sf <- super.clusters %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

# Find nearest cluster ----------------------------
nearest.cluster <- st_nearest_feature(nasc.dist.sf, super.clusters.sf)

# Expand clf to match nasc ------------------------
clf.sp <- super.clusters.sf[nearest.cluster, ] %>% 
  select(geometry) %>% 
  as_Spatial()


# Make nasc sp
# Removing the other data (i.e., only retaining the geometry) decreases the size of nasc from 50 to 1 MB
# I'm wondering if that may speed things along
nasc.sp <- nasc.dist.sf %>% 
  select(geometry) %>% 
  as_Spatial()

# Compute distances with geosphere
# Must be done in WGS84 projection
nasc.dist.sf$distance_m <- geosphere::distGeo(nasc.sp, clf.sp)


nasc2 <- nasc.dist.sf  %>% 
  mutate(cluster=super.clusters.sf$cluster[nearest.cluster],
         cluster.distance=distance_m* 0.000539957,
         long = as.data.frame(st_coordinates(.))$X,
         lat = as.data.frame(st_coordinates(.))$Y) %>% 
  select(-distance_m)

nasc.polygons <- nasc.dist.sf %>% 
  group_by(cluster) %>% 
  summarise() %>% 
  st_convex_hull()

nasc2 <- as.data.frame(nasc2)

#assign a cluster to each interval
nasc2 <- nasc2 %>% 
  left_join(select(clf, -lat, -long, -X, -Y), by=c("cluster"="cluster"))
