# library(here)
# library(tidyverse)
# library(marmap)
# library(sf)
# 
# # Extract bathymetry along each transect ----------------------------------
# get.bathy <- FALSE
# 
# # Get bathymetry data across range of nav data (plus/minus one degree lat/long)
# if (get.bathy) {
#   noaa.bathy <- getNOAA.bathy(lon1 = min(transects$Longitude - 1),
#                               lon2 = max(transects$Longitude + 1),
#                               lat1 = max(transects$Latitude) + 1,
#                               lat2 = min(transects$Latitude) - 1,
#                               resolution = 1)
#   # Save bathy results
#   save(noaa.bathy, file = here("Data/GIS/bathy_data_1907RL.Rdata"))
# } else {
#   load(here("Data/GIS/bathy_data_1907RL.Rdata"))
# }

# wpts <- read_csv(here("Data/Nav/waypoints_1907RL_scb.csv"))
# 
# # extract transect waypoints
# transects <- wpts %>% 
#   mutate(group = paste(transect, region)) 

# Create a df for transect profiles
tx.prof <- data.frame()

# Extract bathy along each transect and combine
for (i in unique(transects$group)) {
  tx.tmp <- filter(transects, group == i)
  
  tmp <- get.transect(noaa.bathy, tx.tmp$Longitude[1], tx.tmp$Latitude[1],
                      tx.tmp$Longitude[nrow(tx.tmp)], tx.tmp$Latitude[nrow(tx.tmp)],
                      distance = TRUE) %>% 
    mutate(transect = unique(tx.tmp$Transect),
           region = unique(tx.tmp$Region)) %>% 
    filter(depth < 0)
  
  tx.prof <- bind_rows(tx.prof,tmp)
}

# Calculate the .99 quantile for each transect type
type.ecdf <- tx.prof %>% 
  group_by(region) %>% 
  summarise(depth.99 = quantile(ecdf(abs(depth)),.99))

# Plot ECDF by type, showing the .99 quantile
tx.ecdf.plot <- ggplot(tx.prof, aes(abs(depth))) + 
  stat_ecdf() +
  geom_vline(data = type.ecdf, aes(xintercept = depth.99), linetype = "dashed") +
  facet_wrap(~region, scales = "free") +
  ylab("ECDF") + xlab("Depth (m)") +
  theme_bw()

# Save plot
ggsave(tx.ecdf.plot, filename = here("Figs/fig_transect_depth_ecdf.png"),
       height = 6, width = 10)

transect.paths <- transects %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>% 
  group_by(Transect, Region) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(distance = round(as.numeric(st_length(.))/1852,1))

# Read bathy contours shapefile 
bathy <- st_read(here("Data/GIS/bathy_contours.shp")) %>% 
  st_transform(4326) %>% 
  rename(Depth = Contour) %>% 
  mutate(depth = as.factor(Depth))

# mapview::mapview(transect.paths) + 
#   mapview::mapview(bathy, zcol = "depth")

# # Same analysis, but for WA/OR nearshore transects only -------------------
# # Restrict ECDF to nearshore transects off OR/WA
# type.ecdf.WAOR <- tx.prof %>% 
#   filter(type == "Nearshore", between(transect, 133, 211)) %>% 
#   group_by(type) %>% 
#   summarise(depth.99 = quantile(ecdf(abs(depth)),.99))
# 
# # Plot ECDF by type, showing the .99 quantile
# tx.ecdf.WAOR.plot <- tx.prof %>% 
#   filter(type == "Nearshore", between(transect, 133, 211)) %>% 
#   ggplot(aes(abs(depth))) + 
#   stat_ecdf() +
#   geom_vline(data = type.ecdf.WAOR, aes(xintercept = depth.99), linetype = "dashed") +
#   ylab("ECDF") + xlab("Depth (m)")
# 
# # Save plot
# ggsave(tx.ecdf.WAOR.plot, filename = here("Figs/fig_transect_depth_ecdf_WAOR.png"))
