# Load libraries
library(tidyverse)
library(here)
library(sf)
library(swfscMisc)

# Show figures?
plot.figs <- FALSE
save.figs <- TRUE

# Load core nasc
load(here("Output/nasc_final.Rdata"))

# Arrange nasc by longitude, to solve problems with multiple EV files
nasc <- nasc %>% 
  arrange(transect.name, long) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

if (plot.figs) {
  ggplot(nasc, aes(colour = vessel.name)) +
    geom_sf()
}

# Load nearshore nasc
load(here("Data/Backscatter/nasc_nearshore.Rdata"))

nasc.nearshore <- nasc.nearshore %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

if (plot.figs) {
  ggplot(nasc.nearshore, aes(colour = vessel.name)) +
    geom_sf() +
    facet_wrap(~vessel.name)
}

# Combine nasc data
nasc.combined <- rbind(select(nasc, vessel.name, transect.name, depth),
                       select(nasc.nearshore, vessel.name, transect.name, depth)) 

# Load shoreline data
shoreline <- read_csv(here("Data/GIS/shoreline_points.csv")) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

if (plot.figs) {
  ggplot(shoreline) + geom_sf()
}

# Get most shoreward point for each vessel and transect ------------------------------
# Combine all NASC data
nasc.all <- select(nasc, transect.name, vessel.name, depth) %>% 
  rbind(select(nasc.nearshore, transect.name, vessel.name, depth))

nearest.df.final <- data.frame()

# Process each vessel
for (v in unique(nasc.all$vessel.name)) {
  # Select first points from each transect
  nasc.first <- nasc.all %>% 
    filter(vessel.name == v) %>% 
    group_by(transect.name) %>%
    slice(1) 
  
  # Get nearest shoreline feature
  if (v == "RL") {
    nearest.df1 <- shoreline[sf::st_nearest_feature(nasc.first, filter(shoreline, name == "mainland")), ] %>% 
      mutate(
        long = as.data.frame(st_coordinates(.))$X,
        lat = as.data.frame(st_coordinates(.))$Y) %>% 
      st_set_geometry(NULL)
  } else {
    nearest.df1 <- shoreline[sf::st_nearest_feature(nasc.first, shoreline), ] %>% 
      mutate(
        long = as.data.frame(st_coordinates(.))$X,
        lat = as.data.frame(st_coordinates(.))$Y) %>% 
      st_set_geometry(NULL)
  }
 
  
  # Convert back to df
  nasc.first <- nasc.first %>% 
    ungroup() %>% 
    mutate(
      long = as.data.frame(st_coordinates(.))$X,
      lat = as.data.frame(st_coordinates(.))$Y) %>% 
    st_set_geometry(NULL)  
  
  # Calculate distance to nearest point on shore
  tmp.df1 <- data.frame()
  
  # Calculate distance between each NASC interval and all trawl clusters
  for (i in 1:nrow(nasc.first)) {
    temp.distance <- swfscMisc::distance(nasc.first$lat[i], nasc.first$long[i], 
                                         nearest.df1$lat[i], nearest.df1$long[i], 
                                         units = "nm")
    
    tmp.df1 <- bind_rows(tmp.df1,
                         data.frame(
                           vessel.name = nasc.first$vessel.name[i],
                           transect.name = nasc.first$transect.name[i],
                           dist.to.shore = temp.distance,
                           lat = nasc.first$lat[i],
                           long = nasc.first$long[i],
                           depth = nasc.first$depth[i]
                         ))
  }
  
  # Process last points from each transect
  nasc.last <- nasc.all %>% 
    filter(vessel.name == v) %>% 
    group_by(transect.name) %>%
    slice(n()) 
  
  if (v == "RL") {
    # Get nearest shoreline feature
    nearest.df2 <- shoreline[sf::st_nearest_feature(nasc.last, filter(shoreline, name == "mainland")), ] %>% 
      mutate(
        long = as.data.frame(st_coordinates(.))$X,
        lat = as.data.frame(st_coordinates(.))$Y) %>% 
      st_set_geometry(NULL)  
  } else {
    # Get nearest shoreline feature
    nearest.df2 <- shoreline[sf::st_nearest_feature(nasc.last, shoreline), ] %>% 
      mutate(
        long = as.data.frame(st_coordinates(.))$X,
        lat = as.data.frame(st_coordinates(.))$Y) %>% 
      st_set_geometry(NULL) 
  }
  
  # Convert to df
  nasc.last <- nasc.last %>% 
    ungroup() %>% 
    mutate(
      long = as.data.frame(st_coordinates(.))$X,
      lat = as.data.frame(st_coordinates(.))$Y) %>% 
    st_set_geometry(NULL)
  
  # Calculate distance to nearest point on shore
  tmp.df2 <- data.frame()
  
  # Calculate distance between each NASC interval and all trawl clusters
  for (i in 1:nrow(nasc.last)) {
    temp.distance <- swfscMisc::distance(nasc.last$lat[i], nasc.last$long[i], 
                                         nearest.df2$lat[i], nearest.df2$long[i], 
                                         units = "nm")
    
    tmp.df2 <- bind_rows(tmp.df2,
                         data.frame(
                           vessel.name = nasc.last$vessel.name[i],
                           transect.name = nasc.last$transect.name[i],
                           dist.to.shore = temp.distance,
                           lat = nasc.last$lat[i],
                           long = nasc.last$long[i],
                           depth = nasc.last$depth[i]
                         ))
  }
  
  # Combine first and last points, and select closest to shore
  nearest.df.tmp <- tmp.df1 %>% 
    bind_rows(tmp.df2) %>% 
    arrange(transect.name, dist.to.shore) %>% 
    group_by(transect.name) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(transect.name, vessel.name, long, lat, dist.to.shore, depth)
  
  # Combine all vessels
  nearest.df.final <- bind_rows(nearest.df.final, nearest.df.tmp)
}

# Convert to spatial, for plotting
nearest.sf.final <- nearest.df.final %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

dist.hist <- ggplot(nearest.df.final, aes(dist.to.shore)) +
  geom_histogram() + 
  facet_wrap(~vessel.name) +
  xlab("Distance to shore (nmi)") + ylab("Frequency") +
  theme_bw() 

# Plot and save histograms
if (plot.figs) {
 dist.hist
}

if (save.figs) {
  ggsave(dist.hist, 
         filename = here("Figs/fig_distance_to_shore_hist.png"))
}

if (plot.figs) {
  ggplot(nearest.df.final, aes(long, lat, colour = vessel.name)) +
    geom_path() + 
    geom_point() +
    coord_map() + 
    theme_bw()  
}

# Get map data
land <- map_data("usa") 
channel.islands <- st_read(here("Data/GIS/channel_islands.shp"))

# Plot data from each vessel, compared to the primary survey vessel
for (v in unique(nearest.df.final$vessel.name)[!str_detect(unique(nearest.df.final$vessel.name), "RL")]) {
  map.bounds <- nearest.sf.final %>%
    filter(vessel.name == v) %>% 
    st_bbox()
  
  dist.v <- ggplot() +
    geom_polygon(data = land, aes(x = long, y = lat, group = group), fill = "gray50") + 
    geom_sf(data = channel.islands, fill = "gray50") + 
    geom_path(data = filter(nearest.df.final, vessel.name == v), 
              aes(long, lat)) + 
    geom_point(data = filter(nearest.df.final, vessel.name == v), 
               aes(long, lat), fill = "blue", shape = 21) +
    geom_path(data = filter(nearest.df.final, vessel.name == "RL"), 
              aes(long, lat)) + 
    geom_point(data = filter(nearest.df.final, vessel.name == "RL"), 
               aes(long, lat), fill = "yellow", shape = 21) +
    coord_sf(crs = 4326, 
             xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
             ylim = c(map.bounds["ymin"], map.bounds["ymax"])) +
    theme_bw()
  
  ggsave(dist.v, filename = here("Figs", paste0("fig_distance_to_shore_", v, ".png")))
}
