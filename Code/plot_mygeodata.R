library(fs)
library(here)
library(plotKML)
library(sf)
library(tidyverse)

# Create directories
dir_create(here("Data/Nav/mygeodata"))
dir_create(here("Data/Nav/mygeodata/route_output"))

# Read waypoints
wpts <- read_csv(here("Data/Nav/mygeodata/route_points.csv"))

# View(wpts)

# Plot raw waypoints
ggplot(wpts, aes(X, Y, group = factor(route_fid))) +
  geom_path() +
  geom_point() + 
  coord_map() +
  theme_bw()

# Renumber transects using the latitude of the easternmost transect waypoint
transect.df <- wpts %>% 
  group_by(route_fid) %>% 
  slice(which.max(Y)) %>% 
  ungroup() %>% 
  arrange(Y) %>% 
  mutate(transect = seq_along(Y)) %>% 
  select(route_fid, transect)

# Join new transect numbers
wpts2 <- wpts %>% 
  select(route_fid, route_point_id, name, lat = Y, long = X) %>% 
  left_join(transect.df) %>% 
  arrange(transect, route_point_id) %>% 
  mutate(
    transect = sprintf("%03d", transect),
    waypoint = paste0(transect, substr(as.character(route_point_id*0.001 + 0.1), 2, 5), "C"),
    name = case_when(
      str_detect(name, "Pairovet") ~ paste0(waypoint, " Pairovet"),
      TRUE ~ name),
    order = seq_along(route_fid))

# View(wpts2)  

# Plot transects with new transect numbers
ggplot(wpts2, aes(long, lat, colour = transect)) + 
  geom_point() +
  geom_line() + 
  coord_map() + 
  theme_bw()

write_csv(select(wpts2, id = waypoint, lat, long), 
          here("Data/Nav/mygeodata/route_points_temp.csv"))

# Write waypoints from individual files to multiple CSV to create single routes
for (i in unique(wpts2$transect)) {
  wpts.sub <- wpts2 %>% 
    filter(transect == i) %>% 
    select(id = name, lat, long) 
  
  if (nrow(wpts.sub) > 0) {
    write_csv(wpts.sub, here("Data/Nav/mygeodata/route_output", paste0(i, "C.csv")))
  }
}

# Subset waypoints to nav poitns and pairovet stations
nav.wpts <- wpts2 %>% 
  filter(!str_detect(name, "Pairovet"))

net.wpts <- wpts2 %>% 
  filter(str_detect(name, "Pairovet"))

net.wpts2 <- data.frame()

for (j in unique(net.wpts$transect)) {
  net.temp <- net.wpts %>% 
    filter(transect == j) %>% 
    mutate(rank = rank(order),
           type = case_when(
             rank%%2 == 1 ~ "Compulsory",
             TRUE ~ "Adaptive"),
           name = case_when(
             type == "Adaptive" ~ str_replace(name, "C", "A"),
             TRUE ~ name))
  
  net.wpts2 <- bind_rows(net.wpts2, net.temp)
}

# Create land object for plotting
usa <- map_data("usa") 

# Convert to sf
wpts.sf <- wpts2 %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) 

# Get map boundaries for plotting
map.bounds <- wpts.sf %>%
  st_bbox()  

# Plot waypoints
ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) + 
  geom_point(data = nav.wpts, aes(long, lat, group = transect)) +
  geom_path(data = nav.wpts, aes(long, lat, group = transect)) +
  geom_point(data = net.wpts2, aes(long, lat, fill = type), shape = 21) +
  coord_sf(crs = 4326, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) + 
  theme_bw()

# Combine nav waypoints and pairovet stations
wpts.final <- nav.wpts %>% 
  bind_rows(net.wpts2) %>% 
  arrange(order)

# Plot final points
my.geodata.plot <- ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) + 
  geom_point(data = filter(wpts.final, is.na(type)), aes(long, lat, group = transect)) +
  geom_path(data = filter(wpts.final, !is.na(type)), aes(long, lat, group = transect)) +
  geom_point(data = filter(wpts.final, !is.na(type)), aes(long, lat, fill = type), shape = 21) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(crs = 4326, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]), 
           ylim = c(map.bounds["ymin"], map.bounds["ymax"])) + 
  theme_bw()

# Save map
ggsave(my.geodata.plot, filename = here("Figs/fig_plot_mygeodata.png"))

# Write final points to CSV
write_csv(select(wpts.final, id = name, lat, long), 
          here("Data/Nav/mygeodata/route_points_final.csv"))



# Old code ----------------------------------------------------------------

# # route <- readGPX(here::here("route_revised.gpx"))
# # route <- readGPX(here::here("route_revised2.gpx"))
# route <- readGPX(here::here("route_revised3.gpx"))
#  
# wpts3 <- route$waypoints 
# 
# write_csv(wpts3, here::here("route_points_final.csv"))
# 
# # %>%
# #   mutate(name = str_replace(name, "P", "C")) %>%
# #   mutate(name = str_replace(name, "11C", "110C")) %>%
# #   arrange(name)
# 
# 
# # %>%
# #          wpt = as.numeric(str_replace(name, "C", "")) %>%
# #   arrange(wpt)
# 
# ggplot(wpts3, aes(lon, lat)) + geom_path() + coord_map()
# 
# route <- readGPX(here::here("route_revised_final.gpx"))
# 
# wpts4 <- route$waypoints %>% 
#   arrange(name) %>% 
#   mutate(transect = substr(name, 1,3)) %>% 
#   select(name, lat, long = lon, transect)
# 
# ggplot(wpts4, aes(long, lat, color = transect)) + 
#   geom_path() + 
#   geom_point() +
#   coord_map()
# 
# # Write waypoints from individual files to multiple CSV to create single routes
# for (i in unique(wpts4$transect)) {
#   wpts.sub <- wpts4 %>% 
#     filter(transect == i) %>% 
#     select(id = name, lat, long) 
#   
#   if (nrow(wpts.sub) > 0) {
#     write_csv(wpts.sub, here::here("route_output2", paste0(i, "C.csv"))) 
#   }
# }
# 
# write_csv(wpts4, here::here("route_table.csv"))
# 
# final.wpts <- read_csv(here::here("route_table.csv")) %>% 
#   mutate(name = case_when(
#     type == "Pairovet" ~ paste(type, name),
#     TRUE ~ name
#   ),
#   transect = sprintf("%03d", transect))
# 
# write_csv(final.wpts, here::here("route_output_final.csv"))
# 
# 
# # Write waypoints from individual files to multiple CSV to create single routes
# for (i in unique(final.wpts$transect)) {
#   wpts.sub <- final.wpts %>% 
#     filter(transect == i) %>% 
#     select(id = name, lat, long) 
#   
#   if (nrow(wpts.sub) > 0) {
#     write_csv(wpts.sub, here::here("route_output_final", paste0(i, "C.csv"))) 
#   }
# }
# 
# ggplot(final.wpts, aes(long, lat, fill = type)) + 
#   geom_path(aes(group = transect)) +
#   geom_point(shape = 21) + 
#   coord_map() + 
#   theme_bw()
