# Read GPX file
route <- tmaptools::read_GPX(here::here("Data/Nav", "RL-track.gpx"))

# Create data frame of waypoints
fixes <- route$track_points %>% 
  atm::project_sf(crs = 4326) %>% 
  select(lon = X, lat = Y, name)
