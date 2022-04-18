# Get Carranza nav data from the meteorological station
nav.jcf <- st_read(here("Data/Nav/JCF/JCFINP2110meteorologica00_27.gpkg")) %>% 
  mutate(
    time = ymd_hms(paste(date, time)),
    leg = "Leg 5") %>% 
  select(time, lat = latitude, long = longitude,
         SST = temperature, SOG = sog, 
         wind_dir = wind_direction10min,
         wind_speed = wind_speed10min,
         leg) %>% 
  st_set_geometry(NULL)
  

# nav.jcf <- read_csv(here("Data/Nav/JCF/nav_vessel_JCF.csv")) %>% 
#   mutate(time = dmy_hms(paste(date, time)),
#          leg  = "Leg 5") %>% 
#   select(time, lat = latitude, long = longitude, leg)

# Convert nav to spatial
nav.jcf.sf <- st_as_sf(nav.jcf, coords = c("long","lat"), crs = crs.geog) 

# Cast nav to paths
nav.paths.jcf.sf <- nav.jcf.sf %>% 
  group_by(leg) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

# Add Carranza data to Lasker nav
nav.all <- bind_rows(nav, nav.jcf)

# Convert to sf 
nav.all.sf <- st_as_sf(nav.all, coords = c("long","lat"), crs = crs.geog) 

# Read station waypoint and transect shapefiles
wpts.jcf <- st_read(here("Data/Nav/JCF/estaciones_BC_planc.shp"))
transects.jcf <- st_read(here("Data/Nav/JCF/transectos_BC_sin_inter.shp"))
