pacman::p_load(tidyverse,sf,sp,leaflet,here,raster,atm,rnaturalearth)

# Made a new data frame with lat, long, and the value
chl <- read_csv(here("Data/Habitat/sardine_habitat_chla.csv"),
                col_names = c("datetime", "lat", "long", "chl")) %>% 
  filter(chl < 30)
sst <- read_csv(here("Data/Habitat/sardine_habitat_sst.csv"),
                col_names = c("datetime", "lat", "long", "sst")) 

chl[atm:::is.nan.df(chl)] <- NA
sst[atm:::is.nan.df(sst)] <- NA

sst$sst[sst$sst > 23] <- NA

# #Did this....
# chl_sp = SpatialPixelsDataFrame(chl[ ,c('long', 'lat')], data = chl)
# crs(chl_sp) = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# 
# chl_r = raster(chl_sp)
# 
# # Set up the colors
# val_chl = as.numeric(c(0:max(chl$chl, na.rm = TRUE)))
# pal_chl = colorNumeric(c("yellow", "orange", "red"), val_chl,
#                    na.color = "transparent")
# 
# # Made the map
# leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
#   addRasterImage(chl_r, colors = pal_chl, opacity = 0.5) %>%
#   addLegend(pal = pal_chl, values = val_chl, title = "Chl a")


# Get map features --------------------------
# Get state data
states     <- ne_states(country = 'United States of America', returnclass = 'sf')
hab.states <- filter(states, name %in% c("California","Oregon","Washington",
                                         "Nevada","Idaho"))
# Get countries
countries <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(subregion %in% c("Northern America","Central America"))

# Read bathy contours shapefile 
bathy <- st_read(here("Data/GIS/bathy_contours.shp")) %>% 
  st_transform(4326) %>% 
  rename(Depth = Contour)

library(cmocean)

# myColor <- cmocean("algae")
# 
# z <- ggplot(alldata,aes(x = lon, y = lat)) + 
#   geom_point(aes(colour = mean, shape = factor(missing)), size = 2.) + 
#   scale_shape_manual(values = c(19, 1))
# z + geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") + 
#   theme_bw() + 
#   scale_fill_cmocean() + 

map.bounds <- chl %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  st_bbox()

# Create map
chl.map <- ggplot() + 
  # facet_wrap(~date) +
  geom_raster(data = chl, aes(long, lat, fill = chl)) +
  scale_fill_cmocean(name = "algae", limits = c(0, 30)) +
  # scale_fill_gradientn(colors = rev(rainbow(10)))
  geom_sf(data = countries, fill = "black", color = "gray30") +
  geom_sf(data = states, fill = "black", colour = "gray30") +
  # Plot bathymetry contours
  # geom_sf(data = bathy, colour = "gray90") +
  # Format axes and titles
  xlab("Longitude") + ylab("Latitude") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = 4326, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
  
  theme_bw() 

sst.map <- ggplot() + 
  # facet_wrap(~date) +
  geom_raster(data = sst, aes(long, lat, fill = sst)) +
  scale_fill_cmocean(name = "thermal") +
  geom_sf(data = countries, fill = "black", color = "gray30") +
  geom_sf(data = states, fill = "black", colour = "gray30") +
  # Plot bathymetry contours
  # geom_sf(data = bathy, colour = "gray90") +
  # Format axes and titles
  xlab("Longitude") + ylab("Latitude") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(crs = 4326, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
  
  theme_bw() 


# Save map
ggsave(hab.map, 
       filename = here("Figs/fig_habitat_map.png"),
       width = 8, height = 10)
