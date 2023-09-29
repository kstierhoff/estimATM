library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(here)
library(htmltools)

# Import waypoints
# Read transect waypoints
wpts <- read_csv(here("Data/Nav", "waypoints_2307SH.csv"))

# Convert planned transects to sf; CRS = crs.geog
wpts.sf <- wpts %>% 
  # filter(Type %in% wpt.types) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>% 
  mutate(
    label    = paste("Transect", Transect),
    popup    = paste('<b>Transect:</b>', Transect, Type)
  )

transects.sf <- wpts.sf %>% 
  group_by(Type, Transect, Region) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(
    distance = round(as.numeric(st_length(.))/1852,1),
    label    = paste("Transect", Transect),
    popup    = paste('<b>Transect:</b>', Transect, Type, '<br/>',
                     'Distance:', distance, 'nmi<br/>')
  )

# Import logs
marks <- read_excel("Data/Nav/marks_2307SH.xlsx") %>% 
  rename(lat = Lat_s, lon = Lon_s) %>% 
  dplyr::filter(lat != 999, lon != 999) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  mutate(mark_type = case_when(
    str_detect(Region_name, "^ST") ~ "start",
    str_detect(Region_name, "^ET") ~ "end",
    str_detect(Region_name, "^BT") ~ "break",
    str_detect(Region_name, "^RT") ~ "resume",
    TRUE ~ "other"
  )) %>% 
  mutate(
    label    = Region_name,
    popup    = paste('<b>Start:</b>', Date_s, Time_s, '<br/>',
                     'Notes:', Region_notes, '<br/>')
  )

# Create color pallette for planned transects
wpt.types            <- c(break = "break", end = "end",
                          resume = "resume", start = "start",)
wpt.colors           <- c(break = "yellow", end = "red",
                          resume = "blue", start = "green") 
txPal    <- colorFactor(wpt.colors, wpt.types)
  
useCachedTile <- FALSE
useCrossOrigin <- FALSE

leaflet() %>% 
  # Enable tile caching
  enableTileCaching() %>%
  # Add provider tiles; 
  # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addProviderTiles("Esri.OceanBasemap", 
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  addProviderTiles("Esri.WorldStreetMap", group = "Esri.WorldStreetMap",
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery",
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap.Mapnik",
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  # Add transects
  addPolylines(data = transects.sf, color = ~txPal(Type),
               weight = 3, opacity = 0.5,
               label = ~label,
               popup = ~popup,
               group = "Transects") %>% 
  addCircleMarkers(data = marks,
                   radius = 3, color = "#000414", stroke = F, opacity = 0.5,
                   fillOpacity = 0.5, fillColor =  ~txPal(mark_type), 
                   label = ~htmlEscape(Region_name),
                   popup = ~htmlEscape(popup),
                   group = "Marks")
