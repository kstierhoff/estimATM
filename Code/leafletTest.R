leaflet() %>% 
  # Enable tile caching
  enableTileCaching() %>% 
  # Add provider tiles; # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addProviderTiles(providers$Esri.OceanBasemap, 
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  addMarkers(data = nav.now, label = ~label) 
