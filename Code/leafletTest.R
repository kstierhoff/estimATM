leaflet() %>% 
  # Enable tile caching
  enableTileCaching() %>% 
  # Add provider tiles; # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addProviderTiles(providers$Esri.OceanBasemap, 
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  # addMarkers(data = nav.now, label = ~label) %>% 
  # addCircleMarkers(data = cufes.ofe.sf,
  #                  radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
  #                  fillOpacity = 0.75, fillColor =  pac.mack.color, label = ~label,
  #                  popup = ~popup, group = "CUFES Egg Density-Other") %>% 
  addMarkers(data = big.nasc.sf, label = ~label,
             popup = ~popup) %>% 
  addCircleMarkers(data = filter(nasc.cps.sf, NASC < 200), 
                   radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  "#000414", label = ~htmlEscape(label),
                   group = "Backscatter-CPS (Small)") %>%
  addCircleMarkers(data = nasc.krill.sf, radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  ~nascPal(bin.level), label = ~htmlEscape(label),
                   group = "Backscatter-Krill") %>% 
  addCircleMarkers(data = filter(nasc.cps.sf, NASC >= 200), 
                   radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  ~nascPal(bin.level), label = ~htmlEscape(label),
                   group = "Backscatter-CPS")
