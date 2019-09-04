leaflet() %>% 
  # Enable tile caching
  enableTileCaching() %>% 
  # Add provider tiles; # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addProviderTiles(providers$Esri.OceanBasemap, 
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  # addPolygons(data = filter(strata.primary.sub, scientificName == "Engraulis mordax"), 
  #             weight = 2, fillColor =  ~cpsPal(scientificName), color = "#000414", 
  #             group = "Stratum (Anchovy)") %>% 
  # addCircleMarkers(data = filter(nasc.density.sf, scientificName == "Engraulis mordax"),
  #                  radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
  #                  fillOpacity = 0.75, fillColor =  ~nascPal(bin.level),
  #                  group = "Backscatter-CPS") 
  addPolygons(data = nasc.super.clusters, 
              weight = 2, fillColor =  "transparent", color = "#000414", 
              label = ~label, popup = ~popup,
              group = "Stratum (Anchovy)") %>% 
  addCircleMarkers(data = cluster.catch.sf, radius = 5, color = "#000000", stroke = TRUE, weight = 2,
                   opacity = 0.8, fillOpacity = 1, fillColor =  "white",
                   popup = ~popup, label = ~label,
                   group =  "Trawls")

# addCircleMarkers(data = nasc.plot, 
#                  radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
#                  fillOpacity = 0.75, fillColor =  ~nascPal(bin.level), 
  #                  label = ~label, popup = ~popup,
  #                group = "Backscatter-CPS") %>%
  # addLegend("bottomleft", colors = nasc.colors.all,
  #           values = sort(unique(nasc.plot$bin.level)),
  #           labels = nasc.labels.all,
  #           title = "CPS Backscatter<br/> (s<sub>A</sub>; m<sup>2</sup> nmi<sup>-2</sup>)",
  #           opacity = 1, group = "Backscatter-CPS")
  # addMarkers(data = nav.now, label = ~label) %>% 
  # addCircleMarkers(data = cufes.ofe.sf,
  #                  radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
  #                  fillOpacity = 0.75, fillColor =  pac.mack.color, label = ~label,
  #                  popup = ~popup, group = "CUFES Egg Density-Other") %>% 
  # addMarkers(data = big.nasc.sf, label = ~label,
  #            popup = ~popup) %>% 
  # addCircleMarkers(data = filter(nasc.cps.sf, NASC < 200), 
  #                  radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
  #                  fillOpacity = 0.75, fillColor =  "#000414", label = ~htmlEscape(label),
  #                  group = "Backscatter-CPS (Small)") %>%
  # addCircleMarkers(data = nasc.krill.sf, radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
  #                  fillOpacity = 0.75, fillColor =  ~nascPal(bin.level), label = ~htmlEscape(label),
  #                  group = "Backscatter-Krill") %>% 
  # addCircleMarkers(data = filter(nasc.cps.sf, NASC >= 200), 
  #                  radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
  #                  fillOpacity = 0.75, fillColor =  ~nascPal(bin.level), label = ~htmlEscape(label),
  #                  group = "Backscatter-CPS")
