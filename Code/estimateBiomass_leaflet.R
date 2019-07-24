# Create leaflet map
leaflet() %>% 
  # Enable tile caching
  enableTileCaching() %>% 
  # Add provider tiles; # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addProviderTiles(providers$Esri.OceanBasemap, 
                   options = tileOptions(useCache = useCachedTile,
                                         crossOrigin = useCrossOrigin)) %>%
  # Add EEZs
  addPolylines(data = eez_usa, color = "#000414", weight = 3, 
               label = "EEZ-U.S.", group = "Exclusive Economic Zones") %>% 
  addPolylines(data = eez_can, color = "#000414", weight = 3, 
               label = "EEZ-Canada", group = "Exclusive Economic Zones") %>% 
  addPolylines(data = eez_mex, color = "#000414", weight = 3, 
               label = "EEZ-Mexico", group = "Exclusive Economic Zones") %>% 
  # Add bathymetry contours
  addPolylines(data = bathy, color = "white", weight = 2, 
               label = ~paste(Depth, "m"), group = "Bathymetry Contours") %>% 
  # Add State waters
  addPolygons(data = or_waters, weight = 2, fillColor = "transparent", 
              opacity = 0.75,
              label = ~htmlEscape("OR State Waters"),
              group = "State Waters") %>%
  addPolygons(data = ca_waters, weight = 2, fillColor = "transparent",
              opacity = 0.75,
              label = ~htmlEscape("CA State Waters"),
              group = "State Waters") %>%
  # Add CA MPAs
  addPolygons(data = ca_mpas, color = "#000414", weight = 2, fillColor = ~mpaPal(Type),
              fillOpacity = 0.3, label = ~htmlEscape(MPA), group = "MPAs") %>%
  # Add OR MPAs
  addPolygons(data = or_mpas, color = "#000414", weight = 2, fillColor =  ~mpaPal(Label),
              fillOpacity = 0.3, label = ~htmlEscape(Name), group = "MPAs") %>%
  # Add core planned transects
  addPolylines(data = filter(transects.sf, Type %in% c("Compulsory", "Adaptive")),
               color = ~txPal(Type), weight = 3, opacity = 0.5,
               label = ~htmlEscape(paste(Type, Transect)),
               popup = ~popup,
               group = "Planned Transects (Core)") %>%
  addCircleMarkers(data = filter(wpts.sf, Type %in% c("Compulsory", "Adaptive")),
                   radius = 3, color = "#000414", stroke = F, opacity = 0.5,
                   fillOpacity = 0.5, fillColor =  ~txPal(Type), 
                   label = ~htmlEscape(paste(Type, Waypoint)),
                   popup = ~popup,
                   group = "Planned Transects (Core)") %>%
  # Add ancillary planned transects 
  addPolylines(data = filter(transects.sf, !Type %in% c("Compulsory", "Adaptive")),
               color = ~txPal(Type), weight = 3, opacity = 0.5,
               label = ~htmlEscape(paste(Type, Transect)), 
               popup = ~popup,
               group = "Planned Transects (Ancillary)") %>%
  addCircleMarkers(data = filter(wpts.sf, !Type %in% c("Compulsory", "Adaptive")),
                   radius = 3, color = "#000414", stroke = F, opacity = 0.5,
                   fillOpacity = 0.5, fillColor =  ~txPal(Type), 
                   label = ~htmlEscape(paste(Type, Waypoint)),
                   popup = ~popup,
                   group = "Planned Transects (Ancillary)") %>%
  # Add nav data
  addPolylines(data = nav_sf, color = "#000414", weight = 1, 
               label = ~leg, group = "Vessel Track") %>%
  # addPolylines(data = nav.sd.sf, color = c("#000414"), weight = 3, 
  #              label = ~paste("Saildrone", saildrone), group = "Saildrone Tracks") %>%
  # Add CUFES data
  addCircleMarkers(data = filter(cufes.sf, Species != "SquidEggs"),
                   radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  ~cufesPal(Species), label = ~label,
                   popup = ~popup, group = "CUFES Egg Density") %>%
  addCircleMarkers(data = cufes.squid.sf,
                   radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  "#FFFFFF", label = ~label,
                   popup = ~popup, group = "CUFES Egg Density-Squid") %>% 
  addCircleMarkers(data = cufes.ofe.sf,
                   radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  pac.mack.color, label = ~label,
                   popup = ~popup, group = "CUFES Egg Density-Other") %>% 
  addCircleMarkers(data = cufes.neg.sf, radius = ~bin.level*2, color = "#000414", stroke = FALSE, weight = 1,
                   fillOpacity = 0.50, fillColor =  "#000414", label = ~htmlEscape(SampleNumber),
                   group = "CUFES (Negative)") %>%
  # Add backscatter data
  addCircleMarkers(data = filter(nasc.sf, NASC < 200), 
                   radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  "#000414", 
                   label = ~label, popup = ~popup, 
                   group = "Backscatter-CPS (Small)") %>%
  # addCircleMarkers(data = nasc.krill.sf, radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
  #                  fillOpacity = 0.75, fillColor =  ~nascPal(bin.level), 
  #                  label = ~label, popup = ~popup,
  #                  group = "Backscatter-Krill") %>% 
  addCircleMarkers(data = filter(nasc.sf, NASC >= 200), 
                   radius = ~bin.level*2, color = "#000414", stroke = TRUE, weight = 1,
                   fillOpacity = 0.75, fillColor =  ~nascPal(bin.level), 
                   label = ~label, popup = ~popup,
                   group = "Backscatter-CPS") %>%
  # Add trawl paths 
  addPolylines(data = haul.paths, color = c("#000000"), weight = 5, opacity = 0.8, 
               popup = ~popup, label = ~label, 
               group = "Trawls") %>% 
  # Add trawl catch
  addCircleMarkers(data = cluster.catch.sf, radius = 5, color = "#000000", stroke = TRUE, weight = 2,
                   opacity = 0.8, fillOpacity = 1, fillColor =  "white",
                   popup = ~popup, label = ~label,
                   group =  "Trawls") %>%
  addMarkers(data = nav.now, label = ~label, group = "Vessel Positions") %>% 
  # addMinicharts(catch.pie$X, catch.pie$Y,
  #               type = "pie",
  #               chartdata = catch.pie[, c("Anchovy", "JackMack", "Jacksmelt", "PacHerring",
  #                                         "PacMack", "Sardine")],
  #               colorPalette = trawlPal,
  #               width = 30, transitionTime = 0) %>% 
  # Add backscatter outliers
  addMarkers(data = big.nasc.sf, label = ~label, popup = ~popup,
             group = "Backscatter (Large)") %>%
  # Add legends
  addLegend("bottomleft", colors = nasc.colors.all, 
            values = sort(unique(nasc.cps.sf$bin.level)),
            labels = nasc.labels.all, 
            title = "CPS Backscatter<br/> (s<sub>A</sub>; m<sup>2</sup> nmi<sup>-2</sup>)", 
            opacity = 1, group = "Backscatter-CPS") %>% 
  addLegend("bottomleft", colors = nasc.colors.all, 
            values = ~sort(unique(nasc.krill.sf$bin.level)),
            labels = nasc.labels.all, 
            title = "Krill Backscatter<br/> (s<sub>A</sub>; m<sup>2</sup> nmi<sup>-2</sup>)", 
            opacity = 1, group = "Backscatter-Krill") %>% 
  addLegend("bottomleft", colors = cufes.colors, 
            values = ~sort(unique(cufes.sf$Species)), 
            labels = cufes.spp.labels, 
            title = "CUFES Egg Density <br/> (eggs m<sup>-3</sup>)", 
            opacity = 1, group = "CUFES Egg Density") %>% 
  # Add scale bar
  addScaleBar(position = "bottomright") %>%
  # Add map coordinates
  addMouseCoordinates() %>% 
  # Add measurement tool
  addMeasure(primaryLengthUnit = "miles", secondaryLengthUnit = "km",
             primaryAreaUnit = "sqmiles", secondaryAreaUnit = "sqmeters",
             position = "topleft") %>% 
  # Add layer controls
  addLayersControl(
    overlayGroups = c("MPAs", "State Waters", "Exclusive Economic Zones", 
                      "Bathymetry Contours",
                      "Planned Transects (Core)", "Planned Transects (Ancillary)",
                      "Vessel Track", "Vessel Positions", "Trawls",
                      "Backscatter-CPS", "Backscatter-CPS (Small)",
                      "Backscatter-Krill", "Backscatter (Large)",
                      "CUFES Egg Density", "CUFES Egg Density-Other",
                      "CUFES Egg Density-Squid", "CUFES (Negative)"),
    options = layersControlOptions(collapsed = F)) %>%  
  hideGroup(c("Backscatter (Large)", "Backscatter-Krill", "CUFES (Negative)", 
              "CUFES Egg Density-Squid", "Vessel Positions", "Saildrone Tracks", 
              "Planned Transects (Ancillary)")) %>% 
  fitBounds(imap.bounds$range.lon[1], imap.bounds$range.lat[1],
            imap.bounds$range.lon[2], imap.bounds$range.lat[2])
