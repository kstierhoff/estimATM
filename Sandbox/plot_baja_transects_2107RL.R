pacman::p_load(tidyverse, here, sf, mapview)

# tx.mex.c <- st_read(here("Data/GIS/Mexico/transectos_BC_planc.shp"))
# tx.mex.a <- st_read(here("Data/GIS/Mexico/transectos_BC_sin_inter.shp"))
# # %>%   mutate(transect = seq_along(Id))
# wpt.mex.c <- st_read(here("Data/GIS/Mexico/estaciones_BC_planc.shp"))
# 
# mapview(tx.mex.a, zcol = "tx_SWFSC")
# mapview(tx.mex.a, zcol = "Type")
# 
# st_write(tx.mex.a, here("Data/GIS/transectos_BC_SWFSC_INAPESCA.shp"))



# Survey plan info --------------------------------------------------------
crs.geog <- 4326
wpt.filename         <- "waypoints_2107RL.csv"
wpt.types            <- c(Adaptive = "Adaptive", Compulsory = "Compulsory", 
                          Nearshore = "Nearshore",Offshore = "Offshore",
                          Saildrone = "Saildrone")
wpt.colors           <- c(Adaptive = "#FF0000", Compulsory = "#000000",  
                          Nearshore = "#FF33F5", Offshore = "#FFA500",
                          Saildrone = "#FFFF00") 
wpt.linetypes        <- c(Adaptive = "dashed", Compulsory = "solid",
                          Nearshore = "solid", Offshore = "dashed", 
                          Saildrone = "solid")

transects.baja <- st_read(here("Data/GIS/Mexico/transectos_BC_SWFSC_INAPESCA.shp"))

# Read transect waypoints
wpts <- read_csv(here("Data/Nav", wpt.filename))

# Convert planned transects to sf; CRS = crs.geog
wpts.sf <- wpts %>% 
  filter(Type %in% wpt.types) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) %>% 
  mutate(
    label = paste("Transect", Transect),
    popup = paste('<b>Transect:</b>', Transect, Type)
  )

transects.sf <- wpts.sf %>% 
  group_by(Type, Transect, Region) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(
    distance = round(as.numeric(st_length(.))/1852,1),
    label    = paste("Transect", Transect),
    popup    = paste('<b>Transect:</b>', Transect, Type, '<br/>',
                     'Distance:', distance, 'nmi<br/>')
  )
