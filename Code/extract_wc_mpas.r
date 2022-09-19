library(sf)
library(tidyverse)
library(mapview)

mpa.gdb <- st_read("C:/Users/kevin.stierhoff/Downloads/NOAA_MPAI_2020_IUCN_gdb")

wc.mpas <- mpa.gdb %>% 
  filter(State %in% c("CA","WA","OR")) %>% 
  select(Site_ID, Site_Name, State, Gov_Level, Mgmt_Plan, Mgmt_Agen, Fish_Rstr)

st_write(wc.mpas, dsn = "C:/KLS/CODE/Github/estimATM/2207RL/Data/GIS/wc_mpas.shp",
         delete_layer = TRUE)

mapview(wc.mpas, zcol = "Marine")
