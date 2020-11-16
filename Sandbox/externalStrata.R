# library(tidyverse)
# library(sf)

# Source polygon info
source(here::here("Data/Backscatter/polygons_sardine_1606RL.R"))

if (exists("polygon.df")) rm(polygon.df)

# Generate polygon stratum numbers
if (polygon.reorder) {
  polygon.nums <- rev(seq_along(polygon.list))
} else {
  polygon.nums <- seq_along(polygon.list)
}

for (i in seq_along(polygon.list)) {
  
  tmp.df <- data.frame(scientificName = polygon.spp,
                       stock = polygon.stock,
                       stratum = polygon.nums[i],
                       vessel.name = polygon.vessel,
                       as.data.frame(polygon.list[[i]])) %>% 
    rename(lat = y, long = x)
  
  if (exists("polygon.df")) {
    polygon.df <- bind_rows(polygon.df, tmp.df)
  } else {
    polygon.df <- tmp.df
  }
}

strata.polygon.final <- polygon.df %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  group_by(scientificName, stock, stratum, vessel.name) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON") %>% 
  ungroup() %>% 
  mutate(area = st_area(.),
         stratum = case_when(
           polygon.offset > 0 ~ stratum + as.integer(polygon.offset),
           TRUE ~ stratum),
           key = paste(scientificName, stock, stratum))

# # View results
# mapview::mapview(polygon.sf, zcol = "stratum") + 
#   mapview::mapview(transects.sf)
# 
# 
# mapview::mapview(filter(nasc.plot.sf, 
#                         str_detect(transect.name, "RL 07[\\d]"))) + 
#   mapview::mapview(polygon.sf, zcol = "stratum") 
# 
# mapview::mapview(filter(nasc.plot.sf, between(transect,91,100))) + 
#   mapview::mapview(polygon.sf, zcol = "stratum") 
#   
