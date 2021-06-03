library(tidyverse)

hab <- read_csv(here::here("Data/Habitat/sardine_habitat.csv"), col_names = FALSE) 

names(hab) <- c("datetime","lat","long","potential_hab",
                "potential_hab_mask","chl_a", "sst")

ggplot(hab, aes(long, lat, fill = chl_a)) + 
  geom_raster() + 
  scale_fill_viridis_c()

ggplot(hab, aes(long, lat, fill = potential_hab)) + 
  geom_raster() + 
  scale_fill_viridis_c() + 
  theme_bw()

