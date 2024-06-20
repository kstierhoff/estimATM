library(tidync)

con <- tidync::tidync(here::here("Data/ADCP/os75nb.nc"))

grid_identifier <- "D7,D9,D11,D8"

lon <- tidync(here::here("Data/ADCP/os75nb.nc")) %>% activate("lon") %>% hyper_tibble()
lat <- tidync(here::here("Data/ADCP/os75nb.nc")) %>% activate("lat") %>% hyper_tibble()


nav <- dplyr::bind_cols(lat, dplyr::select(lon,lon)) %>% 
  arrange(time)

library(tidyverse)

ggplot(nav, aes(lon, lat)) + geom_line()
