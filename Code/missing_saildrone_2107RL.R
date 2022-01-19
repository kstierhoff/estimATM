library(tidyverse)
library(lubridate)
library(shadowtext)

load("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Nav/nav_data_saildrone.Rdata")

states <- map_data("state") %>% 
  filter(region %in% c("california","oregon","washington"))

wpts <- read_csv("Data/Nav/waypoints_2107RL.csv") %>% 
  filter(Type %in% c("Saildrone", "Offshore"))

wpt.labels <- wpts %>% 
  group_by(Transect) %>% 
  slice(1)

theme_set(theme_bw())

sd.missing <- nav.sd %>% 
  filter(between(datetime, ymd_hms("2021-09-22 11:48:00"),ymd_hms("2021-09-22 14:03:00")))

nav.1059.missing <- nav.sd %>% 
  filter(saildrone == 1059,
         between(datetime, 
                 ymd_hms("2021-08-10 00:00:00"), 
                 ymd_hms("2021-08-30 23:59:00")))

nasc.1059.missing <- nav.sd %>% 
  filter(saildrone == 1059,
         between(datetime, 
                 ymd_hms("2021-08-03 00:00:00"),
                 ymd_hms("2021-08-30 23:59:00")))


nasc.1036.missing <- nav.sd %>% 
  filter(saildrone == 1036,
         between(datetime, 
                 ymd_hms("2021-09-22 11:48:00"),
                 ymd_hms("2021-09-22 14:03:00")))

# Plot missing 1059 data
ggplot() + 
  geom_polygon(data = states, aes(long, lat, group = group), colour = "black", fill = "gray50") +
  geom_shadowtext(data = wpt.labels, aes(Longitude, Latitude, label = Transect),
                  colour = "black", bg.colour = "white") +
  geom_path(data = wpts, aes(Longitude, Latitude, group = Transect),
            colour = "gray50", alpha = 0.5, size = 1.5) +
  geom_path(data = filter(wpts, Transect %in% c(39,40)), 
            aes(Longitude, Latitude, group = Transect),
            size = 1.5,colour = "red") +
  geom_path(data = filter(wpts, Transect %in% c(47,48)), 
            aes(Longitude, Latitude, group = Transect),
            size = 1.5,colour = "orange") +
  geom_path(data = nav.sd, aes(long, lat, group = saildrone, colour = factor(saildrone)), 
            size = 0.5, alpha = 0.5) +
  # geom_path(data = filter(nav.sd, saildrone == 1059), aes(long, lat, group = saildrone), 
            # colour = "black", size = 0.5, alpha = 0.5) +
  geom_path(data = nasc.1059.missing, aes(long, lat), 
            colour = "cyan", size = 1) +
  geom_point(data = nav.1059.missing, aes(long, lat), 
             colour = "blue", size = 1) +
  geom_path(data = nasc.1036.missing, aes(long, lat), 
            colour = "purple", size = 1) +
  # geom_point(data = sd.missing, aes(long, lat, colour = factor(saildrone))) +
  coord_map() 

ggsave(file = here::here("Figs/fig_saildrone_missing_2107RL.png"),
       height = 15, width = 10)

