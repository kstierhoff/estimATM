library(tidyverse)
library(xts)

load("C:/KLS/CODE/Github/estimATM/2207RL/Data/Nav/nav_data.Rdata")

# Quick nav data plot
ggplot(nav, aes(long, lat)) + geom_path()

# Summarize data per hour to look at potential trawl locations
nav.summ <- nav %>% 
  mutate(time.align = align.time(time)) %>% 
  group_by(time.align, n = 900) %>% 
  summarize(
    SOG = mean(SOG), 
    lat = mean(lat),
    long = mean(long)
  )
  
ggplot(nav, aes(long, lat)) + 
  geom_path(aes(colour = SOG)) + 
  coord_map() + 
  scale_colour_viridis_c()

