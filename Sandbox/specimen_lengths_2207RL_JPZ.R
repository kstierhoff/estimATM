library(tidyverse)
library(here)

load("C:/KLS/CODE/Github/estimATM/2207RL/Data/Trawl/all_trawl_data-final.Rdata")

lengths.loc <- lengths %>% 
  left_join(select(haul, haul, lat = startLatDecimal, long = startLongDecimal, sample.type)) %>% 
  filter(!is.na(sample.type))

ggplot(lengths.loc, aes(long, lat, colour = sample.type)) + 
  geom_point() + 
  geom_point(data = lengths.loc, aes(long, lat, colour = sample.type)) +
  coord_map()

ggplot(lengths.loc, aes(weightg, fill = sample.type)) + 
  geom_histogram() + 
  facet_grid(sample.type ~ scientificName, scales = "free")

write_csv(lengths.loc, here("Output/lengths_w_locations_2207RL.csv"))
  
