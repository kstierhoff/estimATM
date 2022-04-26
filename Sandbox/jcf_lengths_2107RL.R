library(tidyverse)
library(here)

l.jcf <- read_csv(here("Data/Trawl/JCFINP2110_specimens.csv"))

ggplot() + 
  geom_point(data = l.jcf, aes(forkLength_mm, totalWeightg), colour = "red") + 
  geom_point(data = l.jcf, aes(standardLength_mm, totalWeightg), colour = "blue") + 
  facet_wrap(~species, scales = "free") +
  labs(x = "Length (mm)", y = "Weight (g)")
 

ggplot() + 
  geom_point(data = lengths.jcf, aes(forkLength_mm, weightg), colour = "red") + 
  geom_point(data = lengths.jcf, aes(standardLength_mm, weightg), colour = "blue") + 
  facet_wrap(~scientificName, scales = "free")

ggplot() + 
  geom_point(data = lengths, aes(totalLength_mm, weightg), colour = "red") + 
  geom_point(data = lengths.jcf, aes(totalLength_mm, weightg), colour = "blue") +
  facet_wrap(~scientificName, scales = "free")
