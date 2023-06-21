## A quick comparison of CPS proportions in 2021 where sardine were present with other CPS
## To examine the adjustment of species proportions in areas where LM sampled and RL did not

library(tidyverse)
library(scatterpie)

load("C:/KLS/CODE/Github/estimATM/2107RL/Output/clf_ts_proportions.Rdata")

clf <- read_csv(here("Output/clf_ts_proportions.csv"))
clf <- read_csv("C:/KLS/CODE/Github/estimATM/2107RLOutput/clf_ts_proportions.csv")

sardine.color      <- '#FF0000'
  anchovy.color      <- '#00CD66'
    jack.mack.color    <- '#0000FF'
        pac.mack.color     <- '#00FFFF'
          pac.herring.color  <- '#F5DEB3'
            rnd.herring.color  <- '#F0B81D'

ggplot(filter(clf, prop.sar > 0), aes(prop.sar, prop.jack)) + 
  geom_point()

ggplot() + 
  geom_scatterpie(data = filter(clf, prop.sar > 0), 
                  aes(long, lat, group = cluster),
                  cols = c("prop.anch","prop.her","prop.jack",
                           "prop.mack","prop.rher","prop.sar"),
                  color = 'black', alpha = 0.8)  + 
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "P. herring", "J. mackerel",
                               "P. mackerel", "R. herring", "Sardine"),
                    values = c(anchovy.color, pac.herring.color, jack.mack.color,  
                               pac.mack.color, rnd.herring.color, sardine.color)) + 
  coord_map()

ggplot() + 
  geom_scatterpie(data = filter(clf, prop.sar > 0), 
                  aes(X, Y, group = cluster),
                  cols = c("prop.anch","prop.her","prop.jack",
                           "prop.mack","prop.rher","prop.sar"),
                  color = 'black', alpha = 0.8)  + 
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "P. herring", "J. mackerel",
                               "P. mackerel", "R. herring", "Sardine"),
                    values = c(anchovy.color, pac.herring.color, jack.mack.color,  
                               pac.mack.color, rnd.herring.color, sardine.color))

filter(clf, prop.sar > 0, between(lat, 40, 46.5)) %>% 
  select(long, lat, prop.sar, prop.jack, prop.anch, prop.her, prop.mack) %>% 
  mutate(prop.other = prop.anch + prop.her + prop.mack)

# ggplot() + 
#   geom_scatterpie(data = filter(clf), 
#                   aes(long, lat, group = cluster),
#                   cols = c("prop.anch","prop.her","prop.jack",
#                            "prop.mack","prop.rher","prop.sar"),
#                   color = 'black', alpha = 0.8, r = 0.0005)  + 
#   # Configure trawl scale
#   scale_fill_manual(name = 'Species',
#                     labels = c("Anchovy", "P. herring", "J. mackerel",
#                                "P. mackerel", "R. herring", "Sardine"),
#                     values = c(anchovy.color, pac.herring.color, jack.mack.color,  
#                                pac.mack.color, rnd.herring.color, sardine.color))
