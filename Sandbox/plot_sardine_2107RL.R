library(tidyverse)

load("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Trawl/trawl_data.Rdata")

lengths.rl <- filter(lengths.all) %>% 
  filter(cruise == "202107") %>% 
  left_join(select(spp.codes, species, scientificName)) %>% 
  filter(scientificName == "Sardinops sagax") %>% 
  mutate(ship = "RL") %>% 
  select(ship, standardLength_mm)

catch <- catch.all %>% 
  filter(cruise == "202107") %>% 
  left_join(select(spp.codes, species, scientificName)) %>% 
  filter(scientificName == "Sardinops sagax") %>% 
  mutate(ship = "RL")

filter(lengths.rl, scientificName == "Sardinops sagax", haul %in% c(68,71)) %>% write_csv(here::here("Output/sardine_specimens.csv"))

lengths.lm <- read_csv("Data/Seine/lm_specimens.csv") %>% 
  filter(species_name == "Pacific Sardine") %>% 
  mutate(scientificName = "Sardinops sagax",
         ship = "LM",
         standardLength_mm = mfbds_v_fish.fish_length) 

lengths.lbc <- read_csv("Data/Seine/lbc_catch.csv") %>% 
  filter(scientificName == "Sardinops sagax") %>% 
  mutate(ship = "LBC")
  

lengths.jcf <- read_csv("Data/Trawl/JCFINP2110_specimens.csv") %>% 
  mutate(ship = "JCF")

l.all <- lengths.rl %>% 
  bind_rows(select(lengths.lbc, ship, standardLength_mm)) %>% 
  bind_rows(select(lengths.jcf, ship, standardLength_mm)) %>% 
  bind_rows(select(lengths.lm, ship, standardLength_mm)) %>% 
  mutate(standardLength_cm = standardLength_mm/10)


ggplot() + 
  geom_histogram(data = lengths.rl, aes(standardLength_mm)) + 
  geom_histogram(data = lengths.lbc, aes(standardLength_mm), alpha = 0.5, fill = "red") +
  geom_histogram(data = lengths.jcf, aes(standardLength_mm), alpha = 0.5, fill = "yellow")

ggplot(l.all, aes(standardLength_cm, fill = ship)) + 
  geom_histogram(alpha = 0.5) + 
  facet_wrap(~ship, ncol = 1)
