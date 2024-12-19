# Export specimen lengths and locations for generating stock separation figure

pacman::p_load(tidyverse, here)

# Core area specimens
lengths <- read_csv(here("Output/lengths_final.csv"))

haul.info <- readRDS(here("Output/haul_info.rds")) %>% 
  select(haul, equilibriumTime, startLatDecimal, startLongDecimal)

# Join tables
lengths <- lengths %>% 
  left_join(haul.info)

write_csv(lengths, here("Output/specimens_core_juan.csv"), na = "")

# Nearshore area specimens
load(here("Output/purse_seine_sets.Rdata"))
load(here("Output/purse_seine_lengths.Rdata"))

set.lengths <- set.lengths %>% 
  left_join(select(sets, key.set, lat, long, datetime))

write_csv(set.lengths, here("Output/specimens_nearshore_juan.csv"), na = "")
