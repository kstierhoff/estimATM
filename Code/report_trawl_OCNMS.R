library(tidyverse)
library(here)

load(here("Output/haul_info.Rdata"))
load(here("Output/catch_final.Rdata"))

haul %>% 
  select(cruise, haul, startLat = startLatDecimal, startLong = startLongDecimal,
         stoptLat = stopLatDecimal, stopLong = stopLongDecimal,
         startTime = equilibriumTime, stopTime = haulBackTime) %>% 
write_csv(here::here("Output/2107RL_hauls_ONMS.csv"))
