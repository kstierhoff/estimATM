# Summarise transect dates from summer 2021
library(tidyverse) 
library(lubridate)
library(here)
library(sf)
library(mapview)

load("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Backscatter/nasc_all.Rdata")

nasc.summ <- nasc %>% 
  filter(is.na(vessel.orig)) %>% 
  group_by(transect.name, transect, vessel.orig) %>% 
  summarise(start.date = date(min(datetime)),
            end.date = date(max(datetime))) %>% 
  mutate(ship = str_sub(transect.name, 1,2)) %>% 
  arrange(desc(ship), desc(transect)) %>% 
  select(-vessel.orig, -ship)

write_csv(nasc.summ, here("Output/transect_progress_2107RL.csv"))

unique(nasc$vessel.name)

nasc.sd <- filter(nasc, vessel.name == "SD") %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  mutate(region = case_when(
    transect > 110 ~ "north",
    TRUE ~ "south"
  ))

sd.lines <- nasc.sd %>% 
  group_by(transect, region) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

sd.poly.n <- nasc.sd %>% 
  filter(region == "north") %>% 
  concaveman::concaveman(concavity = 100)

sd.poly.s <- nasc.sd %>% 
  filter(region == "south") %>% 
  concaveman::concaveman(concavity = 100)

sd.poly <- rbind(sd.poly.n, sd.poly.s)

nasc.rl <- nasc %>% 
  filter(vessel.name == "RL") %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  st_intersection(sd.poly)

nasc.comp <- rbind(
  select(nasc.rl, vessel.name, cps.nasc),
  select(nasc.sd, vessel.name, cps.nasc)
) %>% filter(cps.nasc > 0) %>% 
  atm::project_sf(crs = 4326)

write_csv(nasc.comp, here("Output/rl_sd_nasc_comparison.csv"))

ggplot() + 
  geom_histogram(data = filter(nasc.sd, cps.nasc > 0), aes(log(cps.nasc))) +
  geom_histogram(data = filter(nasc.rl, cps.nasc > 0), aes(log(cps.nasc)), fill = "red", alpha = 0.5) 

ggplot() + 
  geom_histogram(data = nasc.comp, aes(log(cps.nasc), fill = vessel.name))

nasc.comp2 <- nasc.comp %>% filter(between(cps.nasc, 1, 2e5))
ggplot() + 
  geom_histogram(data = nasc.comp2, aes(cps.nasc/19, fill = vessel.name))

ggplot() + 
  geom_histogram(data = nasc.comp, aes(cps.nasc, fill = vessel.name))

qqplot()
