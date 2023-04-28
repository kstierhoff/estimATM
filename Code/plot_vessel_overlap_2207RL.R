# Load packages
# pacman::p_load(tidyverse, lubridate, here, sf, gganimate, scatterpie)
# 
# theme_set(theme_bw())

# Load nav data
load(here("Data/Nav/nav_data.Rdata"))
load(here("Data/Nav/nav_data_saildrone.Rdata"))
nav.lm <- readRDS(here("Data/Nav/nav_vessel_LM.rds")) %>% 
  filter(lat != 999, long != 999)

# Load backscatter data
load(here("Data/Backscatter/nasc_all.Rdata"))

# # Quick plot
# ggplot() + 
#   geom_path(data = nav, aes(long, lat)) +
#   geom_path(data = nav.lm, aes(long, lat), colour = "green") +
#   geom_path(data = nav.sd, aes(long, lat, colour = factor(saildrone))) +
#   coord_map()

# Summarize latitude by date
nav.summ.rl <- nav %>% 
  mutate(vessel = "Lasker",
         date = date(time)) %>% 
  group_by(vessel, date) %>% 
  summarise(lat = mean(lat)) 

nav.summ.lm <- nav.lm %>% 
  mutate(vessel = "Lisa Marie",
         date = date(datetime)) %>% 
  group_by(vessel, date) %>% 
  summarise(lat = mean(lat)) 

nav.summ.sd <- nav.sd %>% 
  mutate(date = date(datetime)) %>% 
  group_by(saildrone, date) %>% 
  summarise(lat = mean(lat)) %>% 
  mutate(vessel = paste("Saildrone", saildrone)) %>% 
  select(-saildrone)

nasc.summ.all <- nasc %>% 
  mutate(date = date(datetime),
         vessel.name = case_when(
           is.na(vessel.orig) ~ vessel.name,
           TRUE ~ vessel.orig)) %>% 
  mutate(vessel = case_when(
    vessel.name == "RL" ~ "Lasker",
    vessel.name == "LM" ~ "Lisa Marie",
    vessel.name == "SD1076" ~ "Saildrone 1076",
    vessel.name == "SD1077" ~ "Saildrone 1077"
  )) %>% 
  group_by(vessel, date) %>% 
  summarise(lat = mean(lat)) %>% 
  mutate(key = paste(vessel, date))

nav.summ.all <- nav.summ.rl %>% bind_rows(nav.summ.lm) %>% bind_rows(nav.summ.sd) %>% 
  mutate(key = paste(vessel, date)) %>% 
  mutate(sampling = case_when(
    key %in% unique(nasc.summ.all$key) ~ TRUE,
    TRUE ~ FALSE))

# Add leg breaks
# Use start dates of each leg + end date of last leg
leg.breaks <- data.frame(
  date = lubridate::date(c("2022-06-27", "2022-07-19", "2022-08-12", "2022-09-06", "2022-09-27")),
  leg = c("Leg 1","Leg 2","Leg 3","Leg 4", "End"))

landmarks <- data.frame(
  lat = c(36.646, 40.50),
  name = c("Monterey","Cape Mendocino"),
  date = date("2022-07-07"))

# ggplot(filter(nav.summ.all, vessel == "Lasker"), aes(date, lat)) + 
#   geom_path() + 
#   geom_point(aes(fill = sampling), shape = 21)

vessel.coord.plot <- ggplot(nav.summ.all, 
       aes(date, lat, group = vessel, colour = vessel)) + 
  geom_line(linewidth = 1, linetype = "dashed") + 
  geom_point(aes(fill = sampling), size = 2, shape = 21) + 
  geom_vline(xintercept = leg.breaks$date, linetype = "dashed") +
  geom_hline(yintercept = landmarks$lat, linetype = "dashed") +
  geom_text(data = leg.breaks, aes(date, 48, label = leg), inherit.aes = FALSE) +
  geom_text(data = landmarks, aes(date, lat+0.25, label = name), inherit.aes = FALSE) +
  scale_colour_discrete(name = "Vessel") +
  scale_fill_manual(name = "Sampling", values = c("TRUE" = 'black', "FALSE" = 'white')) +
  scale_x_date(date_breaks = "10 days") + 
  labs(x = "Date", y = "Latitude") + 
  theme_bw()

ggsave(vessel.coord.plot, 
       here("Figs/fig_vessel_coordination.png"), 
       height = 7, width = 12)
