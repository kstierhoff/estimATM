# Load packages
pacman::p_load(tidyverse, lubridate, here, sf, gganimate, scatterpie)
# 
# theme_set(theme_bw())

# Load nav data
load(here("Data/Nav/nav_data.Rdata"))
nav.lm <- readRDS(here("Data/Nav/nav_vessel_lm.rds")) %>% 
  filter(lat != 999, long != 999)
nav.lbc <- readRDS(here("Data/Nav/nav_vessel_lbc.rds")) %>% 
  filter(lat != 999, long != 999)

# Load backscatter data
load(here("Data/Backscatter/nasc_all.Rdata"))

# # Quick plot
# ggplot() +
#   geom_path(data = nav, aes(long, lat)) +
#   geom_path(data = nav.lm, aes(long, lat), colour = "green") +
#   geom_path(data = nav.lbc, aes(long, lat), colour = "red") +
#   coord_map()

# Summarize latitude by date
nasc.summ.all <- nasc %>% 
  mutate(date = date(datetime)) %>% 
  mutate(vessel = case_when(
    vessel.name == "RL" ~ "Lasker",
    vessel.name == "LM" ~ "Lisa Marie",
    vessel.name == "LBC" ~ "Long Beach Carnage")) %>% 
  group_by(vessel, date) %>% 
  summarise(lat = mean(lat)) %>% 
  mutate(key = paste(vessel, date))

nav.summ.rl <- nav %>% 
  mutate(vessel = "Lasker",
         date = date(time)) %>% 
  group_by(vessel, date) %>% 
  summarise(lat = mean(lat)) %>% 
  mutate(key = paste(vessel, date)) %>% 
  mutate(sampling = case_when(
    key %in% unique(nasc.summ.all$key) ~ TRUE,
    TRUE ~ FALSE))

nav.summ.lm <- nav.lm %>% 
  mutate(vessel = "Lisa Marie",
         date = date(datetime),
         sampling = TRUE) %>% 
  group_by(vessel, date, sampling) %>% 
  summarise(lat = mean(lat)) 

nav.summ.lbc <- nav.lbc %>% 
  mutate(vessel = "Long Beach Carnage",
         date = date(datetime),
         sampling = TRUE) %>% 
  group_by(vessel, date, sampling) %>% 
  summarise(lat = mean(lat)) 

nav.summ.all <- nav.summ.rl %>% bind_rows(nav.summ.lbc) %>% bind_rows(nav.summ.lm) 

# Add leg breaks
# Use start dates of each leg + end date of last leg
leg.breaks <- data.frame(
  date = lubridate::ymd(c("2024-06-26", "2024-07-22", 
                          "2024-08-17", "2024-09-12",
                          "2024-09-30")),
  leg = c("Leg 1","Leg 2","Leg 3","Leg 4", "End"))

landmarks <- data.frame(
  lat = c(36.646, 40.50),
  name = c("Monterey","Cape Mendocino"),
  date = date("2024-07-07"))

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
       filename = here("Figs/fig_vessel_coordination.png"), 
       height = 7, width = 12)
