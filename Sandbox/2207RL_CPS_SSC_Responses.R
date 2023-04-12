# Load packages
pacman::p_load(tidyverse, lubridate, here, sf, gganimate, scatterpie)

theme_set(theme_bw())

# Load data
load(here("Data/Nav/nav_data.Rdata"))
load(here("Data/Nav/nav_data_saildrone.Rdata"))
nav.lm <- readRDS(here("Data/Nav/nav_vessel_LM.rds")) %>% 
  filter(lat != 999, long != 999)

# Quick plot
ggplot() + 
  geom_path(data = nav, aes(long, lat)) +
  geom_path(data = nav.lm, aes(long, lat), colour = "green") +
  geom_path(data = nav.sd, aes(long, lat, colour = factor(saildrone))) +
  coord_map()

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

nav.summ.all <- nav.summ.rl %>% bind_rows(nav.summ.lm) %>% bind_rows(nav.summ.sd)

# Add leg breaks
# Use start dates of each leg + end date of last leg
leg.breaks <- data.frame(
  date = lubridate::date(c("2022-06-27", "2022-07-19", "2022-08-12", "2022-09-06", "2022-09-27")),
  leg = c("Leg 1","Leg 2","Leg 3","Leg 4", "End"))

landmarks <- data.frame(
  lat = c(36.646, 40.50),
  name = c("Monterey","Cape Mendocino"),
  date = date("2022-07-07"))


ggplot(nav.summ.all, 
       aes(date, lat, group = vessel, colour = vessel)) + 
  geom_line(linewidth = 2) + 
  geom_point(size = 2, shape = 21, fill = "white") + 
  geom_vline(xintercept = leg.breaks$date, linetype = "dashed") +
  geom_hline(yintercept = landmarks$lat, linetype = "dashed") +
  geom_text(data = leg.breaks, aes(date, 48, label = leg), inherit.aes = FALSE) +
  geom_text(data = landmarks, aes(date, lat+0.25, label = name), inherit.aes = FALSE) +
  scale_x_date(date_breaks = "10 days") + 
  labs(x = "Date", y = "Latitude")

ggsave(here("Figs/fig_vessel_lag.png"), height = 7, width = 12)

clf.seine <- read_csv("Output/clf_ts_proportions_seine.csv")

lm.sets <- read_csv(here("Data/Seine/lm_sets.csv"), lazy = FALSE) %>% 
  mutate(date = mdy(date),
         datetime = ymd_hms(paste(date, time), tz = "America/Los_Angeles"),
         vessel_name = "Lisa Marie",
         vessel.name = "LM",
         key.set = paste(vessel.name, date, set),
         sample.type = "Purse seine") %>% 
  # Convert datetime to UTC
  mutate(datetime = with_tz(datetime, tzone = "UTC"))

lm.catch <- read_csv(here("Data/Seine/lm_catch.csv")) %>% 
  mutate(date = mdy(date),
         vessel.name = "Lisa Marie",
         vessel.name = "LM",
         key.set = paste(vessel.name, date, set),
         scientificName = case_when(
           species_name == "Pacific Herring" ~ "Clupea pallasii",
           species_name == "Pacific Sardine" ~ "Sardinops sagax",
           species_name == "Pacific Mackerel" ~ "Scomber japonicus",
           species_name == "Jack Mackerel" ~ "Trachurus symmetricus",
           species_name == "Northern Anchovy" ~ "Engraulis mordax",
           species_name == "Jacksmelt" ~ "Atherinopsis californiensis",
           species_name == "Whitebait Smelt" ~ "Allosmerus elongatus",
           TRUE ~ NA_character_)) %>% 
  filter(!is.na(scientificName)) %>% 
  group_by(key.set, vessel.name, scientificName) %>% 
  summarise(totalWeight = sum(weightkg)) 

lm.catch.summ <- lm.catch %>%
  select(key.set, scientificName, totalWeight) %>% 
  tidyr::spread(scientificName, totalWeight) %>% 
  right_join(select(lm.sets, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set) 

set.summ.wt <- lm.catch.summ 

# Add species with zero total weight
if (!has_name(set.summ.wt, "Engraulis mordax"))      {set.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(set.summ.wt, "Sardinops sagax"))       {set.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(set.summ.wt, "Scomber japonicus"))     {set.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(set.summ.wt, "Trachurus symmetricus")) {set.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(set.summ.wt, "Clupea pallasii"))       {set.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(set.summ.wt, "Etrumeus acuminatus"))   {set.summ.wt$`Etrumeus acuminatus`   <- 0}
if (!has_name(set.summ.wt, "Atherinopsis californiensis")) {set.summ.wt$`Atherinopsis californiensis` <- 0}
if (!has_name(set.summ.wt, "Other"))                 {set.summ.wt$`Other` <- 0}

set.summ.wt <- set.summ.wt %>%  
  ungroup() %>% 
  replace(is.na(.), 0) %>% 
  select(key.set, vessel.name, lat, long, everything()) %>% 
  mutate(AllCPS = rowSums(select(., -(key.set:long)))) %>%
  rename("Jacksmelt"  = "Atherinopsis californiensis",
         "PacHerring" = "Clupea pallasii",
         "Anchovy"    = "Engraulis mordax",
         "Sardine"    = "Sardinops sagax",
         "PacMack"    = "Scomber japonicus",
         "JackMack"   = "Trachurus symmetricus",
         "RndHerring" = "Etrumeus acuminatus",
         "Other"      = "Other") %>% 
  atm::project_df(to = 3310) %>% 
  arrange(PacMack)


# Set species colors
sardine.color      <- '#FF0000'
  anchovy.color      <- '#00CD66'
    jack.mack.color    <- '#0000FF'
      jacksmelt.color    <- '#A020F0'
        pac.mack.color     <- '#00FFFF'
          pac.herring.color  <- '#F5DEB3'
            rnd.herring.color  <- '#F0B81D'
              other.color        <- 'gray'

ggplot() +
  # Plot seine data
  scatterpie::geom_scatterpie(data = set.summ.wt, 
                              aes(X, Y, group = key.set, r = 25000),
                              cols = c("Anchovy", "JackMack", "PacHerring", 
                                       "PacMack","Sardine"),
                              color = NA, alpha = 0.8) +
  scatterpie::geom_scatterpie(data = filter(set.summ.wt, PacMack > 0), 
                              aes(X, Y, group = key.set, r = 25000),
                              cols = c("Anchovy", "JackMack", "PacHerring", 
                                       "PacMack","Sardine"),
                              color = "black", alpha = 1) +
  # geom_scatterpie(data = set.summ.wt, 
  #                 aes(X, Y, group = cluster, r = 25000),
  #                 cols = c("prop.anch","prop.jack",
  #                          "prop.mack", "prop.sar"),
  #                 alpha = 0.8,
  #                 colour = NA) +
  # # Plot empty trawl locations
  # geom_point(data = cluster.zero, aes(X, Y),
  #            size = 3, shape = 21, fill = 'black', colour = 'white') +
  # # Configure pie outline colors
  # scale_colour_manual(name = "Sample type", 
  #                     labels = c("Purse seine", "Trawl"),
  #                     values = c(seine.color, trawl.color),
  #                     guide = "none") +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. mackerel",
                               "P. herring","P. mackerel","Sardine"),
                    values = c(anchovy.color, jack.mack.color,  
                               pac.herring.color, pac.mack.color,
                               sardine.color)) +
  # Plot panel label
  ggtitle("Seine catch (kg)") +
  coord_sf(crs = 3310) +
  labs(x = "Longitude", y = "Latitude")

ggsave(here("Figs/fig_mackerel_catch.png"), height = 12, width = 6)




