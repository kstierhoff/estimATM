library(tidyverse)
library(lubridate)
library(here)
library(atm)
library(surveyR)

theme_set(theme_bw())

# Load Lasker data
load("C:/KLS/CODE/Github/estimATM/2107RL/Data/Trawl/trawl_data.Rdata")

lengths.2207RL <- readRDS("C:/KLS/CODE/Github/estimATM/2207RL/Output/lw_data_checkTrawls.rds") %>% 
  mutate(season = "summer") 

load("C:/KLS/CODE/Github/estimATM/2207RL/Output/haul_info.Rdata")

haul.2207RL <- haul %>% 
  mutate(region = case_when(
    startLatDecimal < 34.46 ~ "South",
    between(startLatDecimal, 34.46, 40.50) ~ "Central",
    startLatDecimal > 40.50 ~ "North",
    TRUE ~ "Other"))  %>% 
  filter(!is.na(region)) %>% 
  mutate(region = factor(region, levels = rev(c("South", "Central", "North"))))

lengths.2207RL <- lengths.2207RL %>% 
  left_join(select(haul.2207RL, cruise, ship, haul, region))

# Load LM data from 2022
load(here("Output/purse_seine_sets.Rdata"))

lm.sets <- lm.sets %>% 
  mutate(region = case_when(
    lat < 34.46 ~ "South",
    between(lat, 34.46, 40.50) ~ "Central",
    lat > 40.50 ~ "North",
    TRUE ~ "Other"))

lengths.lm <- readRDS(here("Output/lw_data_nearshore.rds")) %>% 
  left_join(select(lm.sets, key.set, region)) %>% 
  # Compute total length
  mutate(
    season = "summer",
    cruise = "202207-LM",
    totalLength_mm = case_when(
      scientificName == "Clupea pallasii" ~ 
        convert_length("Clupea pallasii", .$forkLength_mm, "FL", "TL"),
      scientificName == "Engraulis mordax" ~ 
        convert_length("Engraulis mordax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Sardinops sagax" ~ 
        convert_length("Sardinops sagax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Scomber japonicus" ~ 
        convert_length("Scomber japonicus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Trachurus symmetricus" ~ 
        convert_length("Trachurus symmetricus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Etrumeus acuminatus" ~ 
        convert_length("Etrumeus acuminatus", .$forkLength_mm, "FL", "TL")),
    missing.length = case_when(is.na(totalLength_mm) ~ T, TRUE ~ FALSE)) %>% 
  # Compute differences from model predictions
  mutate(weightg.pred = estimate_weight(.$scientificName, .$totalLength_mm, season = season), 
         weightg.resid = weightg - weightg.pred,
         weightg.pctdiff = (weightg - weightg.pred)/weightg.pred*100)

# Define species to be analysed
cps.spp <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
             "Scomber japonicus","Trachurus symmetricus")

lengths.all <- lengths.all %>% 
  left_join(spp.codes) %>% 
  filter(scientificName %in% cps.spp)

# Classify hauls by season (spring or summer)
haul.all <- haul.all %>% 
  mutate(season = case_when(
    month(equilibriumTime) < 6 ~ "spring",
    TRUE ~ "summer")) %>% 
  mutate(region = case_when(
    startLatDecimal < 34.46 ~ "South",
    between(startLatDecimal, 34.46, 40.50) ~ "Central",
    startLatDecimal > 40.50 ~ "North",
    TRUE ~ "Other")) %>% 
  filter(!is.na(region)) %>% 
  mutate(region = factor(region, levels = rev(c("South", "Central", "North"))))

# ggplot(haul.all, aes(startLongDecimal, startLatDecimal, colour = region)) + geom_point()

# Classify specimens as spring or summer using hauls
lengths.all <- lengths.all %>% 
  left_join(select(haul.all, cruise, ship, haul, season, region)) %>% 
  bind_rows(lengths.2207RL) %>% 
  # Compute total length
  mutate(
    totalLength_mm = case_when(
      scientificName == "Clupea pallasii" ~ 
        convert_length("Clupea pallasii", .$forkLength_mm, "FL", "TL"),
      scientificName == "Engraulis mordax" ~ 
        convert_length("Engraulis mordax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Sardinops sagax" ~ 
        convert_length("Sardinops sagax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Scomber japonicus" ~ 
        convert_length("Scomber japonicus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Trachurus symmetricus" ~ 
        convert_length("Trachurus symmetricus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Etrumeus acuminatus" ~ 
        convert_length("Etrumeus acuminatus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Doryteuthis opalescens" ~  as.numeric(mantleLength_mm)),
    missing.length = case_when(is.na(totalLength_mm) ~ T, TRUE ~ FALSE)) %>% 
  # Compute differences from model predictions
  mutate(weightg.pred = estimate_weight(.$scientificName, .$totalLength_mm, season = season), 
         weightg.resid = weightg - weightg.pred,
         weightg.pctdiff = (weightg - weightg.pred)/weightg.pred*100)

# Plot LW data from specimens --------------------------------------------------
# Data frame for storing results
lw.df.spring <- data.frame()
lw.df.summer <- data.frame()

L.max.all <- select(lengths.all, scientificName, totalLength_mm) %>%
  # Add LBC data
  filter(scientificName %in%  cps.spp) %>%
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

# Generate length/weight curves
for (i in unique(L.max.all$scientificName)) {
  # Create a length vector for each species
  totalLength_mm <- seq(0, L.max.all$max.TL[L.max.all$scientificName == i])
  
  # Calculate weights from lengths
  weightg.spring <- estimate_weight(i, totalLength_mm, season = "spring")
  weightg.summer <- estimate_weight(i, totalLength_mm, season = "summer")
  
  # Combine results
  lw.df.spring <- bind_rows(lw.df.spring, data.frame(scientificName = i, weightg = weightg.spring, totalLength_mm))
  lw.df.summer <- bind_rows(lw.df.summer, data.frame(scientificName = i, weightg = weightg.summer, totalLength_mm))
}

# Convert lengths for plotting
lw.df.spring <- lw.df.spring %>% 
  mutate(
    forkLength_mm     = convert_length(scientificName, totalLength_mm, "TL", "FL"),
    standardLength_mm = convert_length(scientificName, totalLength_mm, "TL", "SL")
  ) %>% 
  filter(totalLength_mm > 20)

lw.df.summer <- lw.df.summer %>% 
  mutate(
    forkLength_mm     = convert_length(scientificName, totalLength_mm, "TL", "FL"),
    standardLength_mm = convert_length(scientificName, totalLength_mm, "TL", "SL")
  ) %>% 
  filter(totalLength_mm > 20)

spp.sl <- c("Engraulis mordax","Sardinops sagax")
spp.fl <- c("Scomber japonicus","Trachurus symmetricus","Clupea pallasii")

lengths.sl <- filter(lengths.all, scientificName %in% spp.sl, !is.na(standardLength_mm), !is.na(weightg))
lengths.fl <- filter(lengths.all, scientificName %in% spp.fl, !is.na(forkLength_mm), !is.na(weightg))

# Anchovy ------------------------------------------------------------------
# Plot anchovy in spring - All years
# Linear
ggplot(filter(lengths.sl, scientificName == "Engraulis mordax", season == "spring"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.spring, scientificName == "Engraulis mordax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  xlim(20, 170) + ylim(0.4,40) +
  ggtitle("Anchovy - Spring") +
  theme_bw()

ggsave(filename = here("Figs/LW_comparisons/anch_spring_LW.png"), 
       height = 10, width = 10)

# Log-log
ggplot(filter(lengths.sl, scientificName == "Engraulis mordax", season == "spring"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.spring, scientificName == "Engraulis mordax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  xlim(20, 170) + ylim(0.4, 40) +
  coord_trans(x ="log", y = "log") +
  ggtitle("Anchovy - Spring") 

ggsave(filename = here("Figs/LW_comparisons/anch_spring_LW_log.png"), 
       height = 10, width = 10)

# Plot anchovy in summer - All years
# Linear
ggplot(filter(lengths.sl, scientificName == "Engraulis mordax", season == "summer"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_point(data = filter(lengths.lm, scientificName == "Engraulis mordax", season == "summer"), 
             aes(standardLength_mm, weightg, colour = region)) +
  geom_line(data = filter(lw.df.summer, scientificName == "Engraulis mordax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  xlim(20, 170) + ylim(0.4, 55) +
  ggtitle("Anchovy - Summer")

ggsave(filename = here("Figs/LW_comparisons/anch_summer_LW.png"), 
       height = 10, width = 10)

# Log-log
ggplot(filter(lengths.sl, scientificName == "Engraulis mordax", season == "summer"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_point(data = filter(lengths.lm, scientificName == "Engraulis mordax", season == "summer"), 
             aes(standardLength_mm, weightg, colour = region)) +
  geom_line(data = filter(lw.df.summer, scientificName == "Engraulis mordax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  xlim(20, 170) + ylim(0.4, 55) +
  coord_trans(x ="log", y = "log") +
  ggtitle("Anchovy - Summer") 

ggsave(filename = here("Figs/LW_comparisons/anch_summer_LW_log.png"), 
       height = 10, width = 10)

# Sardine --------------------------------------------------
# Plot sardine in spring - All years
# Linear
ggplot(filter(lengths.sl, scientificName == "Sardinops sagax", season == "spring"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.spring, scientificName == "Sardinops sagax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  # xlim(50, 280) + ylim(5,250) +
  ggtitle("Sardine - Spring") 

ggsave(filename = here("Figs/LW_comparisons/sar_spring_LW.png"), 
       height = 10, width = 10)

# Log-log
ggplot(filter(lengths.sl, scientificName == "Sardinops sagax", season == "spring"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.spring, scientificName == "Sardinops sagax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  xlim(50, 280) + ylim(5,250) +
  coord_trans(x = "log", y = "log") +
  ggtitle("Sardine - Spring")

ggsave(filename = here("Figs/LW_comparisons/sar_spring_LW_log.png"), 
       height = 10, width = 10)

# Plot sardine in summer - All years
# Linear
ggplot(filter(lengths.sl, scientificName == "Sardinops sagax", season == "summer"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_point(data = filter(lengths.lm, scientificName == "Sardinops sagax", season == "summer"), 
             aes(standardLength_mm, weightg, colour = region)) +
  geom_line(data = filter(lw.df.summer, scientificName == "Sardinops sagax"),
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  xlim(10, 300) + ylim(5,370) +
  # coord_trans(x = "log", y = "log") +
  ggtitle("Sardine - Summer") 

ggsave(filename = here("Figs/LW_comparisons/sar_summer_LW.png"), 
       height = 10, width = 10)

# Log-log
ggplot(filter(lengths.sl, scientificName == "Sardinops sagax", season == "summer"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_point(data = filter(lengths.lm, scientificName == "Sardinops sagax", season == "summer"), 
             aes(standardLength_mm, weightg, colour = region)) +
  geom_line(data = filter(lw.df.summer, scientificName == "Sardinops sagax"),
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise) +
  xlim(50, 300) + ylim(5,370) +
  coord_trans(x = "log", y = "log") +
  ggtitle("Sardine - Summer") 

ggsave(filename = here("Figs/LW_comparisons/sar_summer_LW_log.png"), 
       height = 10, width = 10)

# Plot deviations from the weight model predictions ----------------------------
## Anchovy
### Spring
ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Engraulis mordax", season == "spring"), 
             aes(weightg, weightg.resid, colour = region),
             alpha = 0.2) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Anchovy-Spring") 

ggsave(filename = here("Figs/LW_comparisons/anch_spring_resids.png"), 
       height = 8, width = 12)

ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Engraulis mordax", season == "spring"), 
             aes(weightg, weightg.pctdiff, colour = region),
             alpha = 0.2) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Anchovy-Spring") + 
  ylim(-100, 100)

ggsave(filename = here("Figs/LW_comparisons/anch_spring_pctdiff.png"), 
       height = 8, width = 12)

### Summer
ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Engraulis mordax", season == "summer"), 
             aes(weightg, weightg.resid, colour = region), alpha = 0.2) + 
  geom_point(data = filter(lengths.lm, scientificName == "Engraulis mordax", season == "summer"), 
             aes(weightg, weightg.resid, colour = region), alpha = 0.2,
             show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Anchovy-Summer")

ggsave(filename = here("Figs/LW_comparisons/anch_summer_resids.png"), 
       height = 8, width = 12)

ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Engraulis mordax", season == "summer"), 
             aes(weightg, weightg.pctdiff, colour = region),
             alpha = 0.2) + 
  geom_point(data = filter(lengths.lm, scientificName == "Engraulis mordax", season == "summer"),
             aes(weightg, weightg.pctdiff, colour = region), alpha = 0.2,
             show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Anchovy-Summer") +
  ylim(-100, 100)

ggsave(filename = here("Figs/LW_comparisons/anch_summer_pctdiff.png"), 
       height = 8, width = 12)

## Sardine
### Spring
ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Sardinops sagax", season == "spring"), 
             aes(weightg, weightg.resid, colour = region),
             alpha = 0.2) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Sardine-Spring") 

ggsave(filename = here("Figs/LW_comparisons/sar_spring_resids.png"), 
       height = 8, width = 12)

ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Sardinops sagax", season == "spring"), 
             aes(weightg, weightg.pctdiff, colour = region),
             alpha = 0.2) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Sardine-Spring") + 
  ylim(-100, 100)

ggsave(filename = here("Figs/LW_comparisons/sar_spring_pctdiff.png"), 
       height = 8, width = 12)

### Summer
ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Sardinops sagax", season == "summer"), 
             aes(weightg, weightg.resid, colour = region), alpha = 0.2) + 
  geom_point(data = filter(lengths.lm, scientificName == "Sardinops sagax", season == "summer"), 
             aes(weightg, weightg.resid, colour = region), alpha = 0.2,
             show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Sardine-Summer")

ggsave(filename = here("Figs/LW_comparisons/sar_summer_resids.png"), 
       height = 8, width = 12)

ggplot() + 
  geom_point(data = filter(lengths.all, scientificName == "Sardinops sagax", season == "summer"), 
             aes(weightg, weightg.pctdiff, colour = region),
             alpha = 0.2) + 
  geom_point(data = filter(lengths.lm, scientificName == "Sardinops sagax", season == "summer"), 
             aes(weightg, weightg.pctdiff, colour = region), alpha = 0.2,
             show.legend = FALSE) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~cruise) + 
  labs(title = "Sardine-Summer") +
  ylim(-100, 100)

ggsave(filename = here("Figs/LW_comparisons/sar_summer_pctdiff.png"), 
       height = 8, width = 12)

