library(tidyverse)
library(lubridate)
library(here)
library(atm)
library(surveyR)

# Load data
load("C:/KLS/CODE/Github/estimATM/2107RL/Data/Trawl/trawl_data.Rdata")

lengths.2207RL <- readRDS("C:/KLS/CODE/Github/estimATM/2207RL/Output/lw_data_checkTrawls.rds") %>% 
  mutate(season = "summer") 

load("C:/KLS/CODE/Github/estimATM/2207RL/Output/haul_info.Rdata")

haul.2207RL <- haul %>% 
  mutate(region = case_when(
    startLatDecimal > 34.46 ~ "Central",
    TRUE ~ "South")) %>% 
  filter(!is.na(region))

lengths.2207RL <- lengths.2207RL %>% 
  left_join(select(haul.2207RL, cruise, ship, haul, region))

# Define species to be analysed
cps.spp <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                        "Scomber japonicus","Trachurus symmetricus")

lengths.all <- lengths.all %>% 
  left_join(spp.codes) %>% 
  filter(scientificName %in% cps.spp)

# List regions
region.names <- c("North","Central","South")

# Classify hauls by season (spring or summer)
haul.all <- haul.all %>% 
  mutate(season = case_when(
    month(equilibriumTime) < 6 ~ "spring",
    TRUE ~ "summer")) %>% 
  mutate(region = cut(startLatDecimal, c(0, 34.46, 40.50, 60), labels = FALSE)) %>% 
  filter(!is.na(region))

# Replace regions with labels
haul.all$region <- region.names[haul.all$region]

# Classify specimens as spring or summer using hauls
lengths.all <- lengths.all %>% 
  left_join(select(haul.all, cruise, ship, haul, season, region)) %>% 
  bind_rows(lengths.2207RL) 

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

# # Check lw models
# ggplot() +
#   geom_line(data = lw.df.spring, aes(totalLength_mm, weightg), colour = "green") +
#   geom_line(data = lw.df.summer, aes(totalLength_mm, weightg), colour = "blue") +
#   facet_wrap(~scientificName, scales = "free")
  
# Check lengths
# ggplot(lengths.all, aes(totalLength_mm, weightg, colour = season)) +
#   geom_point() +
#   facet_wrap(~scientificName, scales = "free")

# lw.plot <- ggplot() +
#   # Plot L/W data for current survey
#   geom_point(data = filter(rl.specimens, missing.length == FALSE),
#              aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
#   geom_point(data = lbc.specimens,
#              aes(totalLength_mm, weightg), colour = "red", alpha = 0.75) +
#   geom_point(data = lm.specimens,
#              aes(totalLength_mm, weightg), colour = "blue", alpha = 0.75) +
#   # Plot seasonal length models for each species
#   geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
#             alpha = 0.75, colour = "gray20", linetype = 'dashed') +
#   # Facet by species
#   facet_wrap(~scientificName, scales = "free") +
#   # scale_colour_manual(name = "Sex", values = c("Female" = "pink", "Male" = "lightblue",
#   #                                              "Unknown" = "#FFC926", "Not Sexed" = "purple")) +
#   # Format plot
#   xlab("Total length (mm)") + ylab("Mass (g)") +
#   theme_bw() +
#   theme(strip.background.x = element_blank(),
#         strip.text.x = element_text(face = "bold.italic"))

spp.sl <- c("Engraulis mordax","Sardinops sagax")
spp.fl <- c("Scomber japonicus","Trachurus symmetricus","Clupea pallasii")

lengths.sl <- filter(lengths.all, scientificName %in% spp.sl, !is.na(standardLength_mm))
lengths.fl <- filter(lengths.all, scientificName %in% spp.fl, !is.na(forkLength_mm))

# Plot species with native lengths == SL
ggplot(lengths.sl, aes(standardLength_mm, weightg, colour = season)) + 
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.spring, scientificName %in% spp.sl),
            aes(standardLength_mm, weightg),
            linetype = "dashed", colour = "pink") +
  geom_line(data = filter(lw.df.summer, scientificName %in% spp.sl),
            aes(standardLength_mm, weightg),
            linetype = "dashed", colour = "blue") +
  facet_wrap(~scientificName, scales = "free") + 
  theme_bw()

# Plot species with native lengths == FL
ggplot(lengths.fl, aes(forkLength_mm, weightg, colour = season)) + 
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.spring, scientificName %in% spp.fl),
            aes(forkLength_mm, weightg),
            linetype = "dashed", colour = "pink") +
  geom_line(data = filter(lw.df.summer, scientificName %in% spp.fl),
            aes(forkLength_mm, weightg),
            linetype = "dashed", colour = "blue") +
  facet_wrap(~scientificName, scales = "free") + 
  theme_bw()

# Anchovy ------------------------------------------------------------------
# Plot anchovy in spring - All years
ggplot(filter(lengths.all, scientificName == "Engraulis mordax", season == "spring"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
    geom_line(data = filter(lw.df.spring, scientificName == "Engraulis mordax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise, scales = "fixed") +
  coord_trans(x ="log", y="log") +
  ggtitle("Anchovy - Spring") +
  theme_bw()

ggsave(filename = here("Figs/LW_comparisons/all_years_anch_spring.png"), 
       height = 10, width = 10)

# Plot anchovy in summer - All years
ggplot(filter(lengths.all, scientificName == "Engraulis mordax", season == "summer"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.summer, scientificName == "Engraulis mordax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise, scales = "fixed") +
  coord_trans(x ="log", y="log") +
  ggtitle("Anchovy - Summer") +
  theme_bw()

ggsave(filename = here("Figs/LW_comparisons/all_years_anch_summer.png"), 
       height = 10, width = 10)

# Sardine --------------------------------------------------
# Plot sardine in spring - All years
ggplot(filter(lengths.all, scientificName == "Sardinops sagax", season == "spring"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.spring, scientificName == "Sardinops sagax"), 
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise, scales = "fixed") +
  coord_trans(x ="log", y="log") +
  ggtitle("Sardine - Spring") +
  theme_bw()

ggsave(filename = here("Figs/LW_comparisons/all_years_sar_spring.png"), 
       height = 10, width = 10)

# Plot sardine in summer - All years
ggplot(filter(lengths.sl, scientificName == "Sardinops sagax", season == "summer"), 
       aes(standardLength_mm, weightg, colour = region)) +
  geom_point(alpha = 0.5) + 
  geom_line(data = filter(lw.df.summer, scientificName == "Sardinops sagax"),
            aes(standardLength_mm, weightg), linetype = "dashed", colour = "black") +
  facet_wrap(~cruise, scales = "fixed") +
  coord_trans(x ="log", y="log") +
  ggtitle("Sardine - Summer") +
  theme_bw()

ggsave(filename = here("Figs/LW_comparisons/all_years_sar_summer.png"), 
       height = 10, width = 10)

# ggplot(lengths.sl) + 
#   geom_histogram(aes(standardLength_mm, fill = region), alpha = 0.5) + 
#   facet_wrap(~scientificName, scales = "free") + 
#   theme_bw()
# 
# 
# ggplot(lengths.fl) + 
#   geom_histogram(aes(forkLength_mm, fill = region), alpha = 0.5) + 
#   facet_wrap(~scientificName, scales = "free") + 
#   theme_bw()
# 
# ggsave(filename = here("Figs/LW_comparisons/lengths_by_season.png"), 
#        height = 10, width = 10)
