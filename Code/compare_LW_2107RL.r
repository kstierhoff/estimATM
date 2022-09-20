library(tidyverse)
library(lubridate)
library(here)
library(atm)
library(surveyR)

# Get project name from directory
prj.name <- last(unlist(str_split(here::here(),"/")))

# Get all settings files
settings.files <- dir(here::here("Doc/settings"))

# Source survey settings file
prj.settings <- settings.files[str_detect(settings.files, paste0("settings_", prj.name, ".R"))]
source(here::here("Doc/settings", prj.settings))

# Plot RL data
load("C:/KLS/CODE/Github/estimATM/2107RL/Data/Trawl/all_trawl_data-final.Rdata")

ggplot(lengths, aes(totalLength_mm, weightg)) + 
  geom_point() + 
  facet_wrap(~scientificName, scales = "free")

# Plot LM data
load(here("Output/purse_seine_specimens.Rdata"))

# Plot LM data
ggplot(lm.specimens, aes(totalLength_mm, weightg)) + 
  geom_point() + 
  facet_wrap(~scientificName, scales = "free")

# Plot LBC data
ggplot(lbc.specimens, aes(totalLength_mm, weightg)) + 
  geom_point() + 
  facet_wrap(~scientificName, scales = "free")

# Get max TL for plotting L/W models
L.max.ns <- select(lengths, scientificName, totalLength_mm) %>%
  # Add LBC data
  filter(scientificName %in%  cps.spp) %>%
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

# Plot LW data from specimens --------------------------------------------------
# Data frame for storing results
lw.df.ns <- data.frame()

# Generate length/weight curves
for (i in unique(L.max.ns$scientificName)) {
  # Create a length vector for each species
  totalLength_mm <- seq(0, L.max.ns$max.TL[L.max.ns$scientificName == i])
  
  # Calculate weights from lengths
  weightg <- estimate_weight(i, totalLength_mm, season = tolower(survey.season))
  
  # Combine results
  lw.df.ns <- bind_rows(lw.df.ns, data.frame(scientificName = i, weightg, totalLength_mm))
}

# Convert lengths for plotting
lw.df.ns <- lw.df.ns %>% 
  mutate(
    forkLength_mm     = convert_length(scientificName, totalLength_mm, "TL", "FL"),
    standardLength_mm = convert_length(scientificName, totalLength_mm, "TL", "SL")
  )

# Compare lengths and weights between LM/LBC/RL
lw.plot.comp.lm <- ggplot() +
  # Plot L/W data for current survey
  geom_point(data = lengths,
             aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  geom_point(data = lm.specimens,
             aes(totalLength_mm, weightg), colour = "blue", alpha = 0.75) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
            alpha = 0.75, colour = "gray20", linetype = 'dashed') +
  # Facet by species
  facet_wrap(~scientificName, scales = "free") +
  # scale_colour_manual(name = "Sex", values = c("Female" = "pink", "Male" = "lightblue",
  #                                              "Unknown" = "#FFC926", "Not Sexed" = "purple")) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

lw.plot.comp.lbc <- ggplot() +
  # Plot L/W data for current survey
  geom_point(data = lengths,
             aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  geom_point(data = lbc.specimens,
             aes(totalLength_mm, weightg), colour = "red", alpha = 0.75) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
            alpha = 0.75, colour = "gray20", linetype = 'dashed') +
  # Facet by species
  facet_wrap(~scientificName, scales = "free") +
  # scale_colour_manual(name = "Sex", values = c("Female" = "pink", "Male" = "lightblue",
  #                                              "Unknown" = "#FFC926", "Not Sexed" = "purple")) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

lw.plot.comp.all <- ggplot() +
  # Plot L/W data for current survey
  geom_point(data = filter(lengths, missing.length == FALSE),
             aes(totalLength_mm, weightg), colour = "gray50", alpha = 0.75) +
  geom_point(data = lbc.specimens,
             aes(totalLength_mm, weightg), colour = "red", alpha = 0.75) +
  geom_point(data = lm.specimens,
             aes(totalLength_mm, weightg), colour = "blue", alpha = 0.75) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.ns, aes(totalLength_mm, weightg), 
            alpha = 0.75, colour = "gray20", linetype = 'dashed') +
  # Facet by species
  facet_wrap(~scientificName, scales = "free") +
  # scale_colour_manual(name = "Sex", values = c("Female" = "pink", "Male" = "lightblue",
  #                                              "Unknown" = "#FFC926", "Not Sexed" = "purple")) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "bold.italic"))

ggsave(lw.plot.comp.all, filename = here::here("Figs/fig_LW_comp_all.png"),
       width = 15, height = 10)
ggsave(lw.plot.comp.all, filename = here::here("Figs/fig_LW_comp_all.pdf"),
       width = 15, height = 10)

saveRDS(lengths, file = here("Output/lw_comp_2021_rl.rds"))
saveRDS(lm.specimens, file = here("Output/lw_comp_2021_lm.rds"))
saveRDS(lbc.specimens, file = here("Output/lw_comp_2021_lbc.rds"))
