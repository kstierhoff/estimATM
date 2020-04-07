library(tidyverse)
library(lubridate)
library(here)
library(shadowtext)

theme_set(theme_bw())

load(here::here("Output/process_transects_output.Rdata"))

# Create land object for plotting
usa <- map_data("usa")
continents <- map_data("world") %>% filter(region %in% c("Canada", "Mexico"))
states <- map_data("state")

# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

locations <- read_csv(here("Data/Map/locations.csv")) %>% 
  filter(name %in% label.list)

# Plot initial transects
base.map <- ggplot() + 
  geom_polygon(data = continents, aes(x = long, y = lat, group = group), fill = "gray80", colour = "gray50") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "gray80", colour = "gray50") +
  geom_point(data = locations, aes(long, lat), size = 2, colour = "gray50") + 
  geom_shadowtext(data = locations, 
                  aes(long, lat, label = name), colour = "gray20", size = 2,
                  fontface = "bold", hjust = 0, nudge_x = 0.2, nudge_y = 0.05, angle = 25, bg.colour = "white") +
  coord_map(xlim = c(-129,-116), ylim = c(32, 50.5))

base.map +  geom_path(data = transects, aes(Longitude, Latitude, group = group, colour = Type)) + 
  geom_point(data = transects, aes(Longitude, Latitude, colour = Type))

# Make all adaptive transects compulsory
transects <- transects %>% 
  mutate(
    type2 = case_when(
      Type == "Adaptive" ~ "FSV1",
      Type == "Compulsory" ~ "FSV1",
      TRUE ~ Type)) %>% 
  filter(!group %in% paste(c(seq(110,128)),"Adaptive")) %>% 
  filter(Type != "Transit")

base.map +  geom_path(data = transects, aes(Longitude, Latitude, group = group, colour = type2)) + 
  geom_point(data = transects, aes(Longitude, Latitude, colour = type2))

# Make all even transects south of Cape Flattery FSV
# Make all odd transects south of Cape Flattery Saildrone
transects <- transects %>% 
  mutate(type2 = case_when(
    Type == "Nearshore" ~ "Nearshore",
    Type == "Transit" ~ "Transit",
    Transect %%2 == 1 ~ "FSV1",
    TRUE ~ "Saildrone")) %>% 
  mutate(type2 = case_when(
    type2 == "FSV1" & Transect <= 59 ~ "FSV2",
    TRUE ~ type2)) %>% 
  mutate(group2 = paste(Transect, type2))

# Create final map for proposal
base.map +  
  geom_path(data = transects, 
            aes(Longitude, Latitude, group = group, colour = type2)) +
  geom_point(data = transects, aes(Longitude, Latitude, colour = type2)) +
  scale_colour_manual("Vessel", values = c("FSV1" = "blue", "FSV2" = "cyan",
                                           "Nearshore" = "orange", "Saildrone" = "red",
                                           "Transit" = "magenta")) +
  scale_linetype_manual("Vessel", values = c("FSV1" = "solid", "FSV2" = "solid",
                                             "Nearshore" = "solid", "Saildrone" = "solid",
                                             "Transit" = "dotted")) + 
  xlab("Longitude") +
  ylab("Latitude")

ggsave(here("Figs/fig_survey_plan_uxs.png"),
       width = 6, height = 12)

write_csv(transects, here("Output/transects_uxs.csv"))
