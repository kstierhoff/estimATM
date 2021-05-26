library(tidyverse)
library(sf)
library(mapview)
library(scatterpie)
library(lubridate)
library(here)
library(atm)
library(surveyR)
library(cowplot)

# Load settings
source(here("Doc/settings/settings_2103RL.R"))

# Load base map
load("E:/CODE/R_packages/estimATM/2103RL/Data/Map/basemap.Rdata")

# Load catch data
load(here("Output/catch_info.Rdata"))
# Load backscatter data
load("E:/CODE/R_packages/estimATM/2103RL/Data/Backscatter/nasc_all.Rdata")

nasc <- project_df(nasc, to = 3310)

nasc.plot <- nasc %>%
  select(filename, transect.name, transect, int, dist_m, datetime, lat, long, cps.nasc) %>% 
  group_by(filename, transect.name, transect, int) %>% 
  summarise(
    lat  = lat[1],
    long = long[1],
    NASC = mean(cps.nasc),
    label = paste0('Transect: ', transect[1], "; ",
                   'Distance: ', round(min(dist_m)), "-", round(max(dist_m)), ' m'),
    popup = paste0('<b>Transect: </b>', transect[1], '<br/>',
                   '<b>Time: </b>', min(datetime), " - ", max(datetime), ' UTC<br/>',
                   '<b>Distance: </b>', round(min(dist_m)), "-", round(max(dist_m)), ' m<br/>',
                   '<b>NASC: </b>', round(mean(NASC)), ' m<sup>2</sup> nmi<sup>-2</sup>')) %>% 
  # Create bins for defining point size in NASC plots
  mutate(bin       = cut(NASC, nasc.breaks, include.lowest = TRUE),
         bin.level =  as.numeric(bin)) %>% 
  # st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  ungroup() %>% 
  project_df(to = crs.proj)

# Select plot levels for backscatter data
nasc.levels.all <- sort(unique(nasc.plot$bin.level))
nasc.labels.all <- nasc.labels[nasc.levels.all]
nasc.sizes.all  <- nasc.sizes[nasc.levels.all]
nasc.colors.all <- nasc.colors[nasc.levels.all]

# Read transects
transects <- st_read(here("Output/planned_transects.shp")) %>% 
  filter(Region == "S. CA Bight")

# Read LBC data
lbc.nav <- read_csv(here("Data/Nav/2103LBC.gps.csv")) %>% 
  rename(lat = Latitude, long = Longitude) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING")

lbc.sets <- read_csv(here("Data/Seine/lbc_sets.csv")) %>% 
  project_df(to = 3310)

lbc.logs <- read_csv(here("Data/Seine/lbc_log.csv")) %>% 
  project_df(to = 3310)

lbc.catch <- read_csv(here("Data/Seine/lbc_samples.csv")) %>% 
  group_by(set, species) %>% 
  summarise(totalCount = sum(count)) %>% 
  left_join(select(lbc.sets, set, X, Y)) %>% 
  tidyr::spread(species, totalCount) %>% 
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(AllCPS = rowSums(select(., -(set:Y)))) %>% 
  replace(. == 0, 0.0000001) %>% 
  arrange(desc(X))

write_csv(lbc.catch, file = here("Output/lbc_catch_2103RL.csv"))

# Set map bounds to SCB
# Use nav data to resize map to survey progress
map.bounds <- transects %>%
  st_transform(crs = 3310) %>%
  st_bbox()  


# Calculate pie radius based on latitude range
pie.radius.ns <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*pie.scale)
lbc.catch$radius    <- pie.radius.ns

# # Calculate pie radius of each pie, based on All CPS landings
# if (scale.pies) {
#   lbc.catch$radius    <- pie.radius.ns*lbc.catch$bin
# } else {
#   lbc.catch$radius    <- pie.radius.ns
# }

# Map Lasker data
base.map + 
  geom_sf(data = transects, colour = "gray50", alpha = 0.5, size = 1, linetype = "dashed") +
  # geom_point(data = nasc.plot, aes(X, Y, size = cps.nasc, colour = cps.nasc), alpha = 0.5) + 
  geom_point(data = nasc.plot, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all, labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  coord_sf(crs = 3310, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) 

# Map LBC data
a = base.map + 
  geom_sf(data = transects, colour = "gray50", alpha = 0.5, size = 1, linetype = "dashed") +
  geom_sf(data = lbc.nav, colour = "gray50") +
  geom_point(data = lbc.sets, aes(X, Y), shape = 23, size = 3, fill = "yellow") +
  geom_point(data = lbc.logs, aes(X, Y, size = estimate, colour = species)) +
  scale_colour_manual(name = "Species",
                      values = c("Sardine" = '#FF0000',
                                 "Anchovy" = '#00CD66')) +
  scale_size_continuous(name = "Estimated size (tons)") +
  labs(title = "Captain's Log",
       subtitle = "Observations (tons per species); yellow diamonds are set locations") +
  coord_sf(crs = 3310, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) 

b = base.map + 
  geom_sf(data = transects, colour = "gray50", alpha = 0.5, size = 1) +
  geom_point(data = nasc.plot, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all, labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  geom_point(data = lbc.sets, aes(X, Y), shape = 23, size = 3, fill = "white") +
  guides(colour = guide_legend(), size = guide_legend()) +
  labs(title = "Acoustic backscatter",
       subtitle = "Summarized over 2 km intervals; white diamonds are set locations") +
  # geom_point(data = lbc.logs, aes(X, Y, size = estimate, colour = species)) +
  coord_sf(crs = 3310, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) 


combo.plot <- plot_grid(a, b)

ggsave(combo.plot, filename = here("Figs/fig_LBC_plot_2103RL.png"),
       height = 8, width = 18)

library(ggnewscale)

# base.map + 
#   # Plot purse seine pies
#   scatterpie::geom_scatterpie(data = lbc.catch, aes(X, Y, group = set, r = r),
#                               cols = c("anchovy", "mackerel", "sardine"),
#                               color = 'black', alpha = 0.8) +
#   # Configure trawl scale
#   scale_fill_manual(name = 'Species',
#                     labels = c("Anchovy", "Mackerel", "Sardine"),
#                     values = c(anchovy.color, pac.mack.color, sardine.color)) +
#   coord_sf(crs = 3310, # CA Albers Equal Area Projection
#            xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
#            ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) 

c = base.map + 
  geom_sf(data = transects, colour = "gray50", alpha = 0.5, size = 1, linetype = "dashed") +
  geom_sf(data = lbc.nav, colour = "gray50") +
  # Plot purse seine pies
  geom_scatterpie(data = lbc.catch, aes(X, Y, group = set, r = r*1.25),
                              cols = c("Anchovy", "PacMack", "Sardine"),
                              color = 'black', alpha = 0.75) +
  # Plot trawl pies
  geom_scatterpie(data = haul.pie, aes(X, Y, group = haul, r = pie.radius.ns),
                  cols = c("Anchovy", "PacMack", "Sardine"),
                  color = 'black', alpha = 0.75) +
  # geom_scatterpie(data = haul.pie, aes(X, Y, group = haul, r = r*0.4),
  #                 cols = c("Anchovy", "PacMack", "Sardine"),
  #                 color = 'blue', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "Mackerel", "Sardine"),
                    values = c(anchovy.color, pac.mack.color, sardine.color)) +
  # geom_point(data = lbc.sets, aes(X, Y), shape = 23, size = 3, fill = "yellow") +
  new_scale_fill() + 
  geom_point(data = lbc.logs, aes(X, Y, size = estimate, fill = species), colour = "white", shape = 21) +
  scale_fill_manual(values = c("Sardine" = '#FF0000',
                               "Anchovy" = '#00CD66')) +
  geom_point(data = filter(lbc.sets, successful == "No"), 
             aes(X, Y), shape = 25, size = 4, fill = "yellow") + 
  # guides(fill = guide_legend(), size = guide_legend()) +
  labs(title = "Captain's Log",
       subtitle = "Observations (tons per species); pies are species proportions for Lasker (blue) and LBC (black)") +
  coord_sf(crs = 3310, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) 

combo.plot2 <- plot_grid(c, b)

ggsave(combo.plot2, filename = here("Figs/fig_LBC_plot_2103RL_2.png"),
       height = 8, width = 18)

