library(tidyverse)
library(readr)
library(lubridate)
library(here)
library(plotly)
library(sf)
library(mapview)
library(atm)
library(surveyR)

# Get project name from directory
prj.name <- last(unlist(str_split(here(),"/")))

# Get all settings files
settings.files <- dir(here("Doc/settings"))

# Source survey settings file
prj.settings <- settings.files[str_detect(settings.files, paste0("settings_", prj.name, ".R"))]
source(here("Doc/settings", prj.settings))

# Source following the section entitled estimateNearshore in estimateBiomass.

# Import set data ----------------------------------------------------
lm.sets <- read_csv(here("Data/Seine/lm_sets.csv"), lazy = FALSE) %>% 
  mutate(date = mdy(date),
         vessel_name = "Lisa Marie",
         vessel.name = "LM",
         key.set = paste(vessel.name, date, set))

# lbc.sets <- read_csv(here("Data/Seine/lbc_sets.csv"), lazy = FALSE) %>%
#   mutate(date = date(mdy_hm(datetime)),
#          vessel_name = "Long Beach Carnage",
#          vessel.name = "LBC",
#          key.set = paste(vessel_name, date, set))

# save(lm.sets, lbc.sets, file = here("Output/purse_seine_sets.Rdata"))

# Import specimen data ----------------------------------------------------
lm.specimens <- read_csv(here("Data/Seine/lm_specimens.csv"), lazy = FALSE) %>% 
  mutate(date = mdy(date),
         vessel.name = "Lisa Marie",
         vessel.name = "LM",
         key.set = paste(vessel.name, date, set),
         label = paste("Date:", date, "Set:", set, "Fish num:", fish_number),
         scientificName = case_when(
           species_name == "Pacific Herring" ~ "Clupea pallasii",
           species_name == "Pacific Sardine" ~ "Sardinops sagax",
           species_name == "Chub Mackerel" ~ "Scomber japonicus",
           species_name == "Jack Mackerel" ~ "Trachurus symmetricus",
           species_name == "Northern Anchovy" ~ "Engraulis mordax",
           species_name == "Jacksmelt" ~ "Atherinopsis californiensis",
           species_name == "Whitebait Smelt" ~ "Allosmerus elongatus",
           TRUE ~ NA_character_),
         missing.weight = case_when(is.na(weightg)   ~ T, TRUE ~ FALSE),
         missing.length = case_when(is.na(fish_length) ~ T, TRUE ~ FALSE)) %>% 
  mutate(
    forkLength_mm = case_when(
      length_type == "Fork length" ~ fish_length,
      TRUE ~ NA_real_),
    standardLength_mm = case_when(
      length_type == "Standard length" ~ fish_length,
      TRUE ~ NA_real_)) %>% 
  filter(scientificName %in% cps.spp, !is.na(set)) %>% 
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
        convert_length("Trachurus symmetricus", .$forkLength_mm, "FL", "TL"))) %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
      TRUE ~ totalLength_mm),
    K = round((weightg/totalLength_mm*10^3)*100))

# lbc.specimens <- read_csv(here("Data/Seine/lbc_catch.csv")) %>%
#   left_join(select(lbc.sets, date, set, key.set)) %>%
#   mutate(vessel.name = "LM",
#          label = paste("Date:", date, "Set:", set, "Fish num:", specimen_number),
#          totalLength_mm = case_when(
#            scientificName == "Clupea pallasii" ~
#              convert_length("Clupea pallasii", .$forkLength_mm, "FL", "TL"),
#            scientificName == "Engraulis mordax" ~
#              convert_length("Engraulis mordax", .$standardLength_mm, "SL", "TL"),
#            scientificName == "Sardinops sagax" ~
#              convert_length("Sardinops sagax", .$standardLength_mm, "SL", "TL"),
#            scientificName == "Scomber japonicus" ~
#              convert_length("Scomber japonicus", .$forkLength_mm, "FL", "TL"),
#            scientificName == "Trachurus symmetricus" ~
#              convert_length("Trachurus symmetricus", .$forkLength_mm, "FL", "TL"))) %>%
#   filter(scientificName %in% cps.spp, !is.na(set)) %>%
#   # Estimate missing weights from lengths -------------------------------------------------------
# mutate(
#   weightg = case_when(
#     is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
#     TRUE  ~ weightg),
#   totalLength_mm = case_when(
#     is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
#     TRUE ~ totalLength_mm),
#   K = round((weightg/totalLength_mm*10^3)*100))

# save(lm.specimens, lbc.specimens, file = here("Output/purse_seine_specimens.Rdata"))

# Summarize specimen data ------------------------------------------------
lm.spec.summ <- lm.specimens %>%
  group_by(key.set, scientificName) %>% 
  summarise(totalWeight = sum(weightg, na.rm = TRUE),
            totalCount  = n()) %>% 
  left_join(select(lm.sets, key.set, lat, long)) %>% 
  filter(!is.na(lat)) 

# lbc.spec.summ <- lbc.specimens %>%
#   group_by(key.set, scientificName) %>% 
#   summarise(totalWeight = sum(weightg, na.rm = TRUE),
#             totalCount  = n()) %>% 
#   left_join(select(lbc.sets, key.set, lat, long)) %>% 
#   filter(!is.na(lat)) 

# Make specimen summaries spatial -----------------------------------------
lm.spec.summ.sf <- lm.spec.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

# lbc.spec.summ.sf <- lbc.spec.summ %>% 
#   st_as_sf(coords = c("long","lat"), crs = 4326)

# Summarise catch by weight -----------------------------------------------
set.summ.wt <- lm.spec.summ %>% 
  # bind_rows(lbc.spec.summ) %>% 
  tidyr::spread(scientificName, totalWeight) 

# Add species with zero total weight
if (!has_name(set.summ.wt, "Engraulis mordax"))      {set.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(set.summ.wt, "Sardinops sagax"))       {set.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(set.summ.wt, "Scomber japonicus"))     {set.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(set.summ.wt, "Trachurus symmetricus")) {set.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(set.summ.wt, "Clupea pallasii"))       {set.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(set.summ.wt, "Atherinopsis californiensis")) {set.summ.wt$`Atherinopsis californiensis` <- 0}

# Calculate total weight of all CPS species
set.summ.wt <- set.summ.wt %>%  
  replace(is.na(.), 0) %>% 
  ungroup() %>% 
  mutate(AllCPS = rowSums(select(., -(key.set:long)))) %>%
  # mutate(AllCPS = rowSums(select(., -key.set, -totalCount, -lat, -long))) %>%
  # mutate(AllCPS = rowSums(.[, 3:ncol(.)])) %>%
  rename("Jacksmelt"  = "Atherinopsis californiensis",
         "PacHerring" = "Clupea pallasii",
         "Anchovy"    = "Engraulis mordax",
         "Sardine"    = "Sardinops sagax",
         "PacMack"    = "Scomber japonicus",
         "JackMack"   = "Trachurus symmetricus") 

set.pie <- set.summ.wt %>% 
  select(key.set, long, lat, Anchovy, JackMack, 
         Jacksmelt, PacHerring, PacMack, Sardine, AllCPS) %>% 
  atm::project_df(to = 3310) %>% 
  mutate(
    label = paste("Set", key.set),
    popup = paste('<b>Set:', key.set, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'All CPS:', AllCPS, 'kg'))

# Load nearshore backscatter data -----------------------------------------
# load(here("Output/cps_nasc_prop_ns.Rdata"))

# Summarize transects -----------------------------------------------------
# Summarize nasc data
nasc.summ.ns <- nasc.nearshore %>% 
  group_by(vessel.name, transect.name, transect) %>% 
  summarise(
    start     = min(datetime),
    end       = max(datetime),
    duration  = difftime(end, start, units = "hours"),
    n_int     = length(Interval),
    distance  = length(Interval)*nasc.interval/1852,
    lat       = lat[which.min(long)],
    long      = long[which.min(long)],
    mean_nasc = mean(cps.nasc)) %>% 
  arrange(vessel.name, start)

save(nasc.summ.ns, file = here("Output/nasc_summ_tx_ns.Rdata"))

# Summarize nasc for plotting
nasc.plot.ns <- nasc.nearshore %>%
  select(filename, transect, transect.name, int, lat, long, cps.nasc) %>%
  group_by(filename, transect, transect.name, int) %>%
  summarise(
    lat  = lat[1],
    long = long[1],
    NASC = mean(cps.nasc)) %>%
  # Create bins for defining point size in NASC plots%>%
  mutate(bin       = cut(NASC, nasc.breaks, include.lowest = TRUE),
         bin.level =  as.numeric(bin)) %>%
  ungroup() %>%
  project_df(to = crs.proj)

nasc.paths.ns <- nasc.plot.ns %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  group_by(transect.name) %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") %>%
  ungroup()


# Read GPX file
lm.route <- tmaptools::read_GPX(here::here("Data/Nav", "lm_tracks_20220825.gpx"))

# Create data frame of waypoints
lm.points <- lm.route$track_points %>% 
  atm::project_sf(crs = 4326) %>% 
  select(long = X, lat = Y, name, time) %>% 
  filter(lat > 37) %>% 
  arrange(time) %>% 
  project_df(to = crs.proj)

lm.points.sf <- lm.points %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog)

lm.path <- lm.points.sf %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_transform(crs.proj)

# Load Lasker nav
load("C:/KLS/CODE/Github/estimATM/2207RL/Data/Nav/nav_data.Rdata")

# Plot Lisa Marie data ----------------------------------------------------
# Use nav data to resize map to survey progress
map.bounds.ns <- nav.paths.sf %>%
    st_transform(crs = 3310) %>%
  bind_rows(lm.path) %>% 

  st_bbox()

# map.bounds.ns <- nasc.paths.ns %>%
#   filter(str_detect(transect.name, "LM")) %>% 
#   st_transform(crs = 3310) %>%
#   st_bbox()

# Calculate pie radius based on latitude range
pie.radius.ns <- as.numeric(abs(map.bounds.ns$ymin - map.bounds.ns$ymax)*pie.scale)

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.pie$r    <- pie.radius.ns*set.pie$bin
} else {
  set.pie$r    <- pie.radius.ns
}

# Filter for empty trawls
set.zero    <- filter(set.pie, AllCPS == 0)

# Replace zeros with minimally small value for scatterpie plotting
set.pos <- filter(set.pie, AllCPS > 0) %>% 
  replace(. == 0, 0.0000001) %>% 
  arrange(desc(X))

# Save output
save(set.pie, set.zero, set.pos, 
     file = here("Output/purse_set_pies.Rdata"))

# Select plot levels for backscatter data
# nasc.plot.ns.sub <- filter(nasc.plot.ns, str_detect(transect.name, "LM"))
# 
# nasc.levels.all <- sort(unique(nasc.plot.ns.sub$bin.level))
# nasc.labels.all <- nasc.labels[nasc.levels.all]
# nasc.sizes.all  <- nasc.sizes[nasc.levels.all]
# nasc.colors.all <- nasc.colors[nasc.levels.all]

load("C:/KLS/CODE/Github/estimATM/2207RL/Data/Map/basemap.Rdata")

load(here("Output/trawl_pie_plotBio.Rdata"))

set.pies <- base.map + 
  # Plot NASC data
  geom_path(data = lm.points, aes(X, Y)) +
  geom_point(data = project_df(lm.sets, to = crs.proj), 
             aes(X, Y),
             shape = 21, fill = "black", colour = "white", size = 3) +
  # geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name)) +
  # geom_sf(data = lm.path) %>%
  # Plot purse seine pies
  scatterpie::geom_scatterpie(data = filter(set.pos, str_detect(key.set, "LM")), 
                              aes(X, Y, group = key.set, r = r*1.5),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # # Plot haul pies
  # scatterpie::geom_scatterpie(data = haul.pos, 
  #                             aes(X, Y, group = haul, r = r),
  #                             cols = c("Anchovy", "JackMack", "Jacksmelt",
  #                                      "PacHerring", "PacMack", "Sardine"),
  #                             color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, sardine.color)) +
  geom_point(data = set.zero, aes(X, Y)) +
  # ggtitle("CPS Proportions in Purse Seines") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"]*1.4, map.bounds.ns["xmax"]*0.8), 
           ylim = c(map.bounds.ns["ymin"]*0.9, map.bounds.ns["ymax"]))

ggsave(set.pies, filename = here("Figs/fig_seine_proportion_set_wt_LisaMarie.png"),
       height = 10, width = 6)

haul.pies <- base.map + 
  # # Plot NASC data
  geom_path(data = project_df(nav, to = crs.proj), aes(X, Y)) +
  # geom_point(data = project_df(lm.sets, to = crs.proj), 
  #            aes(X, Y),
  #            shape = 21, fill = "black", colour = "white", size = 3) +
  # geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name)) +
  # geom_sf(data = st_transform(nav.paths.sf, crs.proj)) %>%
  # Plot purse seine pies
  # scatterpie::geom_scatterpie(data = filter(set.pos, str_detect(key.set, "LM")), 
  #                             aes(X, Y, group = key.set, r = r*1.5),
  #                             cols = c("Anchovy", "JackMack", "Jacksmelt",
  #                                      "PacHerring", "PacMack", "Sardine"),
  #                             color = 'black', alpha = 0.8) +
  # Plot haul pies
  scatterpie::geom_scatterpie(data = haul.pos, 
                              aes(X, Y, group = haul, r = r),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, sardine.color)) +
  geom_point(data = haul.zero, aes(X, Y)) +
  # ggtitle("CPS Proportions in Purse Seines") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"]*1.4, map.bounds.ns["xmax"]*0.8), 
           ylim = c(map.bounds.ns["ymin"]*0.9, map.bounds.ns["ymax"]))

set.haul.combo <- cowplot::plot_grid(set.pies, haul.pies, align = "hv")

ggsave(set.haul.combo, filename = here("Figs/fig_seine_proportion_set_wt_LM-RL.png"),
       height = 10, width = 10)

# Map backscatter
nasc.map.ns.lm <- base.map +
  # Plot transects data
  geom_sf(data = filter(transects.sf, Type == "Nearshore"), 
          size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # Plot NASC data
  geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name),
            colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.plot.ns.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Plot title
  # ggtitle("CPS Backscatter-Lisa Marie") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"]*1.4, map.bounds.ns["xmax"]*0.8), 
           ylim = c(map.bounds.ns["ymin"]*0.9, map.bounds.ns["ymax"]))

ggsave(nasc.map.ns.lm, filename = here("Figs/fig_backscatter_cps_LisaMarie.png"),
       height = 10, width = 6)

nasc.set.wt.combo <- plot_grid(nasc.map.ns.lm, set.pies, nrow = 1,
                               labels = c("a)", "b)"))

# Save combo map
ggsave(nasc.set.wt.combo, filename = here("Figs/fig_nasc_seine_proportion_set_wt_LisaMarie.png"),
       height = 7, width = 6.5)

# Plot Long Beach Carnage data ----------------------------------------------------
# Use nav data to resize map to survey progress
map.bounds.ns <- nasc.paths.ns %>%
  filter(str_detect(transect.name, "LBC")) %>% 
  st_transform(crs = 3310) %>%
  st_bbox()

# Select plot levels for backscatter data
nasc.plot.ns.sub <- filter(nasc.plot.ns, str_detect(transect.name, "LBC"))

nasc.levels.all <- sort(unique(nasc.plot.ns.sub$bin.level))
nasc.labels.all <- nasc.labels[nasc.levels.all]
nasc.sizes.all  <- nasc.sizes[nasc.levels.all]
nasc.colors.all <- nasc.colors[nasc.levels.all]

# Calculate pie radius based on latitude range
pie.radius.ns <- as.numeric(abs(map.bounds.ns$ymin - map.bounds.ns$ymax)*pie.scale)

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.pie$r    <- pie.radius.ns*set.pie$bin
} else {
  set.pie$r    <- pie.radius.ns
}

# Filter for empty trawls
set.zero    <- filter(set.pie, AllCPS == 0)

# Replace zeros with minimally small value for scatterpie plotting
set.pos <- filter(set.pie, AllCPS > 0) %>% 
  replace(. == 0, 0.0000001) %>% 
  arrange(desc(X))

set.pies <- base.map +
  # Plot NASC data
  geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name)) +
  # Plot purse seine pies
  scatterpie::geom_scatterpie(data = filter(set.pos, str_detect(key.set, "Long Beach Carnage")), 
                              aes(X, Y, group = key.set, r = r*2.5),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # scatterpie::geom_scatterpie(data = cluster.pie, aes(X, Y, group = cluster, r = r*0.5),
  #                             cols = c("Anchovy", "JackMack", "Jacksmelt",
  #                                      "PacHerring", "PacMack", "Sardine"),
  #                             color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, sardine.color)) +
  geom_point(data = set.zero, aes(X, Y)) +
  # ggtitle("CPS Proportions in Purse Seines") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"], map.bounds.ns["xmax"]*1.1), 
           ylim = c(map.bounds.ns["ymin"], map.bounds.ns["ymax"]*0.95))

# set.pos.sf <- set.pos %>% st_as_sf(coords = c("long","lat"), crs = 4326)

ggsave(set.pies, filename = here("Figs/fig_seine_proportion_set_wt_LongBeachCarnage.png"),
       height = 6, width = 10)

# Map backscatter
nasc.map.ns.lbc <- base.map +
  # Plot transects data
  geom_sf(data = filter(transects.sf, Type == "Nearshore"), 
          size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # Plot NASC data
  geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name),
            colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot NASC data
  geom_point(data = nasc.plot.ns.sub, aes(X, Y, size = bin, fill = bin), 
             shape = 21, alpha = 0.75) +
  # Configure size and colour scales
  scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.sizes.all,labels = nasc.labels.all) +
  scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
                    values = nasc.colors.all,labels = nasc.labels.all) +
  # Configure legend guides
  guides(fill = guide_legend(), size = guide_legend()) +
  # Plot title
  # ggtitle("CPS Backscatter-Long Beach Carnage") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds.ns["xmin"], map.bounds.ns["xmax"]*1.1), 
           ylim = c(map.bounds.ns["ymin"], map.bounds.ns["ymax"]*0.95))

ggsave(nasc.map.ns.lbc, filename = here("Figs/fig_backscatter_cps_LongBeachCarnage.png"),
       height = 6, width = 10)

nasc.set.wt.combo <- plot_grid(nasc.map.ns.lbc, set.pies, nrow = 2,
                               labels = c("a)", "b)"))

# Save combo map
ggsave(nasc.set.wt.combo, filename = here("Figs/fig_nasc_seine_proportion_set_wt_LongBeachCarnage.png"),
       height = 10, width = 8)

# # Plot Saildrone data ----------------------------------------------------
# # Assign backscatter to trawl hauls ------------------------------------
# # Create variable for nearest cluster and minimum distance
# haul.distance.ns <- data.frame(haul = rep(NA, nrow(nasc.nearshore)),
#                                haul.distance = rep(NA, nrow(nasc.nearshore)))
# # Configure progress bar
# pb <- tcltk::tkProgressBar("R Progress Bar", "Haul Assignment", 0, 100, 0)
# 
# # Assign trawl clusters
# for (i in 1:nrow(nasc.nearshore)) {
#   # Calculate distance between each NASC interval and all trawl clusters
#   temp.distance <- distance(nasc.nearshore$lat[i], nasc.nearshore$long[i], 
#                             haul.pie$lat, haul.pie$long, 
#                             units = "nm")
#   
#   # Assign cluster with minimum distance to NASC interval
#   haul.distance.ns$haul[i]          <- haul.pie$haul[which.min(temp.distance)]
#   haul.distance.ns$haul.distance[i] <- temp.distance[which.min(temp.distance)]
#   
#   # Update progress bar
#   pb.prog <- round(i/nrow(nasc.nearshore)*100)
#   info <- sprintf("%d%% done", pb.prog)
#   tcltk::setTkProgressBar(pb, pb.prog, sprintf("Haul Assignment (%s)", info), info)
# }
# 
# # Close progress bar
# close(pb)
# 
# # Add haul distances to nasc
# nasc.nearshore <- bind_cols(nasc.nearshore, haul.distance.ns) 
# 
# # Use nav data to resize map to survey progress
# map.bounds.ns <- nasc.paths.ns %>%
#   filter(str_detect(transect.name, "SD")) %>% 
#   st_transform(crs = 3310) %>%
#   st_bbox()
# 
# # Calculate pie radius based on latitude range
# pie.radius.ns <- as.numeric(abs(map.bounds.ns$ymin - map.bounds.ns$ymax)*pie.scale)
# 
# # Calculate pie radius of each pie, based on All CPS landings
# if (scale.pies) {
#   haul.pie$radius    <- pie.radius.ns*haul.pie$bin
# } else {
#   haul.pie$radius    <- pie.radius.ns
# }
# 
# # Filter for empty trawls
# haul.zero    <- filter(haul.pie, AllCPS == 0, haul %in% nasc.nearshore$haul)
# 
# # Replace zeros with minimally small value for scatterpie plotting
# haul.pos <- filter(haul.pie, AllCPS > 0, haul %in% nasc.nearshore$haul) %>% 
#   replace(. == 0, 0.0000001) %>% 
#   arrange(desc(X))
# 
# # Select plot levels for backscatter data
# nasc.plot.ns.sub <- filter(nasc.plot.ns, str_detect(transect.name, "SD"))
# 
# nasc.levels.all <- sort(unique(nasc.plot.ns.sub$bin.level))
# nasc.labels.all <- nasc.labels[nasc.levels.all]
# nasc.sizes.all  <- nasc.sizes[nasc.levels.all]
# nasc.colors.all <- nasc.colors[nasc.levels.all]
# 
# haul.pies <- base.map + 
#   # Plot NASC data
#   geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name)) +
#   # Plot purse seine pies
#   scatterpie::geom_scatterpie(data = haul.pos, 
#                               aes(X, Y, group = haul, r = r*1.5),
#                               cols = c("Anchovy", "JackMack", "Jacksmelt",
#                                        "PacHerring", "PacMack", "Sardine"),
#                               color = 'black', alpha = 0.8) +
#   # Configure trawl scale
#   scale_fill_manual(name = 'Species',
#                     labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
#                                "P. herring", "P. mackerel", "Sardine"),
#                     values = c(anchovy.color, jack.mack.color, jacksmelt.color,
#                                pac.herring.color, pac.mack.color, sardine.color)) +
#   geom_point(data = filter(haul.zero, haul %in% nasc.nearshore$haul), 
#              aes(X, Y)) +
#   # ggtitle("CPS Proportions in Trawl Hauls") +
#   coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
#            xlim = c(map.bounds.ns["xmin"]*1.4, map.bounds.ns["xmax"]*0.8),
#            ylim = c(map.bounds.ns["ymin"]*0.9, map.bounds.ns["ymax"]))
# 
# ggsave(haul.pies, filename = here("Figs/fig_trawl_proportion_haul_wt_Saildrone.png"),
#        height = 10, width = 4)
# 
# # Map backscatter
# nasc.map.ns.sd <- base.map +
#   # Plot transects data
#   geom_sf(data = filter(transects.sf, Type == "Nearshore"), 
#           size = 0.5, colour = "gray70", 
#           alpha = 0.75, linetype = "dashed") +
#   # Plot NASC data
#   geom_path(data = nasc.plot.ns.sub, aes(X, Y, group = transect.name),
#             colour = "gray50", size = 0.5, alpha = 0.5) +
#   # Plot NASC data
#   geom_point(data = nasc.plot.ns.sub, aes(X, Y, size = bin, fill = bin), 
#              shape = 21, alpha = 0.75) +
#   # Configure size and colour scales
#   scale_size_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
#                     values = nasc.sizes.all,labels = nasc.labels.all) +
#   scale_fill_manual(name = bquote(atop(italic(s)[A], ~'(m'^2 ~'nmi'^-2*')')),
#                     values = nasc.colors.all,labels = nasc.labels.all) +
#   # Configure legend guides
#   guides(fill = guide_legend(), size = guide_legend()) +
#   # Plot title
#   # ggtitle("CPS Backscatter-USV") +
#   coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
#            xlim = c(map.bounds.ns["xmin"]*1.4, map.bounds.ns["xmax"]*0.8), 
#            ylim = c(map.bounds.ns["ymin"]*0.9, map.bounds.ns["ymax"]))
# 
# ggsave(nasc.map.ns.sd, filename = here("Figs/fig_backscatter_cps_Saildrone.png"),
#        height = 10, width = 6)
# 
# nasc.set.wt.combo <- plot_grid(nasc.map.ns.sd, haul.pies, nrow = 1,
#                                labels = c("a)", "b)"))
# 
# # Save combo map
# ggsave(nasc.set.wt.combo, filename = here("Figs/fig_nasc_trawl_proportion_haul_wt_Saildrone.png"),
#        height = 10, width = 12)


# Combine all backscatter figures -----------------------------------------
nasc.ns.combo <- plot_grid(nasc.map.ns.lm, nasc.map.ns.lbc,
                           align = "v", nrow = 1, 
                           labels = c("a)", "b)"))

# Save combo map
ggsave(nasc.ns.combo, filename = here("Figs/fig_backscatter_cps_AllNearshore.png"),
       height = 10, width = 20)

# Get max TL for plotting L/W models
L.max <- select(lm.specimens, scientificName, totalLength_mm) %>%
  bind_rows(select(lbc.specimens, scientificName, totalLength_mm)) %>% 
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

# Data frame for storing results
lw.df <- data.frame()

# Generate length/weight curves
for (i in unique(L.max$scientificName)) {
  # Create a length vector for each species
  L <- seq(0, L.max$max.TL[L.max$scientificName == i])
  
  # Calculate weights from lengths
  W <- estimate_weight(i, L, season = tolower(survey.season))
  
  # Combine results
  lw.df <- bind_rows(lw.df, data.frame(scientificName = i, L, W))
}

# Estimate missing weights from lengths -------------------------------------------------------
# Add a and b to length data frame and calculate missing lengths/weights
lm.specimens <- lm.specimens %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
      TRUE ~ totalLength_mm),
    K = round((weightg/totalLength_mm*10^3)*100))

lbc.specimens <- lbc.specimens %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
      TRUE ~ totalLength_mm),
    K = round((weightg/totalLength_mm*10^3)*100))


# Plot specimen L/W for Lisa Marie ----------------------------- 
lw.plot.lm <- ggplot() + 
  # Plot seasonal length models for each species
  geom_line(data = lw.df, aes(L, W), linetype = 'dashed') +
  geom_point(data = lm.specimens, aes(forkLength_mm, weightg, label = label), 
             colour = "blue") + 
  geom_point(data = lm.specimens, aes(standardLength_mm, weightg, label = label), 
             colour = "red") + 
  geom_point(data = lm.specimens, aes(totalLength_mm, weightg, label = label), 
             colour = "green") + 
  facet_wrap(~scientificName, scales = "free") +
  xlab("Length (mm)") +
  ylab("Weight (g)") +
  theme_bw()  +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic"))

ggsave(lw.plot.lm, filename = here("Figs/fig_LW_plots_LisaMarie.png"),
       width = 10, height = 7)

# ggplotly(lw.plot.lm)

# Plot speciment L/W for Long Beach Carnage ----------------------------- 
lw.plot.lbc <- ggplot() + 
  # Plot seasonal length models for each species
  geom_line(data = lw.df, aes(L, W), linetype = 'dashed') +
  geom_point(data = lbc.specimens, aes(forkLength_mm, weightg, label = label), 
             colour = "blue") + 
  geom_point(data = lbc.specimens, aes(standardLength_mm, weightg, label = label), 
             colour = "red") + 
  geom_point(data = lbc.specimens, aes(totalLength_mm, weightg, label = label), 
             colour = "green") + 
  facet_wrap(~scientificName, scales = "free") +
  xlab("Length (mm)") +
  ylab("Weight (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic"))

ggsave(lw.plot.lbc, filename = here("Figs/fig_LW_plots_LongBeachCarnage.png"),
       width = 10, height = 7)

# ggplotly(lw.plot.lbc)

