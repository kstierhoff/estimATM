# Script to process sets, catch, and specimens for comparative purse-seine (Lisa Marie) and trawl sampling (Lasker)
# during summer 2024

# Load required packages
pacman::p_load(sf, scatterpie)

## Import comparative set data ------------------------------------------------------------------
if (exists("sets.comp")) rm(sets.comp)
for (v in "LM") {
  sets.comp <- readxl::read_xlsx(seine.data.paths[v], sheet = "sets-comparative") 
  
  if (nrow(sets.comp) > 0) {
    sets.comp <- sets.comp %>%
      mutate(datetime = force_tz(datetime,"America/Los_Angeles"),
             vessel_name = seine.vessels.long[v],
             vessel.name = v,
             key.set = paste(vessel.name, date, haul),
             sample.type = "Purse seine") %>% 
      # Convert datetime to UTC
      mutate(datetime = with_tz(datetime, tzone = "UTC"))
  }
}

# Create set cluster file
sets.comp.clusters <- select(sets.comp, key.set, date, datetime, vessel.name, lat, long, sample.type) %>%
  filter(vessel.name %in% seine.vessels) %>% 
  arrange(vessel.name, datetime) %>% 
  # Begin clustering after primary survey vessel clusters
  mutate(sample.type = "Purse seine") %>%
  project_df(to = crs.proj)

# Create haul paths from starts and ends
haul.paths <- select(haul, haul, cluster, equilibriumTime, lat = startLatDecimal, long = startLongDecimal) %>% 
  bind_rows(select(haul, haul, cluster, equilibriumTime, lat = stopLatDecimal, long = stopLongDecimal)) %>% 
  arrange(haul) %>% 
  # Select hauls that occurred during the comparative survey
  filter(between(equilibriumTime, min(sets.comp.clusters$datetime) - hours(3), max(sets.comp.clusters$datetime) + hours(3))) %>% 
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  group_by(haul, cluster, equilibriumTime) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") 

# Load base.map if not in environment
if(!exists("base.map")) load(here("Data/Map/basemap.Rdata"))

# Save set info
save(sets.comp, sets.comp.clusters, file = here("Output/purse_seine_sets_comp.Rdata"))

## Import comparative catch data ---------------------------------------------------------------------
if (exists("set.catch.comp")) rm(set.catch.comp)
for (v in "LM") {
  set.catch.comp <- readxl::read_xlsx(seine.data.paths[v], sheet = "catch-comparative") 
  
  if (nrow(set.catch.comp) > 0) {
    set.catch.comp <- set.catch.comp %>%
      mutate(vessel_name = seine.vessels.long[v],
             vessel.name = v,
             key.set = paste(vessel.name, date, haul),
             scientificName = case_when(
               common_name == "Pacific Herring" ~ "Clupea pallasii",
               common_name == "Pacific Sardine" ~ "Sardinops sagax",
               common_name == "Pacific Mackerel" ~ "Scomber japonicus",
               common_name == "Jack Mackerel" ~ "Trachurus symmetricus",
               common_name == "Northern Anchovy" ~ "Engraulis mordax",
               common_name == "Round Herring" ~ "Etrumeus acuminatus",
               # common_name == "Jack Smelt" ~ "Atherinopsis californiensis",
               # common_name == "Whitebait Smelt" ~ "Allosmerus elongatus",
               # common_name == "Pacific Hake" ~ "Merluccius productus",
               TRUE ~ NA_character_)) %>% 
      filter(!is.na(scientificName)) %>% 
      group_by(key.set, vessel.name, scientificName) %>% 
      summarise(totalWeight = sum(weight_kg),
                totalNum    = sum(count))  
  }
}

# Save set catch info
save(set.catch.comp, file = here("Output/purse_seine_catch_comp.Rdata"))

## Import comparative set specimen data ----------------------------------------------------
if (exists("set.lengths.comp")) rm(set.lengths.comp)
for (v in "LM") {
  set.lengths.comp <- readxl::read_xlsx(seine.data.paths[v], sheet = "specimens-comparative")
  
  if (nrow(set.lengths.comp) > 0) {
    set.lengths.comp <- set.lengths.comp %>%
      mutate(vessel_name = seine.vessels.long[v],
             vessel.name = v,
             key.set = paste(vessel.name, date, haul),
             label = paste("Date:", date, "Haul:", haul, "Specimen:", specimen),
             scientificName = case_when(
               common_name == "Pacific Herring" ~ "Clupea pallasii",
               common_name == "Pacific Sardine" ~ "Sardinops sagax",
               common_name == "Pacific Mackerel" ~ "Scomber japonicus",
               common_name == "Jack Mackerel" ~ "Trachurus symmetricus",
               common_name == "Northern Anchovy" ~ "Engraulis mordax",
               common_name == "Round Herring" ~ "Etrumeus acuminatus",
               TRUE ~ NA_character_),
             missing.weight = case_when(is.na(weight_g)   ~ T, TRUE ~ FALSE),
             missing.length = case_when(is.na(length_mm) ~ T, TRUE ~ FALSE)) %>%
      mutate(
        weightg = weight_g,
        forkLength_mm = case_when(
          scientificName %in% c("Clupea pallasii","Scomber japonicus",
                                "Trachurus symmetricus","Etrumeus acuminatus") ~ length_mm,
          TRUE ~ NA_real_),
        standardLength_mm = case_when(
          scientificName %in% c("Engraulis mordax","Sardinops sagax") ~ length_mm,
          TRUE ~ NA_real_)) %>%
      filter(scientificName %in% cps.spp, !is.na(haul)) %>%
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
            convert_length("Etrumeus acuminatus", .$forkLength_mm, "FL", "TL"))) %>%
      mutate(
        weightg = case_when(
          is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
          TRUE  ~ weightg),
        totalLength_mm = case_when(
          is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
          TRUE ~ totalLength_mm),
        K = round((weightg/totalLength_mm*10^3)*100)) 
  }
}

# set.lengths.comp %>% group_by(scientificName) %>% summarise(min.length = min(length_mm), max.length = max(length_mm))
# set.lengths.comp %>% group_by(scientificName) %>% summarise(min.w = min(weight_g), max.w = max(weight_g))
# filter(set.lengths.comp, is.na(forkLength_mm)) %>% group_by(scientificName) %>% tally()
# filter(set.lengths.comp, is.na(standardLength_mm)) %>% group_by(scientificName) %>% tally()

save(set.lengths.comp, file = here("Output/purse_seine_lengths_comp.Rdata"))

# Get max TL for plotting L/W models
L.max.ns.comp <- set.lengths.comp %>% 
  filter(scientificName %in% c(cps.spp, "Merluccius productus")) %>%
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

lw.df.ns.comp <- data.frame()

# Generate length/weight curves
for (i in unique(L.max.ns.comp$scientificName)) {
  # Create a length vector for each species
  totalLength_mm <- seq(0, L.max.ns.comp$max.TL[L.max.ns.comp$scientificName == i])
  
  # Calculate weights from lengths
  weightg <- estimate_weight(i, totalLength_mm, season = tolower(survey.season))
  
  # Combine results
  lw.df.ns.comp <- bind_rows(lw.df.ns.comp, data.frame(scientificName = i, weightg, totalLength_mm))
}

# Convert lengths for plotting
lw.df.ns.comp <- lw.df.ns.comp %>% 
  mutate(
    forkLength_mm     = convert_length(scientificName, totalLength_mm, "TL", "FL"),
    standardLength_mm = convert_length(scientificName, totalLength_mm, "TL", "SL")
  ) %>% 
  pivot_longer(totalLength_mm:standardLength_mm, names_to = "length_type", values_to = "length_mm")

# Plot specimen lengths
lw.plot.ns.comp <- ggplot() +
  # Plot seasonal length models for each species
  geom_line(data = filter(lw.df.ns.comp, scientificName %in% unique(set.lengths.comp$scientificName)), 
            aes(length_mm, weightg, group = length_type, linetype = length_type),
            colour = "gray50", alpha = 0.5) +
  geom_point(data = set.lengths.comp,
             aes(standardLength_mm, weightg, fill = sex, shape = vessel.name, label = label),
             colour = "gray50", alpha = 0.9) +
  geom_point(data = set.lengths.comp,
             aes(forkLength_mm, weightg, fill = sex, shape = vessel.name, label = label),
             colour = "gray50", alpha = 0.9) +
  scale_fill_manual(name = "Sex", values = c("pink","lightblue","orange","lightgreen")) +
  scale_linetype_discrete(name = "Length type") +
  scale_shape_manual(name = "Vessel", values = c(21, 23)) +
  facet_wrap(~scientificName, scales = "free") +
  xlab("Length (mm)") + ylab("Mass (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic")) 

# save length/weight plot
ggsave(lw.plot.ns.comp, filename = here("Figs/fig_LW_plots_nearshore_comp.png"),
       width = 10, height = 7) 

# plotly::ggplotly(lw.plot.ns.comp)

# Summarize catch data ------------------------------------------------
set.catch.comp.summ <- set.catch.comp %>%
  select(key.set, scientificName, totalWeight) %>% 
  tidyr::spread(scientificName, totalWeight) %>% 
  right_join(select(sets.comp, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set) 

# Make specimen summaries spatial -----------------------------------------
set.catch.comp.summ.sf <- set.catch.comp.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

# Summarise catch by weight -----------------------------------------------
sets.comp.summ.wt <- set.catch.comp.summ %>% 
  filter(vessel.name %in% seine.vessels)

# Add species with zero total weight
if (!has_name(sets.comp.summ.wt, "Engraulis mordax"))      {sets.comp.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(sets.comp.summ.wt, "Sardinops sagax"))       {sets.comp.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(sets.comp.summ.wt, "Scomber japonicus"))     {sets.comp.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(sets.comp.summ.wt, "Trachurus symmetricus")) {sets.comp.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(sets.comp.summ.wt, "Clupea pallasii"))       {sets.comp.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(sets.comp.summ.wt, "Etrumeus acuminatus"))   {sets.comp.summ.wt$`Etrumeus acuminatus`   <- 0}
if (!has_name(sets.comp.summ.wt, "Atherinopsis californiensis")) {sets.comp.summ.wt$`Atherinopsis californiensis` <- 0}
if (!has_name(sets.comp.summ.wt, "Other"))                 {sets.comp.summ.wt$`Other` <- 0}

# Calculate total weight of all CPS species
sets.comp.summ.wt <- sets.comp.summ.wt %>%  
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
  select(-vessel.name, -lat, -long, -AllCPS) %>%
  group_by(key.set) %>% 
  summarise_all(list(sum)) %>% 
  ungroup() %>% 
  mutate(AllCPS = rowSums(select(., -key.set))) %>% 
  right_join(select(sets.comp.clusters, key.set, vessel.name, datetime, long, lat, sample.type)) %>%
  replace(is.na(.), 0) 

# Get species present in the trawl catch
pie.spp.ns.comp <- c("PacHerring" = "Clupea pallasii", "Anchovy" = "Engraulis mordax", 
                     "Sardine" = "Sardinops sagax", "PacMack" = "Scomber japonicus", 
                     "JackMack" = "Trachurus symmetricus", "AllCPS" = "AllCPS")

sets.comp.pie <- sets.comp.summ.wt %>% 
  select(key.set, vessel.name, long, lat, Anchovy, JackMack, PacHerring, PacMack, Sardine, AllCPS, sample.type) %>% 
  project_df(to = 3310) %>% 
  mutate(
    sample.type = "Purse seine")

# Determine map bounds from set data
map.bounds.ns.comp <- set.catch.comp.summ.sf %>%
  st_buffer(10/60) %>% 
  st_transform(crs = 3310) %>%
  st_bbox()

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds.ns.comp$xmax - map.bounds.ns.comp$xmin)/(map.bounds.ns.comp$ymax - map.bounds.ns.comp$ymin)
map.width  <- map.height*map.aspect

# Calculate pie radius based on latitude range
pie.radius.ns.comp  <- as.numeric(abs(map.bounds.ns.comp$ymin - map.bounds.ns.comp$ymax)*pie.scale)

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  sets.comp.pie$r      <- pie.radius.ns.comp*sets.comp.pie$bin
} else {
  sets.comp.pie$r      <- pie.radius.ns.comp
}

sets.comp.pos  <- filter(sets.comp.pie, AllCPS > 0) %>% 
  arrange(desc(X))

sets.comp.zero <- filter(sets.comp.pie, AllCPS == 0)

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(sets.comp.pos) > 0) {
  sets.comp.pos <- sets.comp.pos %>% 
    replace(. == 0, 0.0000001) 
}

# Load cluster data if not present
if(!exists("cluster.pos")) load(here("Output/catch_info.Rdata"))

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  haul.pie$r    <- pie.radius.ns.comp*haul.pie$bin
  cluster.pie$r <- pie.radius.ns.comp*cluster.pie$bin
} else {
  haul.pie$r    <- pie.radius.ns.comp
  cluster.pie$r <- pie.radius.ns.comp
}

# Remove hauls/clusters that weren't part of the comparison
haul.pie <- filter(haul.pie, haul %in% haul.paths$haul)
cluster.pie <- filter(cluster.pie, cluster %in% haul.paths$cluster)

# Filter for positive hauls and clusters
haul.pos <- filter(haul.pie, AllCPS > 0) %>% 
  arrange(X) 

cluster.pos <- filter(cluster.pie, AllCPS > 0) %>% 
  arrange(X) 

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(haul.pos) > 0) {
  haul.pos <- haul.pos %>% 
    replace(. == 0, 0.0000001) 
  
  cluster.pos <- cluster.pos %>% 
    replace(. == 0, 0.0000001) 
}

if (nrow(sets.comp.pos) > 0) {
  sets.comp.pos <- sets.comp.pos %>% 
    replace(. == 0, 0.0000001) 
}

# Map trawl species proportions -------------------------------------------------------
# Load planned transects
if(!exists("transects.sf")) transects.sf <- st_read(here("Output/planned_transects.shp"))
# Load nav data if not present
if(!exists("nav.paths.sf")) load(here("Data/Nav/nav_data.Rdata"))

# Plot locations of trawl hauls and purse seine sets -------------------------------
sets.comp.map <- base.map +
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5, linetype = "dashed") +
  geom_point(data = sets.comp.pie, aes(X, Y), size = 2, shape = 21, fill = "red") +
  geom_sf(data = haul.paths, size = 10) + 
  # Adjust longitude axis to avoid overplotting
  scale_x_continuous(breaks = seq(-125, -123.5, by = 0.5)) +
  coord_sf(crs = crs.proj,
           xlim = unname(c(map.bounds.ns.comp["xmin"], map.bounds.ns.comp["xmax"])),
           ylim = unname(c(map.bounds.ns.comp["ymin"], map.bounds.ns.comp["ymax"])))

ggsave(sets.comp.map,
       filename = here("Figs/fig_trawl_seine_sets_comp.png"),
       width = map.width, height = map.height)

# Create seine catch figure ---------------------
sets.comp.pie.cluster.wt <- base.map +
  # Plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5, linetype = "dashed") +
  # Plot seine pies
  geom_scatterpie(data = sets.comp.pos, aes(X, Y, group = key.set, r = pie.radius.ns.comp*2.5, colour = sample.type),
                  cols = pie.cols[names(pie.cols) %in% pie.spp.ns.comp],
                  alpha = 0.8, sorted_by_radius = TRUE) +
  # Configure pie outline colors
  scale_colour_manual(name = "Sample type",
                      labels = c("Purse seine", "Trawl"),
                      values = c("Purse seine" = seine.color, "Trawl" = trawl.color),
                      guide = "none") +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp.ns.comp]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp.ns.comp])) +
  # Adjust longitude axis to avoid overplotting
  scale_x_continuous(breaks = seq(-125, -123.5, by = 0.5)) +
  # Plot empty cluster locations
  geom_point(data = sets.comp.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  theme(legend.position.inside = c(0.05,0.9),
        legend.justification = c(0,0.5),
        legend.background = element_rect(fill="white", colour="white")) +
  # Plot panel label
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds.ns.comp["xmin"], map.bounds.ns.comp["xmax"])), 
           ylim = unname(c(map.bounds.ns.comp["ymin"], map.bounds.ns.comp["ymax"])))

# Save nasc plot
ggsave(sets.comp.pie.cluster.wt,
       filename = here("Figs/fig_set_proportion_cluster_wt_nearshore_comp.png"),
       width = map.width, height = map.height) 

# Create trawl cluster figure ----------------------------
trawl.pie.cluster.wt.zoom <- base.map +
  # Plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5, linetype = "dashed") +
  # Plot trawl pies
  geom_scatterpie(data = cluster.pos, aes(X, Y, group = cluster, r = pie.radius.ns.comp*2.5, colour = sample.type),
                  cols = pie.cols[names(pie.cols) %in% pie.spp.ns.comp],
                  alpha = 0.8, sorted_by_radius = TRUE) +
  # Configure pie outline colors
  scale_colour_manual(name = "Sample type",
                      labels = c("Purse seine", "Trawl"),
                      values = c("Seine" = seine.color, "Trawl" = trawl.color),
                      guide = "none") +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp.ns.comp]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp.ns.comp])) +
  # Adjust longitude axis to avoid overplotting
  scale_x_continuous(breaks = seq(-125, -123.5, by = 0.5)) +
  # Plot empty cluster locations
  geom_point(data = cluster.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  theme(legend.position.inside = c(0.05,0.9),
        legend.justification = c(0,0.5),
        legend.background = element_rect(fill="white", colour="white")) +
  # Plot panel label
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds.ns.comp["xmin"], map.bounds.ns.comp["xmax"])), 
           ylim = unname(c(map.bounds.ns.comp["ymin"], map.bounds.ns.comp["ymax"])))

# Create trawl haul figure ----------------------------
trawl.pie.haul.wt.zoom <- base.map +
  # Plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5, linetype = "dashed") +
  # Plot trawl pies
  geom_scatterpie(data = haul.pos, aes(X, Y, group = haul, r = pie.radius.ns.comp*2.5, colour = sample.type),
                  cols = pie.cols[names(pie.cols) %in% pie.spp.ns.comp],
                  alpha = 0.8, sorted_by_radius = TRUE) +
  # Configure pie outline colors
  scale_colour_manual(name = "Sample type", 
                      labels = c("Purse seine", "Trawl"),
                      values = c("Seine" = seine.color, "Trawl" = trawl.color),
                      guide = "none") +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp.ns.comp]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp.ns.comp])) +
  # Adjust longitude axis to avoid overplotting
  scale_x_continuous(breaks = seq(-125, -123.5, by = 0.5)) +
  # Plot empty cluster locations
  geom_point(data = haul.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  theme(legend.position.inside = c(0.05,0.9),
        legend.justification = c(0,0.5),
        legend.background = element_rect(fill="white", colour="white")) +
  # Plot panel label
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds.ns.comp["xmin"], map.bounds.ns.comp["xmax"])), 
           ylim = unname(c(map.bounds.ns.comp["ymin"], map.bounds.ns.comp["ymax"])))

# Combine RL and LM catch plots
## Compare RL clusters
trawl.cluster.sets.comp.map <- trawl.pie.cluster.wt.zoom + sets.comp.pie.cluster.wt + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')')
## Compare RL hauls
trawl.haul.sets.comp.map <- trawl.pie.haul.wt.zoom + sets.comp.pie.cluster.wt + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

# Save trawl plot
ggsave(trawl.cluster.sets.comp.map,
       filename = here("Figs/fig_trawl_cluster_set_comp_map.png"),
       width = map.width*2, height = map.height)

# Save trawl plot
ggsave(trawl.haul.sets.comp.map,
       filename = here("Figs/fig_trawl_haul_set_comp_map.png"),
       width = map.width*2, height = map.height)

# Load length data
load(here("Output/lengths_final.Rdata"))

# Compare specimen lengths for RL and LM
lengths.sub.rl <- lengths %>% 
  right_join(haul.paths) %>% 
  select(haul, cluster, scientificName, totalLength_mm) %>% 
  mutate(vessel_name = "Reuben Lasker",
         sample.type = "Trawl") 

lengths.sub.lm <- set.lengths.comp %>% 
  select(scientificName, totalLength_mm, vessel_name) %>% 
  mutate(sample.type = "Purse seine")

lengths.sub.all <- bind_rows(lengths.sub.rl, lengths.sub.lm) %>% 
  filter(!is.na(scientificName)) %>% 
  filter(scientificName != "Engraulis mordax") %>%
  mutate(group = fct_rev(as.factor(sample.type)))

# Plot length distributions for each species and vessel, on separate plots
lengths.comp.grid <- ggplot(lengths.sub.all, aes(totalLength_mm)) + geom_histogram() + 
  facet_grid(sample.type~scientificName, scales = "free_y") + theme_bw() +
  labs(x = "\nTotal length (mm)",
       y = "Counts") +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic")) +
  theme(strip.background.y = element_blank())

# Save figure
ggsave(lengths.comp.grid, filename = here("Figs/fig_trawl_seine_lengths_comp_grid.png"),
       height = 5, width = 10)

# Plot length distributions for each species and vessel
lengths.comp.combo <- ggplot(lengths.sub.all, aes(totalLength_mm, group = group, fill = group)) + 
  geom_histogram(position = "identity", alpha = 0.5) + 
  facet_wrap(~scientificName, nrow = 1, scales = "free_x") + theme_bw() +
  scale_fill_discrete(name = "Sample type") +
  labs(x = "\nTotal length (mm)",
       y = "Counts") +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic")) 

# Save figure
ggsave(lengths.comp.combo, filename = here("Figs/fig_trawl_seine_lengths_comp_combo.png"),
       height = 3, width = 10)

# Save summary statistics from the nearshore sampling
# Net sampling effort
catch.comp.summ <- select(sets.comp, set, sample.type, lat, long) %>% 
  bind_rows(select(haul.pie, haul, sample.type, lat, long)) %>% 
  group_by(sample.type) %>% tally()

# Speciment info
length.comp.summ <- lengths.sub.all %>% 
  group_by(sample.type, scientificName) %>% 
  tally()

# Save results for use in the biomass report
save(catch.comp.summ, length.comp.summ, 
     file = here("Output/catch_comp_summary.Rdata"))
