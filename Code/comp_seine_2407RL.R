# Script to process sets, catch, and specimens for comparative purse-seine (Lisa Marie) and trawl sampling (Lasker)
# During summer 2024

## Import comparative set data ------------------------------------------------------------------
if (exists("set.comp")) rm(set.comp)
for (v in "LM") {
  set.comp <- readxl::read_xlsx(seine.data.paths[v], sheet = "sets-comparative") 
  
  if (nrow(set.comp) > 0) {
    set.comp <- set.comp %>%
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
set.comp.clusters <- select(set.comp, key.set, date, datetime, vessel.name, lat, long, sample.type) %>%
  filter(vessel.name %in% seine.vessels) %>% 
  arrange(vessel.name, datetime) %>% 
  # Begin clustering after primary survey vessel clusters
  mutate(sample.type = "Purse seine") %>%
  project_df(to = crs.proj)

# Plot purse seine sets retained in the analysis
set.comp.map <- base.map + 
  geom_point(data = set.comp.clusters, aes(X, Y, colour = vessel.name)) + 
  coord_sf(crs = crs.proj, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

ggsave(set.comp.map, 
       filename = here("Figs/fig_seine_sets_comp.png"),
       width = map.width, height = map.height)

# Save set info
save(set.comp, set.comp.clusters, file = here("Output/purse_seine_sets_comp.Rdata"))

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

# ggplotly(lw.plot.ns.comp)

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
set.comp.summ.wt <- set.catch.comp.summ %>% 
  filter(vessel.name %in% seine.vessels)

# Add species with zero total weight
if (!has_name(set.comp.summ.wt, "Engraulis mordax"))      {set.comp.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(set.comp.summ.wt, "Sardinops sagax"))       {set.comp.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(set.comp.summ.wt, "Scomber japonicus"))     {set.comp.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(set.comp.summ.wt, "Trachurus symmetricus")) {set.comp.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(set.comp.summ.wt, "Clupea pallasii"))       {set.comp.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(set.comp.summ.wt, "Etrumeus acuminatus"))   {set.comp.summ.wt$`Etrumeus acuminatus`   <- 0}
if (!has_name(set.comp.summ.wt, "Atherinopsis californiensis")) {set.comp.summ.wt$`Atherinopsis californiensis` <- 0}
if (!has_name(set.comp.summ.wt, "Other"))                 {set.comp.summ.wt$`Other` <- 0}

# Calculate total weight of all CPS species
set.comp.summ.wt <- set.comp.summ.wt %>%  
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
  right_join(select(set.comp.clusters, key.set, vessel.name, datetime, long, lat, sample.type)) %>%
  replace(is.na(.), 0) 

set.comp.pie <- set.comp.summ.wt %>% 
  select(key.set, vessel.name, long, lat, Anchovy, JackMack, 
         Jacksmelt, Other, PacHerring, PacMack, RndHerring, Sardine, AllCPS, sample.type) %>% 
  project_df(to = 3310) %>% 
  mutate(
    sample.type = "Purse seine",
    label = paste("Set", key.set),
    popup = paste('<b>Set:', key.set, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'R. herring:', RndHerring, 'kg<br/>',
                  'Other:', Other, 'kg<br/>', 
                  'All CPS:', AllCPS, 'kg'))

# Determine map bounds from set data
map.bounds.ns.comp <- set.catch.comp.summ.sf %>%
  st_buffer(10/60) %>% 
  st_transform(crs = 3310) %>%
  st_bbox()

# Calculate pie radius based on latitude range
pie.radius.ns.comp  <- as.numeric(abs(map.bounds.ns$ymin - map.bounds.ns$ymax)*pie.scale)

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.comp.pie$r      <- pie.radius.ns*set.comp.pie$bin
} else {
  set.comp.pie$r      <- pie.radius.ns
}

set.comp.pos  <- filter(set.comp.pie, AllCPS > 0) %>% 
  arrange(desc(X))

set.comp.zero <- filter(set.comp.pie, AllCPS == 0)

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(set.comp.pos) > 0) {
  set.comp.pos <- set.comp.pos %>% 
    replace(. == 0, 0.0000001) 
}

# Map trawl species proportions -------------------------------------------------------
# Get species present in the trawl catch
pie.spp.ns.comp <- sort(unique(set.catch.comp$scientificName))

# Create trawl haul figure
set.comp.pie.cluster.wt <- base.map +
  # Plot transects data
  geom_sf(data = filter(transects.sf, Type == "Nearshore"), size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  # geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot trawl pies
  geom_scatterpie(data = set.comp.pos, aes(X, Y, group = key.set, r = pie.radius*.3, colour = sample.type),
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
  # Plot empty cluster locations
  geom_point(data = set.comp.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  # Plot panel label
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds.ns.comp["xmin"], map.bounds.ns.comp["xmax"])), 
           ylim = unname(c(map.bounds.ns.comp["ymin"], map.bounds.ns.comp["ymax"])))

# Save nasc plot
ggsave(set.comp.pie.cluster.wt,
       filename = here("Figs/fig_set_proportion_cluster_wt_nearshore_comp.png"),
       width = map.width, height = map.height) 

# Create trawl haul figure
trawl.pie.cluster.wt.zoom <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot trawl pies
  geom_scatterpie(data = cluster.pos, aes(X, Y, group = cluster, r = pie.radius*.3, colour = sample.type),
                  cols = pie.cols[names(pie.cols) %in% pie.spp],
                  alpha = 0.8, sorted_by_radius = TRUE) +
  # Configure pie outline colors
  scale_colour_manual(name = "Sample type",
                      labels = c("Purse seine", "Trawl"),
                      values = c("Seine" = seine.color, "Trawl" = trawl.color),
                      guide = "none") +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp])) +
  # Plot empty cluster locations
  geom_point(data = cluster.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  # Plot panel label
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds.ns.comp["xmin"], map.bounds.ns.comp["xmax"])), 
           ylim = unname(c(map.bounds.ns.comp["ymin"], map.bounds.ns.comp["ymax"])))

# Create trawl haul figure
trawl.pie.haul.wt.zoom <- base.map +
  # Plot transects data
  geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
          alpha = 0.75, linetype = "dashed") +
  # plot ship track data
  geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
  # Plot trawl pies
  geom_scatterpie(data = haul.pos, aes(X, Y, group = haul, r = pie.radius*.3, colour = sample.type),
                  cols = pie.cols[names(pie.cols) %in% pie.spp],
                  alpha = 0.8, sorted_by_radius = TRUE) +
  # Configure pie outline colors
  scale_colour_manual(name = "Sample type", 
                      labels = c("Purse seine", "Trawl"),
                      values = c("Seine" = seine.color, "Trawl" = trawl.color),
                      guide = "none") +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp])) +
  # Plot empty cluster locations
  geom_point(data = haul.zero, aes(X, Y),
             size = 3, shape = 21, fill = 'black', colour = 'white') +
  # geom_scatterpie_legend(radius = trawl.breaks, x = -50000, y = -600000) + 
  # Plot panel label
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = unname(c(map.bounds.ns.comp["xmin"], map.bounds.ns.comp["xmax"])), 
           ylim = unname(c(map.bounds.ns.comp["ymin"], map.bounds.ns.comp["ymax"])))

# Combine RL and LM catch plots
## Compare RL clusters
trawl.cluster.set.comp.map <- trawl.pie.cluster.wt.zoom + set.comp.pie.cluster.wt
## Compare RL hauls
trawl.haul.set.comp.map <- trawl.pie.haul.wt.zoom + set.comp.pie.cluster.wt

# Save trawl plot
ggsave(trawl.cluster.set.comp.map,
       filename = here("Figs/fig_trawl_cluster_set_comp_map.png"),
       width = map.width*2, height = map.height)

# Save trawl plot
ggsave(trawl.haul.set.comp.map,
       filename = here("Figs/fig_trawl_haul_set_comp_map.png"),
       width = map.width*2, height = map.height)

# Compare specimen lengths for RL and LM
lengths.sub.rl <- filter(lengths, between(haul, 123, 146)) %>% 
  select(scientificName, totalLength_mm) %>% 
  mutate(vessel_name = "Reuben Lasker") %>% 
  filter(scientificName != "Engraulis mordax")

lengths.sub.lm <- set.lengths.comp %>% 
  select(scientificName, totalLength_mm, vessel_name)

lengths.sub.all <- bind_rows(lengths.sub.rl, lengths.sub.lm)

length.comp.lm <- ggplot(set.lengths.comp, aes(totalLength_mm)) + geom_histogram() + 
  facet_wrap(~scientificName, nrow = 1, scales = "free_y") + theme_bw()
length.comp.rl <- ggplot(lengths.sub, aes(totalLength_mm)) + geom_histogram() + 
  facet_wrap(~scientificName, nrow = 1, scales = "free_y") + theme_bw()

lengths.comp <- length.comp.rl / length.comp.lm

lengths.comp.grid <- ggplot(lengths.sub.all, aes(totalLength_mm)) + geom_histogram() + 
  facet_grid(vessel_name~scientificName, scales = "free") + theme_bw() +
  labs(x = "\nTotal length (mm)",
       y = "Counts")


ggsave(lengths.comp.grid, filename = here("Figs/fig_length_comps.png"),
       height = 5, width = 10)
