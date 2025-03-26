# This script imports and processes purse seine data from fishing vessels -------
# Source following the section entitled estimateNearshore in estimateBiomass ----
# This script replaces survey-specific versions, and aims to use standardized data inputs. -----

# Import set data ------------------------------------------------------------------
if (exists("sets")) rm(sets)
for (v in seine.vessels) {
  sets.tmp <- readxl::read_xlsx(seine.data.paths[v], sheet = "sets") 
  
  if (nrow(sets.tmp) > 0) {
    sets.tmp <- sets.tmp %>%
      mutate(datetime = force_tz(datetime,"America/Los_Angeles"),
             vessel_name = seine.vessels.long[v],
             vessel.name = v,
             key.set = paste(vessel.name, date, set),
             sample.type = "Purse seine") %>% 
      # Convert datetime to UTC
      mutate(datetime = with_tz(datetime, tzone = "UTC"))
    
    # Combine results
    if (exists("sets")) {
      sets <- bind_rows(sets, sets.tmp)
    } else {
      sets <- sets.tmp
    }
  }
}

# Manually remove "bad" sets (defined in settings)
if(exists("key.set.rm")) {
  sets <- filter(sets, !key.set %in% key.set.rm)
}

# Create set cluster file
set.clusters <- select(sets, key.set, date, datetime, vessel.name, lat, long, sample.type) %>%
  filter(vessel.name %in% seine.vessels) %>% 
  arrange(vessel.name, datetime) %>% 
  # Begin clustering after primary survey vessel clusters
  mutate(cluster = max(cluster.mid$cluster) + as.numeric(as.factor(key.set)),
         haul    = max(haul.mid$haul) + as.numeric(as.factor(key.set)),
         sample.type = "Purse seine") %>%
  project_df(to = crs.proj)

# Plot purse seine sets retained in the analysis
set.map <- base.map + 
  geom_point(data = set.clusters, aes(X, Y, colour = vessel.name)) + 
  coord_sf(crs = crs.proj, 
           xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
           ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))

ggsave(set.map, 
       filename = here("Figs/fig_seine_sets.png"),
       width = map.width, height = map.height)

# Save set info
save(sets, set.clusters, file = here("Output/purse_seine_sets.Rdata"))

## Import set catch data -----------------------------------------------------------
if (exists("set.catch")) rm(set.catch)
for (v in seine.vessels) {
  set.catch.tmp <- readxl::read_xlsx(seine.data.paths[v], sheet = "catch") 
  
  if (nrow(set.catch.tmp) > 0) {
    set.catch.tmp <- set.catch.tmp %>%
      mutate(vessel_name = seine.vessels.long[v],
             vessel.name = v,
             key.set = paste(vessel.name, date, set),
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
    
    # Combine results
    if (exists("set.catch")) {
      set.catch <- bind_rows(set.catch, set.catch.tmp)
    } else {
      set.catch <- set.catch.tmp
    } 
  }
}

# Remove catch data from "bad" sets
set.catch <- filter(set.catch, key.set %in% sets$key.set)

# Save set catch info
save(set.catch, file = here("Output/purse_seine_catch.Rdata"))

# Import set specimen data ----------------------------------------------------
if (exists("set.lengths")) rm(set.lengths)
for (v in seine.vessels) {
  set.lengths.tmp <- readxl::read_xlsx(seine.data.paths[v], sheet = "specimens")
  
  if (nrow(set.lengths.tmp) > 0) {
    set.lengths.tmp <- set.lengths.tmp %>%
      mutate(vessel_name = seine.vessels.long[v],
             vessel.name = v,
             key.set = paste(vessel.name, date, set),
             label = paste("Date:", date, "Set:", set, "Specimen:", specimen),
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
    
    # Combine results
    if (exists("set.lengths")) {
      set.lengths <- bind_rows(set.lengths, set.lengths.tmp)
    } else {
      set.lengths <- set.lengths.tmp
    } 
  }
}

# Remove specimen data from "bad" sets
set.lengths <- filter(set.lengths, key.set %in% sets$key.set)

# set.lengths %>% group_by(scientificName) %>% summarise(min.length = min(length_mm, na.rm = T), max.length = max(length_mm, na.rm = T))
# set.lengths %>% group_by(scientificName) %>% summarise(min.w = min(weight_g, na.rm = T), max.w = max(weight_g, na.rm = T))
# filter(set.lengths, is.na(forkLength_mm)) %>% group_by(scientificName) %>% tally()
# filter(set.lengths, is.na(standardLength_mm)) %>% group_by(scientificName) %>% tally()

save(set.lengths, file = here("Output/purse_seine_lengths.Rdata"))

# Get max TL for plotting L/W models
L.max.ns <- set.lengths %>% 
  filter(scientificName %in% c(cps.spp, "Merluccius productus")) %>%
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

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
  ) %>% 
  pivot_longer(totalLength_mm:standardLength_mm, names_to = "length_type", values_to = "length_mm")

# Plot specimen lengths
lw.plot.ns <- ggplot() +
  # Plot seasonal length models for each species
  geom_line(data = filter(lw.df.ns, scientificName %in% unique(set.lengths$scientificName)), 
            aes(length_mm, weightg, group = length_type, linetype = length_type),
            colour = "gray50", alpha = 0.5) +
  geom_point(data = set.lengths,
             aes(standardLength_mm, weightg, fill = sex, shape = vessel.name, label = label),
             colour = "gray50", alpha = 0.9) +
  geom_point(data = set.lengths,
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
ggsave(lw.plot.ns, filename = here("Figs/fig_LW_plots_nearshore.png"),
       width = 10, height = 7) 

# Summarize catch data ------------------------------------------------
set.catch.summ <- set.catch %>%
  select(key.set, scientificName, totalWeight) %>% 
  tidyr::spread(scientificName, totalWeight) %>% 
  right_join(select(sets, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set) 

set.catch.summ.deep <- set.catch %>%
  select(key.set, scientificName, totalWeight) %>%
  filter(scientificName %in% c("Engraulis mordax","Clupea pallasii")) %>%
  tidyr::spread(scientificName, totalWeight) %>%
  right_join(select(sets, key.set, vessel.name, lat, long)) %>% # Add all sets, incl. empty hauls
  arrange(key.set)

# Make specimen summaries spatial -----------------------------------------
set.catch.summ.sf <- set.catch.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

set.catch.summ.deep.sf <- set.catch.summ.deep %>%
  st_as_sf(coords = c("long","lat"), crs = 4326)

# Summarise catch by weight -----------------------------------------------
set.summ.wt <- set.catch.summ %>% 
  filter(vessel.name %in% seine.vessels)

set.summ.wt.deep <- set.catch.summ.deep %>%
  filter(vessel.name %in% seine.vessels)

# Add species with zero total weight
if (!has_name(set.summ.wt, "Engraulis mordax"))      {set.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(set.summ.wt, "Sardinops sagax"))       {set.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(set.summ.wt, "Scomber japonicus"))     {set.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(set.summ.wt, "Trachurus symmetricus")) {set.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(set.summ.wt, "Clupea pallasii"))       {set.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(set.summ.wt, "Etrumeus acuminatus"))   {set.summ.wt$`Etrumeus acuminatus`   <- 0}
if (!has_name(set.summ.wt, "Atherinopsis californiensis")) {set.summ.wt$`Atherinopsis californiensis` <- 0}
if (!has_name(set.summ.wt, "Other"))                 {set.summ.wt$`Other` <- 0}

# Add species with zero total weight to deep species
if (!has_name(set.summ.wt.deep, "Engraulis mordax"))      {set.summ.wt.deep$`Engraulis mordax`      <- 0}
if (!has_name(set.summ.wt.deep, "Sardinops sagax"))       {set.summ.wt.deep$`Sardinops sagax`       <- 0}
if (!has_name(set.summ.wt.deep, "Scomber japonicus"))     {set.summ.wt.deep$`Scomber japonicus`     <- 0}
if (!has_name(set.summ.wt.deep, "Trachurus symmetricus")) {set.summ.wt.deep$`Trachurus symmetricus` <- 0}
if (!has_name(set.summ.wt.deep, "Clupea pallasii"))       {set.summ.wt.deep$`Clupea pallasii`       <- 0}
if (!has_name(set.summ.wt.deep, "Etrumeus acuminatus"))   {set.summ.wt.deep$`Etrumeus acuminatus`   <- 0}
if (!has_name(set.summ.wt.deep, "Atherinopsis californiensis")) {set.summ.wt.deep$`Atherinopsis californiensis` <- 0}
if (!has_name(set.summ.wt.deep, "Other"))                 {set.summ.wt.deep$`Other` <- 0}

# Calculate total weight of all CPS species
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
  left_join(select(set.clusters, key.set, cluster, haul)) %>% 
  select(-vessel.name, -lat, -long, -AllCPS) %>%
  group_by(key.set, cluster, haul) %>% 
  summarise_all(list(sum)) %>% 
  ungroup() %>% 
  mutate(AllCPS = rowSums(select(., -key.set, -cluster, -haul))) %>% 
  right_join(select(set.clusters, key.set, vessel.name, datetime, long, lat, sample.type)) %>%
  replace(is.na(.), 0) 

set.summ.wt.deep <- set.summ.wt.deep %>%
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
  left_join(select(set.clusters, key.set, cluster, haul)) %>%
  select(-vessel.name, -lat, -long, -AllCPS) %>%
  group_by(key.set, cluster, haul) %>%
  summarise_all(list(sum)) %>%
  ungroup() %>%
  mutate(AllCPS = rowSums(select(., -key.set, -cluster, -haul))) %>%
  right_join(select(set.clusters, key.set, vessel.name, datetime, long, lat, sample.type)) %>%
  replace(is.na(.), 0)

set.pie <- set.summ.wt %>% 
  arrange(cluster) %>% 
  select(key.set, cluster, haul, vessel.name, long, lat, Anchovy, JackMack, 
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

set.pie.deep <- set.summ.wt.deep %>%
  arrange(cluster) %>%
  select(key.set, cluster, haul, vessel.name, long, lat, Anchovy, JackMack,
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
map.bounds.ns <- set.catch.summ.sf %>%
  st_transform(crs = 3310) %>%
  st_bbox()

# Calculate pie radius based on latitude range
pie.radius.ns  <- as.numeric(abs(map.bounds.ns$ymin - map.bounds.ns$ymax)*pie.scale)

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.pie$r      <- pie.radius.ns*set.pie$bin
  # set.pie.deep$r <- pie.radius.ns*set.pie.deep$bin
} else {
  set.pie$r      <- pie.radius.ns
  # set.pie.deep$r <- pie.radius.ns
}

# Extract sets with positive CPS catch
set.pos      <- filter(set.pie, AllCPS > 0) %>% 
  arrange(desc(X))
set.pos.deep <- filter(set.pie.deep, AllCPS > 0) %>%
  arrange(desc(X))

# Extract sets with zero CPS catch
set.zero      <- filter(set.pie, AllCPS == 0)
set.zero.deep <- filter(set.pie.deep, AllCPS == 0)

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(set.pos) > 0) {
  set.pos <- set.pos %>% 
    replace(. == 0, 0.0000001) 
}

if (nrow(set.pos.deep) > 0) {
  set.pos.deep <- set.pos.deep %>%
    replace(. == 0, 0.0000001)
}

# Summarize seine set data
seine.summ <- set.summ.wt %>% 
  left_join(select(set.clusters, key.set, Vessel = vessel.name, Date = datetime, 
                   Latitude = lat, Longitude = long)) %>%
  select(Vessel, Set = haul, Date, Latitude, Longitude, "N. Anchovy" = Anchovy, "P. Sardine" = Sardine, 
         "P. Mackerel" = PacMack, "J. Mackerel" = JackMack, "P. Herring" = PacHerring, 
         "R. Herring" = RndHerring, "Other" = Other, Jacksmelt, "All CPS" = AllCPS) %>% 
  mutate(Date = format(Date, "%m/%d/%Y %H:%M")) %>% 
  arrange(desc(Vessel), Set)

seine.summ.deep <- set.summ.wt.deep %>%
  left_join(select(set.clusters, key.set, Vessel = vessel.name, Date = datetime,
                   Latitude = lat, Longitude = long)) %>%
  select(Vessel, Set = haul, Date, Latitude, Longitude, "N. Anchovy" = Anchovy, "P. Sardine" = Sardine,
         "P. Mackerel" = PacMack, "J. Mackerel" = JackMack, "P. Herring" = PacHerring,
         "R. Herring" = RndHerring, "Other" = Other, Jacksmelt, "All CPS" = AllCPS) %>%
  mutate(Date = format(Date, "%m/%d/%Y %H:%M")) %>%
  arrange(desc(Vessel), Set)

# Select positive clusters
super.clusters.ns <- filter(set.clusters, cluster %in% unique(set.pos$cluster)) %>% 
  select(-key.set, -vessel.name)

super.clusters.ns.deep <- filter(set.clusters, cluster %in% unique(set.pos.deep$cluster)) %>%
  select(-key.set, -vessel.name)

# Subset data from all platforms
lengths.seine <- set.lengths %>%
  select(key.set, scientificName, standardLength_mm, forkLength_mm, totalLength_mm, weightg, vessel.name) %>% 
  filter(vessel.name %in% seine.vessels)

lengths.seine.deep <- lengths.seine %>%
  filter(scientificName %in% c("Engraulis mordax","Clupea pallasii")) %>%
  filter(key.set %in% set.pos.deep$key.set)

# Estimate TS
ts.df.seine <- estimate_ts(lengths.seine$scientificName, lengths.seine$totalLength_mm)
ts.df.seine.deep <- estimate_ts(lengths.seine.deep$scientificName, lengths.seine.deep$totalLength_mm)

# Add TS estimates to lengths data frame
lengths.seine <- bind_cols(lengths.seine, select(ts.df.seine, -species, -TL)) %>%
  arrange(vessel.name, scientificName, id) %>%
  filter(vessel.name %in% seine.vessels) %>%
  left_join(select(set.clusters, key.set, cluster, haul))

lengths.seine.deep <- bind_cols(lengths.seine.deep, select(ts.df.seine.deep, -species, -TL)) %>%
  arrange(vessel.name, scientificName, id) %>%
  filter(vessel.name %in% seine.vessels) %>%
  left_join(select(set.clusters, key.set, cluster, haul))

# Calculate cluster length frequencies for purse seine data ---------------
# Calculate numeric abundance by species and set
n.summ.set <- lengths.seine %>%
  group_by(scientificName, cluster, haul) %>%
  tally(name = "num") %>%
  ungroup()

n.summ.set.deep <- lengths.seine.deep %>%
  group_by(scientificName, cluster, haul) %>%
  tally(name = "num") %>%
  ungroup()

# Summarise weights and sigmas by haul
l.summ.set <- lengths.seine %>%
  group_by(scientificName, cluster, haul) %>%
  summarise(
    meanwg       = mean(estimated.wg),
    sigmaindiv   = mean(sigma.ind),
    sigmawg      = sum(estimated.wg*sigma.wg)/sum(estimated.wg)) %>%
  left_join(n.summ.set) %>%
  ungroup()

l.summ.set.deep <- lengths.seine.deep %>%
  group_by(scientificName, cluster, haul) %>%
  summarise(
    meanwg       = mean(estimated.wg),
    sigmaindiv   = mean(sigma.ind),
    sigmawg      = sum(estimated.wg*sigma.wg)/sum(estimated.wg)) %>%
  left_join(n.summ.set.deep) %>%
  ungroup()

# Create a data frame with abbreviations
trawl.ts.names <- data.frame(
  scientificName = c("Clupea pallasii","Engraulis mordax","Etrumeus acuminatus", "Other",
                     "Sardinops sagax","Scomber japonicus","Trachurus symmetricus"),
  shortName      = c("her","anch","rher","other","sar",
                     "mack","jack"))

# Remove seine TS data
if (exists("seine.ts")) rm(seine.ts)
if (exists("seine.ts.deep")) rm(seine.ts.deep)

# Combine results to compare with Juan's output
for (k in unique(l.summ.set$scientificName)) {
  # Summarize length summary by species
  l.summ.tmp <- l.summ.set %>%
    filter(scientificName == k) %>%
    select(cluster, haul, sigmawg, sigmaindiv)
  
  # Rename columns per species
  names(l.summ.tmp)[3:4] <- paste(names(l.summ.tmp)[3:4],
                                  trawl.ts.names$shortName[trawl.ts.names$scientificName == k],
                                  sep = ".")
  
  # Combine results
  if (exists("seine.ts")) {
    seine.ts <- full_join(seine.ts, l.summ.tmp) %>%
      arrange(cluster)
  } else {
    seine.ts <- l.summ.tmp
  }
}

for (k in unique(l.summ.set.deep$scientificName)) {
  # Summarize length summary by species
  l.summ.tmp <- l.summ.set.deep %>%
    filter(scientificName == k) %>%
    select(cluster, haul, sigmawg, sigmaindiv)

  # Rename columns per species
  names(l.summ.tmp)[3:4] <- paste(names(l.summ.tmp)[3:4],
                                  trawl.ts.names$shortName[trawl.ts.names$scientificName == k],
                                  sep = ".")

  # Combine results
  if (exists("seine.ts.deep")) {
    seine.ts.deep <- full_join(seine.ts.deep, l.summ.tmp) %>%
      arrange(cluster)
  } else {
    seine.ts.deep <- l.summ.tmp
  }
}

# Write results to CSV
write.csv(n.summ.set, file = here("Output/n_summ_set.csv"),
          quote = F, row.names = F)
write.csv(l.summ.set, file = here("Output/l_summ_set.csv"),
          quote = F, row.names = F)
write.csv(seine.ts, file = here("Output/seine_ts_EstimateCPS.csv"),
          quote = F, row.names = F, na = "0")

write.csv(n.summ.set.deep, file = here("Output/n_summ_set_deep.csv"),
          quote = F, row.names = F)
write.csv(l.summ.set.deep, file = here("Output/l_summ_set_deep.csv"),
          quote = F, row.names = F)
write.csv(seine.ts.deep, file = here("Output/seine_ts_EstimateCPS_deep.csv"),
          quote = F, row.names = F, na = "0")

# Summarize target strength proportions by cluster and species
ts.summ.seine <- select(l.summ.set, cluster, haul, scientificName,
                        meanwg, num, sigmawg, sigmaindiv)

ts.summ.seine.deep <- select(l.summ.set.deep, cluster, haul, scientificName,
                        meanwg, num, sigmawg, sigmaindiv)

# Save to file
write_csv(ts.summ.seine, file = here("Output/ts_summ_cluster_seine.csv"))
write_csv(ts.summ.seine.deep, file = here("Output/ts_summ_cluster_seine_deep.csv"))

# Subset sardine results
ts.sub.sar.seine <- filter(ts.summ.seine, scientificName == "Sardinops sagax") %>%
  select(-scientificName)
names(ts.sub.sar.seine) <- paste(names(ts.sub.sar.seine), "sar", sep = ".")
# Subset anchovy results
ts.sub.anch.seine <- filter(ts.summ.seine, scientificName == "Engraulis mordax") %>%
  select(-scientificName)
names(ts.sub.anch.seine) <- paste(names(ts.sub.anch.seine), "anch", sep = ".")
# Subset mackerel results
ts.sub.mack.seine <- filter(ts.summ.seine, scientificName == "Scomber japonicus") %>%
  select(-scientificName)
names(ts.sub.mack.seine) <- paste(names(ts.sub.mack.seine), "mack", sep = ".")
# Subset jack mackerel results
ts.sub.jack.seine <- filter(ts.summ.seine, scientificName == "Trachurus symmetricus") %>%
  select(-scientificName)
names(ts.sub.jack.seine) <- paste(names(ts.sub.jack.seine), "jack", sep = ".")
# Subset jack mackerel results
ts.sub.her.seine <- filter(ts.summ.seine, scientificName == "Clupea pallasii") %>%
  select(-scientificName)
names(ts.sub.her.seine) <- paste(names(ts.sub.her.seine), "her", sep = ".")
# Subset round herring results
ts.sub.rher.seine <- filter(ts.summ.seine, scientificName == "Etrumeus acuminatus") %>%
  select(-scientificName)
names(ts.sub.rher.seine) <- paste(names(ts.sub.rher.seine), "rher", sep = ".")
# Subset "other" results
ts.sub.other.seine <- filter(ts.summ.seine, scientificName == "Other") %>%
  select(-scientificName)
names(ts.sub.other.seine) <- paste(names(ts.sub.other.seine), "other", sep = ".")

# Combine all TS estimates
# Add sardine TS estimates to trawl clusters
clf.seine <- set.clusters %>%
  select(cluster, haul, lat, long, X, Y,) %>%
  left_join(ts.sub.sar.seine, by  = c("cluster" = "cluster.sar", "haul" = "haul.sar")) %>%
  # Add anchovy TS estimates to clf
  left_join(ts.sub.anch.seine, by = c("cluster" = "cluster.anch", "haul" = "haul.anch")) %>%
  # Add mackerel TS estimates to clf
  left_join(ts.sub.mack.seine, by = c("cluster" = "cluster.mack", "haul" = "haul.mack")) %>%
  # Add jack mackerel TS estimates to clf
  left_join(ts.sub.jack.seine, by = c("cluster" = "cluster.jack", "haul" = "haul.jack")) %>%
  # Add herring TS estimates to clf
  left_join(ts.sub.her.seine, by  = c("cluster" = "cluster.her", "haul" = "haul.her")) %>%
  # Add round herring TS estimates to clf
  # left_join(ts.sub.rher.seine, by = c("cluster" = "cluster.rher", "haul" = "haul.rher")) %>%
  # Add other TS estimates to clf
  left_join(ts.sub.other.seine, by = c("cluster" = "cluster.other", "haul" = "haul.other"))

# Add round herring TS estimates to clf, if present in nearshore catches
if (nrow(ts.sub.rher.seine) > 0) {
  clf.seine <- clf.seine %>%
    left_join(ts.sub.rher.seine, by = c("cluster" = "cluster.rher", "haul" = "haul.rher"))
} else {
  clf.seine$meanwg.rher     <- NA
  clf.seine$num.rher        <- NA
  clf.seine$sigmawg.rher    <- NA
  clf.seine$sigmaindiv.rher <- NA
}

# Do deep sets
# Subset sardine results
ts.sub.sar.seine <- filter(ts.summ.seine.deep, scientificName == "Sardinops sagax") %>%
  select(-scientificName)
names(ts.sub.sar.seine) <- paste(names(ts.sub.sar.seine), "sar", sep = ".")
# Subset anchovy results
ts.sub.anch.seine <- filter(ts.summ.seine.deep, scientificName == "Engraulis mordax") %>%
  select(-scientificName)
names(ts.sub.anch.seine) <- paste(names(ts.sub.anch.seine), "anch", sep = ".")
# Subset mackerel results
ts.sub.mack.seine <- filter(ts.summ.seine.deep, scientificName == "Scomber japonicus") %>%
  select(-scientificName)
names(ts.sub.mack.seine) <- paste(names(ts.sub.mack.seine), "mack", sep = ".")
# Subset jack mackerel results
ts.sub.jack.seine <- filter(ts.summ.seine.deep, scientificName == "Trachurus symmetricus") %>%
  select(-scientificName)
names(ts.sub.jack.seine) <- paste(names(ts.sub.jack.seine), "jack", sep = ".")
# Subset jack mackerel results
ts.sub.her.seine <- filter(ts.summ.seine.deep, scientificName == "Clupea pallasii") %>%
  select(-scientificName)
names(ts.sub.her.seine) <- paste(names(ts.sub.her.seine), "her", sep = ".")
# Subset round herring results
ts.sub.rher.seine <- filter(ts.summ.seine.deep, scientificName == "Etrumeus acuminatus") %>%
  select(-scientificName)
names(ts.sub.rher.seine) <- paste(names(ts.sub.rher.seine), "rher", sep = ".")
# Subset "other" results
ts.sub.other.seine <- filter(ts.summ.seine.deep, scientificName == "Other") %>%
  select(-scientificName)
names(ts.sub.other.seine) <- paste(names(ts.sub.other.seine), "other", sep = ".")

# Combine all TS estimates
# Add sardine TS estimates to trawl clusters
clf.seine.deep <- set.clusters %>%
  select(cluster, haul, lat, long, X, Y,) %>%
  left_join(ts.sub.sar.seine, by  = c("cluster" = "cluster.sar", "haul" = "haul.sar")) %>%
  # Add anchovy TS estimates to clf
  left_join(ts.sub.anch.seine, by = c("cluster" = "cluster.anch", "haul" = "haul.anch")) %>%
  # Add mackerel TS estimates to clf
  left_join(ts.sub.mack.seine, by = c("cluster" = "cluster.mack", "haul" = "haul.mack")) %>%
  # Add jack mackerel TS estimates to clf
  left_join(ts.sub.jack.seine, by = c("cluster" = "cluster.jack", "haul" = "haul.jack")) %>%
  # Add herring TS estimates to clf
  left_join(ts.sub.her.seine, by  = c("cluster" = "cluster.her", "haul" = "haul.her")) %>%
  # Add round herring TS estimates to clf
  # left_join(ts.sub.rher.seine, by = c("cluster" = "cluster.rher", "haul" = "haul.rher")) %>%
  # Add other TS estimates to clf
  left_join(ts.sub.other.seine, by = c("cluster" = "cluster.other", "haul" = "haul.other"))

if (nrow(ts.sub.rher.seine) > 0) {
  clf.seine.deep <- clf.seine.deep %>%
    # Add round herring TS estimates to clf
    left_join(ts.sub.rher.seine, by = c("cluster" = "cluster.rher", "haul" = "haul.rher"))
} else {
  clf.seine.deep$meanwg.rher     <- NA
  clf.seine.deep$num.rher        <- NA
  clf.seine.deep$sigmawg.rher    <- NA
  clf.seine.deep$sigmaindiv.rher <- NA
}

# Calculate total CPS number in each cluster
catch.summ.num.seine <- clf.seine %>%
  group_by(cluster, haul) %>%
  summarise(CPS.num = sum(num.sar, num.anch, num.mack,
                          num.jack, num.her, num.rher, num.other,
                          na.rm = TRUE))

catch.summ.num.seine.deep <- clf.seine.deep %>%
  group_by(cluster, haul) %>%
  summarise(CPS.num = sum(num.sar, num.anch, num.mack,
                          num.jack, num.her, num.rher, num.other,
                          na.rm = TRUE))

# Calculate total CPS weight in each cluster
cluster.summ.wt.seine <- lengths.seine %>%
  filter(scientificName %in% cps.spp) %>%
  group_by(cluster, haul) %>%
  summarise(CPS.wg = sum(weightg))

cluster.summ.wt.seine.deep <- lengths.seine.deep %>%
  filter(scientificName %in% cps.spp) %>%
  group_by(cluster, haul) %>%
  summarise(CPS.wg = sum(weightg))

# Replace NA's with 0's
clf.seine[is.na(clf.seine)] <- 0
clf.seine.deep[is.na(clf.seine.deep)] <- 0

# Save to file
write_csv(clf.seine, file = here("Output/clf_cluster_summary_seine.csv"))
write_csv(clf.seine.deep, file = here("Output/clf_cluster_summary_seine_deep.csv"))

# Calculate species proportion (weighted average, by number and weight) in each trawl cluster
ts.proportions.seine <- clf.seine %>%
  group_by(cluster, haul) %>%
  summarise(
    # Calculate the weighted number of each species
    weighted.num = (num.sar     * sigmaindiv.sar  +
                      num.anch    * sigmaindiv.anch +
                      num.her     * sigmaindiv.her  +
                      num.mack    * sigmaindiv.mack +
                      num.jack    * sigmaindiv.jack +
                      num.rher    * sigmaindiv.rher +
                      num.other   * sigmaindiv.other),
    # Calculate the weighted weight of each species
    weighted.wg  = (meanwg.sar  * num.sar  * sigmawg.sar +
                      meanwg.anch  * num.anch  * sigmawg.anch +
                      meanwg.her   * num.her   * sigmawg.her +
                      meanwg.mack  * num.mack  * sigmawg.mack +
                      meanwg.jack  * num.jack  * sigmawg.jack +
                      meanwg.rher  * num.rher  * sigmawg.rher +
                      meanwg.other * num.other * sigmawg.other),
    # Calculate the proportion, by number and weight, for each species
    prop.sar     = (num.sar     * sigmaindiv.sar)  / weighted.num,
    prop.sar.wg  = (meanwg.sar  * sigmawg.sar  * num.sar) / weighted.wg,
    prop.anch    = (num.anch    * sigmaindiv.anch) / weighted.num,
    prop.anch.wg = (meanwg.anch * sigmawg.anch * num.anch) / weighted.wg,
    prop.mack    = (num.mack    * sigmaindiv.mack) / weighted.num,
    prop.mack.wg = (meanwg.mack * sigmawg.mack * num.mack) / weighted.wg,
    prop.jack    = (num.jack    * sigmaindiv.jack) / weighted.num,
    prop.jack.wg = (meanwg.jack * sigmawg.jack * num.jack) / weighted.wg,
    prop.her     = (num.her     * sigmaindiv.her)  / weighted.num,
    prop.her.wg  = (meanwg.her  * sigmawg.her  * num.her) / weighted.wg,
    prop.rher    = (num.rher    * sigmaindiv.rher)  / weighted.num,
    prop.rher.wg = (meanwg.rher * sigmawg.rher  * num.rher) / weighted.wg,
    prop.other   = (num.other   * sigmaindiv.other)  / weighted.num,
    prop.other.wg = (meanwg.other * sigmawg.other  * num.other) / weighted.wg) %>%
  replace(is.na(.), 0)

# Replace all NaNs with zeros
ts.proportions.seine[atm:::is.nan.df(ts.proportions.seine)] <- NA

# Save to file
write_csv(ts.proportions.seine, file = here("Output/ts_proportions_raw_seine.csv"))

# Add cluster weights, numbers, and TS proportions
clf.seine <- clf.seine %>%
  left_join(catch.summ.num.seine,  by = c('haul','cluster')) %>%
  left_join(cluster.summ.wt.seine, by = c('haul','cluster')) %>%
  left_join(ts.proportions.seine,  by = c('haul','cluster')) %>%
  mutate(sample.type = "Purse seine") %>%
  arrange(cluster)

# Replace 0's with 1's for sigmaindiv and sigmawg when proportions == 0
clf.seine$sigmaindiv.anch[clf.seine$prop.anch == 0]   <- 1
clf.seine$sigmaindiv.her[clf.seine$prop.her   == 0]   <- 1
clf.seine$sigmaindiv.jack[clf.seine$prop.jack == 0]   <- 1
clf.seine$sigmaindiv.mack[clf.seine$prop.mack == 0]   <- 1
clf.seine$sigmaindiv.sar[clf.seine$prop.sar   == 0]   <- 1
clf.seine$sigmaindiv.rher[clf.seine$prop.rher == 0]   <- 1
clf.seine$sigmaindiv.other[clf.seine$prop.other == 0] <- 1

clf.seine$sigmawg.anch[clf.seine$prop.anch    == 0]   <- 1
clf.seine$sigmawg.her[clf.seine$prop.her      == 0]   <- 1
clf.seine$sigmawg.jack[clf.seine$prop.jack    == 0]   <- 1
clf.seine$sigmawg.mack[clf.seine$prop.mack    == 0]   <- 1
clf.seine$sigmawg.sar[clf.seine$prop.sar      == 0]   <- 1
clf.seine$sigmawg.rher[clf.seine$prop.rher    == 0]   <- 1
clf.seine$sigmawg.other[clf.seine$prop.other  == 0]   <- 1

# Save to file
write_csv(clf.seine, file = here("Output/clf_ts_proportions_seine.csv"))

# Do deep data
ts.proportions.seine.deep <- clf.seine.deep %>%
  group_by(cluster, haul) %>%
  summarise(
    # Calculate the weighted number of each species
    weighted.num = (num.sar     * sigmaindiv.sar  +
                      num.anch    * sigmaindiv.anch +
                      num.her     * sigmaindiv.her  +
                      num.mack    * sigmaindiv.mack +
                      num.jack    * sigmaindiv.jack +
                      num.rher    * sigmaindiv.rher +
                      num.other   * sigmaindiv.other),
    # Calculate the weighted weight of each species
    weighted.wg  = (meanwg.sar  * num.sar  * sigmawg.sar +
                      meanwg.anch  * num.anch  * sigmawg.anch +
                      meanwg.her   * num.her   * sigmawg.her +
                      meanwg.mack  * num.mack  * sigmawg.mack +
                      meanwg.jack  * num.jack  * sigmawg.jack +
                      meanwg.rher  * num.rher  * sigmawg.rher +
                      meanwg.other * num.other * sigmawg.other),
    # Calculate the proportion, by number and weight, for each species
    prop.sar     = (num.sar     * sigmaindiv.sar)  / weighted.num,
    prop.sar.wg  = (meanwg.sar  * sigmawg.sar  * num.sar) / weighted.wg,
    prop.anch    = (num.anch    * sigmaindiv.anch) / weighted.num,
    prop.anch.wg = (meanwg.anch * sigmawg.anch * num.anch) / weighted.wg,
    prop.mack    = (num.mack    * sigmaindiv.mack) / weighted.num,
    prop.mack.wg = (meanwg.mack * sigmawg.mack * num.mack) / weighted.wg,
    prop.jack    = (num.jack    * sigmaindiv.jack) / weighted.num,
    prop.jack.wg = (meanwg.jack * sigmawg.jack * num.jack) / weighted.wg,
    prop.her     = (num.her     * sigmaindiv.her)  / weighted.num,
    prop.her.wg  = (meanwg.her  * sigmawg.her  * num.her) / weighted.wg,
    prop.rher    = (num.rher    * sigmaindiv.rher)  / weighted.num,
    prop.rher.wg = (meanwg.rher * sigmawg.rher  * num.rher) / weighted.wg,
    prop.other   = (num.other   * sigmaindiv.other)  / weighted.num,
    prop.other.wg = (meanwg.other * sigmawg.other  * num.other) / weighted.wg) %>%
  replace(is.na(.), 0)

# Replace all NaNs with zeros
ts.proportions.seine.deep[atm:::is.nan.df(ts.proportions.seine.deep)] <- NA

# Save to file
write_csv(ts.proportions.seine.deep, file = here("Output/ts_proportions_raw_seine_deep.csv"))

# Add cluster weights, numbers, and TS proportions
clf.seine.deep <- clf.seine.deep %>%
  left_join(catch.summ.num.seine.deep,  by = c('haul','cluster')) %>%
  left_join(cluster.summ.wt.seine.deep, by = c('haul','cluster')) %>%
  left_join(ts.proportions.seine.deep,  by = c('haul','cluster')) %>%
  mutate(sample.type = "Purse seine") %>%
  arrange(cluster)

# Replace 0's with 1's for sigmaindiv and sigmawg when proportions == 0
clf.seine.deep$sigmaindiv.anch[clf.seine.deep$prop.anch == 0]   <- 1
clf.seine.deep$sigmaindiv.her[clf.seine.deep$prop.her   == 0]   <- 1
clf.seine.deep$sigmaindiv.jack[clf.seine.deep$prop.jack == 0]   <- 1
clf.seine.deep$sigmaindiv.mack[clf.seine.deep$prop.mack == 0]   <- 1
clf.seine.deep$sigmaindiv.sar[clf.seine.deep$prop.sar   == 0]   <- 1
clf.seine.deep$sigmaindiv.rher[clf.seine.deep$prop.rher == 0]   <- 1
clf.seine.deep$sigmaindiv.other[clf.seine.deep$prop.other == 0] <- 1

clf.seine.deep$sigmawg.anch[clf.seine.deep$prop.anch    == 0]   <- 1
clf.seine.deep$sigmawg.her[clf.seine.deep$prop.her      == 0]   <- 1
clf.seine.deep$sigmawg.jack[clf.seine.deep$prop.jack    == 0]   <- 1
clf.seine.deep$sigmawg.mack[clf.seine.deep$prop.mack    == 0]   <- 1
clf.seine.deep$sigmawg.sar[clf.seine.deep$prop.sar      == 0]   <- 1
clf.seine.deep$sigmawg.rher[clf.seine.deep$prop.rher    == 0]   <- 1
clf.seine.deep$sigmawg.other[clf.seine.deep$prop.other  == 0]   <- 1

# Save to file
write_csv(clf.seine.deep, file = here("Output/clf_ts_proportions_seine_deep.csv"))

# Calculate length frequencies for each species and purse seine cluster --------------------
# Create a list for storing final species-specific cluster results
cluster.final.seine <- list()
haul.final.seine    <- list()
lf.final.seine      <- data.frame()

# For each species, calculate length frequencies, combine with clf, and write final file
for (i in cps.spp) {
  # for (i in unique(lengths.seine$scientificName)) {
  # Create a data frame for results
  lf.df.seine <- data.frame()
  
  for (ii in unique(lengths.seine$cluster)) {
    # Subset specimen data by species and trawl cluster
    lengths.sub <- droplevels(filter(lengths.seine, scientificName == i & cluster == ii))
    
    # Define length bins
    lf.breaks <- seq(length.min - 0.5, length.max$sl[length.max$species == i] + 0.5, 1)
    lf.labels <- seq(length.min, length.max$sl[length.max$species == i], 1)
    
    # Calculate the proportion (histogram density) of individuals in each size class
    # Setting right = F includes X.5 in the lower class (e.g., 14.5 = 14, 14.6 = 15)
    # Length inputs are native lengths (i.e., SL for sardine and anchovy, FL for herring and meckerels)
    if (i %in% c("Sardinops sagax", "Engraulis mordax")) {
      f <- hist(lengths.sub$standardLength_mm/10, breaks = lf.breaks, plot = F, right = F)$density
    } else if (i %in% c("Scomber japonicus", "Trachurus symmetricus", "Clupea pallasii")) {
      f <- hist(lengths.sub$forkLength_mm/10, breaks = lf.breaks, plot = F, right = F)$density
    }
    
    # Create a vector of size class
    lf.bin <- lf.breaks[1:length(lf.breaks) - 1]
    
    # Create a vector of cluster number
    # haul <- rep(ii, length(f))
    # Add results to data frame
    lf.df.seine <- rbind(lf.df.seine, data.frame(cluster = ii, lf.bin, lf.labels, f))
  }
  
  # Replace NaN values with zeros
  lf.df.seine$f[is.nan(lf.df.seine$f)] <- 0
  
  # Get estimated numbers of individuals for each haul
  n.summ.set.sub <- filter(n.summ.set, as.character(scientificName) == i) %>%
    select(cluster, num)
  
  # Add estimated total number of individuals in each cluster (based on subsample weight)
  # to the length frequency data frame
  lf.df.seine <- left_join(lf.df.seine, n.summ.set.sub) %>%
    rename(spp.num = num) %>%
    replace_na(list(spp.num = 0)) %>%
    mutate(counts = f * spp.num)
  
  # Summarise lengths by cluster, to filter subsequent data frames
  lf.df.seine.summ <- lf.df.seine %>%
    group_by(cluster) %>%
    summarise(nIndiv = sum(counts)) %>%
    filter(nIndiv > 0)
  
  # Calculate the estimated number of individuals in each size class
  lf.df.seine <- lf.df.seine %>%
    mutate(counts         = f * spp.num,
           scientificName = i,
           sample.type    = "Purse seine") %>%
    filter(is.na(cluster) == FALSE) 
  
  # Combine length frequency data for plotting later
  lf.final.seine <- bind_rows(lf.final.seine, lf.df.seine)
  
  # Reshape data frame by cluster for adding to clf
  lf.table.seine <- pivot_wider(lf.df.seine, id_cols = cluster, names_from = lf.labels, values_from = counts, values_fn = sum) %>% 
    rowwise(cluster) %>%
    mutate('(all)' = sum(c_across(all_of(lf.labels)))) %>% 
    arrange(cluster)
  
  # Convert counts to relative frequencies
  lf.table.seine[ , 2:ncol(lf.table.seine)] <- lf.table.seine[ , 2:ncol(lf.table.seine)]/lf.table.seine$`(all)`
  
  # Rename columns
  names(lf.table.seine)[2:ncol(lf.table.seine)] <- paste("L", names(lf.table.seine)[2:ncol(lf.table.seine)], sep = "")
  lf.table.seine <- left_join(select(set.clusters, cluster), lf.table.seine)
  
  # Replace NA values with zeros
  lf.table.seine[is.na(lf.table.seine)] <- 0
  
  # Add length frequencies for each species to clf for final analysis
  clf.final.ns <- left_join(clf.seine, lf.table.seine, by = 'cluster') %>%
    mutate(species = i)
  
  hlf.final.ns <- clf.final.ns %>%
    # left_join(select(set.clusters, cluster, haul)) %>%
    select(cluster, haul, everything()) %>%
    select(-cluster)
  
  # Write final data frame to .csv for each species
  write_csv(clf.final.ns,
            file = paste0(here("Output/cluster_length_frequency_seine_"), i, "_", survey.name, ".csv"))
  
  write_csv(hlf.final.ns,
            file = paste0(here("Output/haul_length_frequency_seine_"), i, "_", survey.name, ".csv"))
  # Add species-specific results to list of results
  cluster.final.seine[[i]] <- clf.final.ns
  haul.final.seine[[i]] <- hlf.final.ns
  
  # Plot length frequencies by species and cluster
  lf.plot.seine <- ggplot(lf.df.seine, aes(lf.labels, counts)) + geom_bar(stat = 'identity') +
    facet_wrap(~cluster, scales = 'free_y') + theme_bw() +
    xlab("Standard length (cm)") + ylab("Counts") + ggtitle(i)
  
  # Save plot
  ggsave(lf.plot.seine,
         filename = paste0(here("Figs/fig_cluster_length_frequency_seine_"), i, ".png"),
         height = 7, width = 10)
}

# Save results
save(n.summ.set, l.summ.set,
     file = here("Output/seine_summaries.Rdata"))
save(cluster.final.seine,
     file = here("Output/cluster_length_frequency_all_seine.Rdata"))
save(haul.final.seine,
     file = here("Output/haul_length_frequency_all_seine.Rdata"))
save(lf.final.seine,
     file = here("Output/cluster_length_frequency_tables_seine.Rdata"))

# Do deep data
# Create a list for storing final species-specific cluster results
cluster.final.seine.deep <- list()
haul.final.seine.deep    <- list()
lf.final.seine.deep      <- data.frame()

# For each species, calculate length frequencies, combine with clf, and write final file
for (i in cps.spp) {
  # for (i in unique(lengths.seine$scientificName)) {
  # Create a data frame for results
  lf.df.seine <- data.frame()

  for (ii in unique(lengths.seine.deep$cluster)) {
    # Subset specimen data by species and trawl cluster
    lengths.sub <- droplevels(filter(lengths.seine.deep, scientificName == i & cluster == ii))

    # Define length bins
    lf.breaks <- seq(length.min - 0.5, length.max$sl[length.max$species == i] + 0.5, 1)
    lf.labels <- seq(length.min, length.max$sl[length.max$species == i], 1)

    # Calculate the proportion (histogram density) of individuals in each size class
    # Setting right = F includes X.5 in the lower class (e.g., 14.5 = 14, 14.6 = 15)
    # Length inputs are native lengths (i.e., SL for sardine and anchovy, FL for herring and meckerels)
    if (i %in% c("Sardinops sagax", "Engraulis mordax")) {
      f <- hist(lengths.sub$standardLength_mm/10, breaks = lf.breaks, plot = F, right = F)$density
    } else if (i %in% c("Scomber japonicus", "Trachurus symmetricus", "Clupea pallasii")) {
      f <- hist(lengths.sub$forkLength_mm/10, breaks = lf.breaks, plot = F, right = F)$density
    }

    # Create a vector of size class
    lf.bin <- lf.breaks[1:length(lf.breaks) - 1]

    # Create a vector of cluster number
    # haul <- rep(ii, length(f))
    # Add results to data frame
    lf.df.seine <- rbind(lf.df.seine, data.frame(cluster = ii, lf.bin, lf.labels, f))
  }

  # Replace NaN values with zeros
  lf.df.seine$f[is.nan(lf.df.seine$f)] <- 0

  # Get estimated numbers of individuals for each haul
  n.summ.set.sub <- filter(n.summ.set.deep, as.character(scientificName) == i) %>%
    select(cluster, num)

  # Add estimated total number of individuals in each cluster (based on subsample weight)
  # to the length frequency data frame
  lf.df.seine <- left_join(lf.df.seine, n.summ.set.sub) %>%
    rename(spp.num = num) %>%
    replace_na(list(spp.num = 0)) %>%
    mutate(counts = f * spp.num)

  # Summarise lengths by cluster, to filter subsequent data frames
  lf.df.seine.summ <- lf.df.seine %>%
    group_by(cluster) %>%
    summarise(nIndiv = sum(counts)) %>%
    filter(nIndiv > 0)

  # Calculate the estimated number of individuals in each size class
  lf.df.seine <- lf.df.seine %>%
    mutate(counts         = f * spp.num,
           scientificName = i,
           sample.type    = "Purse seine") %>%
    filter(is.na(cluster) == FALSE)

  # Combine length frequency data for plotting later
  lf.final.seine.deep <- bind_rows(lf.final.seine.deep, lf.df.seine)

  # Reshape data frame by cluster for adding to clf
  lf.table.seine <- pivot_wider(lf.df.seine, id_cols = cluster, names_from = lf.labels, values_from = counts, values_fn = sum) %>%
    rowwise(cluster) %>%
    mutate('(all)' = sum(c_across(all_of(lf.labels)))) %>%
    arrange(cluster)

  # Convert counts to relative frequencies
  lf.table.seine[ , 2:ncol(lf.table.seine)] <- lf.table.seine[ , 2:ncol(lf.table.seine)]/lf.table.seine$`(all)`

  # Rename columns
  names(lf.table.seine)[2:ncol(lf.table.seine)] <- paste("L", names(lf.table.seine)[2:ncol(lf.table.seine)], sep = "")
  lf.table.seine <- left_join(select(set.clusters, cluster), lf.table.seine)

  # Replace NA values with zeros
  lf.table.seine[is.na(lf.table.seine)] <- 0

  # Add length frequencies for each species to clf for final analysis
  clf.final.ns <- left_join(clf.seine.deep, lf.table.seine, by = 'cluster') %>%
    mutate(species = i)

  hlf.final.ns <- clf.final.ns %>%
    # left_join(select(set.clusters, cluster, haul)) %>%
    select(cluster, haul, everything()) %>%
    select(-cluster)

  # Write final data frame to .csv for each species
  write_csv(clf.final.ns,
            file = paste0(here("Output/cluster_length_frequency_seine_deep_"), i, "_", survey.name, ".csv"))

  write_csv(hlf.final.ns,
            file = paste0(here("Output/haul_length_frequency_seine__deep_"), i, "_", survey.name, ".csv"))
  # Add species-specific results to list of results
  cluster.final.seine.deep[[i]] <- clf.final.ns
  haul.final.seine.deep[[i]] <- hlf.final.ns

  # Plot length frequencies by species and cluster
  lf.plot.seine <- ggplot(lf.df.seine, aes(lf.labels, counts)) + geom_bar(stat = 'identity') +
    facet_wrap(~cluster, scales = 'free_y') + theme_bw() +
    xlab("Standard length (cm)") + ylab("Counts") + ggtitle(i)

  # Save plot
  ggsave(lf.plot.seine,
         filename = paste0(here("Figs/fig_cluster_length_frequency_seine_deep_"), i, ".png"),
         height = 7, width = 10)
}

# Save results
save(n.summ.set.deep, l.summ.set.deep,
     file = here("Output/seine_summaries_deep.Rdata"))
save(cluster.final.seine.deep,
     file = here("Output/cluster_length_frequency_all_seine_deep.Rdata"))
save(haul.final.seine.deep,
     file = here("Output/haul_length_frequency_all_seine_deep.Rdata"))
save(lf.final.seine.deep,
     file = here("Output/cluster_length_frequency_tables_seine_deep.Rdata"))
