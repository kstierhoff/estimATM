# Import purse seine data ----------------------------------------------------
# Import set info
lm.sets <- read_csv(here("Data/Seine/lm_sets.csv")) %>% 
  mutate(date = mdy(date),
         lat = lat_deg + lat_decmin/60,
         long = -(long_deg + long_decmin/60),
         vessel.name = "LM",
         key.set = paste(vessel_name, date, set))

lbc.sets <- read_csv(here("Data/Seine/lbc_sets.csv")) %>% 
  mutate(date = date(mdy_hm(datetime)),
         vessel_name = "Long Beach Carnage",
         vessel.name = "LBC",
         key.set = paste(vessel_name, date, set))

set.clusters <- select(lm.sets, key.set, vessel.name, lat, long) %>% 
  bind_rows(select(lbc.sets, key.set, vessel.name, lat, long)) %>% 
  filter(vessel.name %in% seine.vessels) %>% 
  # Begin clustering after Lasker clusters
  mutate(cluster = max(cluster.mid$cluster) + as.numeric(as.factor(key.set))) %>% 
  project_df(to = crs.proj)

save(lm.sets, lbc.sets, set.clusters, file = here("Output/purse_seine_sets.Rdata"))

# Import specimen info
lm.specimens <- read_csv(here("Data/Seine/lm_catch.csv")) %>% 
  mutate(date = mdy(date),
         key.set = paste(vessel_name, date, set),
         vessel.name = "LM",
         label = paste("Date:", date, "Set:", set, "Fish num:", specimen_number),
         scientificName = case_when(
           common_name == "Pacific Herring" ~ "Clupea pallasii",
           common_name == "Pacific Sardine" ~ "Sardinops sagax",
           common_name == "Jack Mackerel" ~ "Trachurus symmetricus",
           common_name == "Northern Anchovy" ~ "Engraulis mordax"
         ),
         totalLength_mm = case_when(
           scientificName == "Clupea pallasii"       ~ -0.323 + 1.110*forkLength_mm,
           scientificName == "Engraulis mordax"      ~  2.056 + 1.165*standardLength_mm,
           scientificName == "Sardinops sagax"       ~  3.574 + 1.149*standardLength_mm,
           scientificName == "Scomber japonicus"     ~  2.994 + 1.092*forkLength_mm,
           scientificName == "Trachurus symmetricus" ~  7.295 + 1.078*forkLength_mm),
         missing.weight = case_when(is.na(weightg)   ~ T, TRUE ~ FALSE),
         missing.length = case_when(is.na(totalLength_mm) ~ T, TRUE ~ FALSE)) %>% 
  filter(!is.na(scientificName), !is.na(set))

lbc.specimens <- read_csv(here("Data/Seine/lbc_catch.csv")) %>% 
  left_join(select(lbc.sets, date, set, key.set)) %>% 
  mutate(vessel.name = "LBC",
         label = paste("Date:", date, "Set:", set, "Specimen num:", specimen_number),
         totalLength_mm = case_when(
           scientificName == "Clupea pallasii"       ~ -0.323 + 1.110*forkLength_mm,
           scientificName == "Engraulis mordax"      ~  2.056 + 1.165*standardLength_mm,
           scientificName == "Sardinops sagax"       ~  3.574 + 1.149*standardLength_mm,
           scientificName == "Scomber japonicus"     ~  2.994 + 1.092*forkLength_mm,
           scientificName == "Trachurus symmetricus" ~  7.295 + 1.078*forkLength_mm),
         missing.weight = case_when(is.na(weightg)   ~ T, TRUE ~ FALSE),
         missing.length = case_when(is.na(totalLength_mm) ~ T, TRUE ~ FALSE)) %>% 
  filter(!is.na(scientificName), !is.na(set))  

# Estimate missing weights from lengths -------------------------------------------------------
# Add a and b to length data frame and calculate missing lengths/weights
lm.specimens <- lm.specimens %>% 
  left_join(lw.models, by = 'scientificName') %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ a*totalLength_mm^b,
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ (weightg/a)^(1/b),
      TRUE ~ totalLength_mm),
    K = (weightg/totalLength_mm*10^3)*100)

lbc.specimens <- lbc.specimens %>% 
  left_join(lw.models, by = 'scientificName') %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ a*totalLength_mm^b,
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ (weightg/a)^(1/b),
      TRUE ~ totalLength_mm),
    K = (weightg/totalLength_mm*10^3)*100)

# Summarize specimen data ------------------------------------------------
lm.spec.summ <- lm.specimens %>%
  group_by(key.set, vessel.name, scientificName) %>% 
  summarise(totalWeight = sum(weightg),
            totalCount  = n()) %>% 
  ungroup() %>% 
  left_join(select(lm.sets, key.set, lat, long)) %>% 
  filter(!is.na(lat)) %>% 
  mutate()

lbc.spec.summ <- lbc.specimens %>%
  group_by(key.set, vessel.name, scientificName) %>% 
  summarise(totalWeight = sum(weightg),
            totalCount  = n()) %>% 
  ungroup() %>% 
  left_join(select(lbc.sets, key.set, lat, long)) %>% 
  filter(!is.na(lat))

# Make specimen summaries spatial -----------------------------------------
lm.spec.summ.sf <- lm.spec.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

lbc.spec.summ.sf <- lbc.spec.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

# Summarise catch by weight -----------------------------------------------
set.summ.wt <- lm.spec.summ %>% 
  bind_rows(lbc.spec.summ) %>% 
  filter(vessel.name %in% seine.vessels) %>% 
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
  mutate(AllCPS = rowSums(select(., -(key.set:long)))) %>%
  # mutate(AllCPS = rowSums(select(., -key.set, -totalCount, -lat, -long))) %>%
  # mutate(AllCPS = rowSums(.[, 3:ncol(.)])) %>%
  rename("Jacksmelt"  = "Atherinopsis californiensis",
         "PacHerring" = "Clupea pallasii",
         "Anchovy"    = "Engraulis mordax",
         "Sardine"    = "Sardinops sagax",
         "PacMack"    = "Scomber japonicus",
         "JackMack"   = "Trachurus symmetricus") %>% 
  left_join(select(set.clusters, key.set, cluster)) %>% 
  select(-key.set, -vessel.name, -totalCount, -lat, -long, -AllCPS) %>%
  group_by(cluster) %>% 
  summarise_all(list(sum)) %>% 
  mutate(AllCPS = rowSums(select(., -cluster))) %>% 
  right_join(select(set.clusters, -vessel.name)) %>% 
  replace(is.na(.), 0)

set.pie <- set.summ.wt %>% 
  select(cluster, long, lat, Anchovy, JackMack, 
         Jacksmelt, PacHerring, PacMack, Sardine, AllCPS) %>% 
  project_df(to = crs.proj) %>% 
  mutate(
    label = paste("Cluster", cluster),
    popup = paste('<b>Cluster:', cluster, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'All CPS:', AllCPS, 'kg'))

set.pos <- filter(set.pie, AllCPS > 0) %>% 
  arrange(desc(X))

set.zero <- filter(set.pie, AllCPS == 0)

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(set.pos) > 0) {
  set.pos <- set.pos %>% 
    replace(. == 0, 0.0000001) 
}

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.pie$radius    <- pie.radius*set.pie$bin
} else {
  set.pie$radius    <- pie.radius
}

# Select positive clusters
super.clusters.ns <- filter(set.clusters, cluster %in% unique(set.pos$cluster)) %>% 
  select(-key.set, -vessel.name)

# Subset data from all platforms
lm.lengths <- lm.specimens %>% 
  select(key.set, scientificName, totalLength_mm, weightg) %>% 
  mutate(vessel.name = "LM")

lbc.lengths <- lbc.specimens %>% 
  select(key.set, scientificName, totalLength_mm, weightg) %>% 
  mutate(vessel.name = "LBC")

# Combine data from all platforms and filter unwanted vessels
lengths.seine <- bind_rows(lm.lengths, lbc.lengths) %>% 
  filter(vessel.name %in% seine.vessels)

# Estimate TS
ts.df.seine <- estimate_ts(lengths.seine$scientificName, lengths.seine$totalLength_mm)

# Add TS estimates to lengths data frame
lengths.seine <- bind_cols(lengths.seine, select(ts.df.seine, -species, -TL)) %>% 
  arrange(vessel.name, scientificName, id) %>% 
  filter(vessel.name %in% seine.vessels) %>% 
  # mutate(cluster = as.numeric(as.factor(key.set)))
  left_join(select(set.clusters, key.set, cluster))

# Calculate cluster length frequencies for purse seine data ---------------
# Calculate numeric abundance by species and set
n.summ.set <- lengths.seine %>% 
  group_by(scientificName, cluster) %>% 
  tally(name = "num") %>% 
  ungroup()

# Summarise weights and sigmas by haul
l.summ.set <- lengths.seine %>% 
  group_by(scientificName, cluster) %>% 
  summarise(
    meanwg       = mean(estimated.wg),
    sigmaindiv   = mean(sigma.ind),
    sigmawg      = sum(estimated.wg*sigma.wg)/sum(estimated.wg)) %>% 
  left_join(n.summ.set) %>% 
  ungroup()

# Create a data frame with abbreviations
trawl.ts.names <- data.frame(
  scientificName = c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                     "Scomber japonicus","Trachurus symmetricus"),
  shortName      = c("her","anch","sar",
                     "mack","jack"))

# Remove seine TS data
if (exists("seine.ts")) rm(seine.ts)

# Combine results to compare with Juan's output
for (k in unique(l.summ.set$scientificName)) {
  # Summarize length summary by species
  l.summ.tmp <- l.summ.set %>%
    filter(scientificName == k) %>% 
    select(cluster, sigmawg, sigmaindiv)
  
  # Rename columns per species
  names(l.summ.tmp)[2:3] <- paste(names(l.summ.tmp)[2:3], 
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

# Write results to CSV
write.csv(n.summ.set, file = here("Output/n_summ_set.csv"), 
          quote = F, row.names = F)
write.csv(l.summ.set, file = here("Output/l_summ_set.csv"), 
          quote = F, row.names = F)
write.csv(seine.ts, file = here("Output/seine_ts_EstimateCPS.csv"), 
          quote = F, row.names = F, na = "0")

# Summarize target strength proportions by cluster and species
ts.summ.seine <- select(l.summ.set, cluster, scientificName, 
                        meanwg, num, sigmawg, sigmaindiv) 

# Save to file
write.csv(ts.summ.seine, file = here("Output/ts_summ_cluster_seine.csv"), 
          quote = F, row.names = F)

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

# Combine all TS estimates
# Add sardine TS estimates to trawl clusters
clf.seine <- set.clusters %>%
  select(cluster, lat, long, X, Y,) %>% 
  left_join(ts.sub.sar.seine, by  = c("cluster" = "cluster.sar")) %>% 
  # Add anchovy TS estimates to clf
  left_join(ts.sub.anch.seine, by = c("cluster" = "cluster.anch")) %>% 
  # Add mackerel TS estimates to clf
  left_join(ts.sub.mack.seine, by = c("cluster" = "cluster.mack")) %>% 
  # Add jack mackerel TS estimates to clf
  left_join(ts.sub.jack.seine, by = c("cluster" = "cluster.jack")) %>% 
  # Add herring TS estimates to clf
  left_join(ts.sub.her.seine, by  = c("cluster" = "cluster.her"))

# Calculate total CPS number in each cluster
catch.summ.num.seine <- clf.seine %>% 
  group_by(cluster) %>% 
  summarise(CPS.num = sum(num.sar, num.anch, num.mack, 
                          num.jack, num.her, na.rm = TRUE))

# Calculate total CPS weight in each cluster
cluster.summ.wt.seine <- lengths.seine %>% 
  filter(scientificName %in% cps.spp) %>% 
  group_by(cluster) %>% 
  summarise(CPS.wg = sum(weightg))

# Replace NA's with 0's
clf.seine[is.na(clf.seine)] <- 0

# Save to file
write.csv(clf.seine, file = here("Output/clf_cluster_summary_seine.csv"), 
          quote = F, row.names = F)  

# Calculate species proportion (weighted average, by number and weight) in each trawl cluster  
ts.proportions.seine <- clf.seine %>% 
  group_by(cluster) %>% 
  summarise(
    # Calculate the weighted number of each species
    weighted.num = (num.sar     * sigmaindiv.sar  + 
                      num.anch    * sigmaindiv.anch +
                      num.her     * sigmaindiv.her  +
                      num.mack    * sigmaindiv.mack + 
                      num.jack    * sigmaindiv.jack),
    # Calculate the weighted weight of each species
    weighted.wg  = (meanwg.sar  * num.sar  * sigmawg.sar +
                      meanwg.anch * num.anch * sigmawg.anch + 
                      meanwg.her  * num.her  * sigmawg.her +
                      meanwg.mack * num.mack * sigmawg.mack + 
                      meanwg.jack * num.jack * sigmawg.jack),
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
    prop.her.wg  = (meanwg.her  * sigmawg.her  * num.her) / weighted.wg)

# Replace all NaNs with zeros
ts.proportions.seine[atm:::is.nan.df(ts.proportions.seine)] <- NA

# Save to file
write.csv(ts.proportions.seine, file = here("Output/ts_proportions_raw_seine.csv"), 
          row.names = F, quote = F)

# Add cluster weights, numbers, and TS proportions
clf.seine <- clf.seine %>% 
  left_join(catch.summ.num.seine,  by = 'cluster') %>% 
  left_join(cluster.summ.wt.seine, by = 'cluster') %>% 
  left_join(ts.proportions.seine,  by = 'cluster')

# Replace 0's with 1's for sigmaindiv and sigmawg when proportions == 0
clf.seine$sigmaindiv.anch[clf.seine$prop.anch == 0] <- 1
clf.seine$sigmaindiv.her[clf.seine$prop.her   == 0] <- 1
clf.seine$sigmaindiv.jack[clf.seine$prop.jack == 0] <- 1
clf.seine$sigmaindiv.mack[clf.seine$prop.mack == 0] <- 1
clf.seine$sigmaindiv.sar[clf.seine$prop.sar   == 0] <- 1

clf.seine$sigmawg.anch[clf.seine$prop.anch    == 0] <- 1
clf.seine$sigmawg.her[clf.seine$prop.her      == 0] <- 1
clf.seine$sigmawg.jack[clf.seine$prop.jack    == 0] <- 1
clf.seine$sigmawg.mack[clf.seine$prop.mack    == 0] <- 1
clf.seine$sigmawg.sar[clf.seine$prop.sar      == 0] <- 1

# Save to file
write.csv(clf.seine, file = here("Output/clf_ts_proportions_seine.csv"),
          row.names = F, quote = F)
