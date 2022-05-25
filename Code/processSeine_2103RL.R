# Import purse seine data ----------------------------------------------------
# Import set info
lbc.sets <- read_csv(here("Data/Seine/lbc_sets.csv")) %>% 
  mutate(date = date(mdy_hm(datetime)),
         vessel_name = "Long Beach Carnage",
         vessel.name = "LBC",
         key.set = paste(vessel_name, date, set))

set.clusters <- select(lbc.sets, key.set, vessel.name, lat, long) %>% 
  filter(vessel.name %in% seine.vessels) %>% 
  # Begin clustering after Lasker clusters
  mutate(cluster = max(cluster.mid$cluster) + as.numeric(as.factor(key.set)),
         haul    = max(haul.mid$haul) + as.numeric(as.factor(key.set))) %>% 
  project_df(to = crs.proj)

save(lbc.sets, set.clusters, file = here("Output/purse_seine_sets.Rdata"))

# Import specimen info
lbc.specimens <- read_csv(here("Data/Seine/lbc_catch.csv")) %>% 
  left_join(select(lbc.sets, date, set, key.set)) %>% 
  mutate(vessel.name = "LBC",
         label = paste("Date:", date, "Set:", set, "Specimen num:", specimen_number),
         totalLength_mm = case_when(
           scientificName == "Clupea pallasii"       ~ -0.323 + 1.110*forkLength_mm,
           scientificName == "Engraulis mordax"      ~  2.056 + 1.165*standardLength_mm,
           scientificName == "Sardinops sagax"       ~  3.574 + 1.149*standardLength_mm,
           scientificName == "Scomber japonicus"     ~  2.994 + 1.092*forkLength_mm,
           scientificName == "Trachurus symmetricus" ~  7.295 + 1.078*forkLength_mm,
           scientificName == "Etrumeus acuminatus"   ~ -0.323 + 1.110*forkLength_mm),
         missing.weight = case_when(is.na(weightg)   ~ T, TRUE ~ FALSE),
         missing.length = case_when(is.na(totalLength_mm) ~ T, TRUE ~ FALSE)) %>% 
  filter(!is.na(scientificName), !is.na(set))  

# Estimate missing weights from lengths -------------------------------------------------------
# Add a and b to length data frame and calculate missing lengths/weights
lbc.specimens <- lbc.specimens %>% 
  mutate(
    weightg = case_when(
      is.na(weightg) ~ estimate_weight(.$scientificName, .$totalLength_mm, season = tolower(survey.season)),
      TRUE  ~ weightg),
    totalLength_mm = case_when(
      is.na(totalLength_mm) ~ estimate_length(.$scientificName, .$weightg, season = tolower(survey.season)),
      TRUE ~ totalLength_mm),
    K = round((weightg/totalLength_mm*10^3)*100))

save(lbc.specimens, file = here("Output/purse_seine_specimens.Rdata"))

# Summarize specimen data ------------------------------------------------
lbc.spec.summ <- lbc.specimens %>%
  group_by(key.set, vessel.name, scientificName) %>% 
  summarise(totalWeight = sum(weightg),
            totalCount  = n()) %>% 
  ungroup() %>% 
  left_join(select(lbc.sets, key.set, lat, long)) %>% 
  filter(!is.na(lat))

# Make specimen summaries spatial -----------------------------------------
lbc.spec.summ.sf <- lbc.spec.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

# Summarise catch by weight -----------------------------------------------
set.summ.wt <- lbc.spec.summ %>% 
  filter(vessel.name %in% seine.vessels) %>% 
  tidyr::spread(scientificName, totalWeight) 

# Add species with zero total weight
if (!has_name(set.summ.wt, "Engraulis mordax"))      {set.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(set.summ.wt, "Sardinops sagax"))       {set.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(set.summ.wt, "Scomber japonicus"))     {set.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(set.summ.wt, "Trachurus symmetricus")) {set.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(set.summ.wt, "Clupea pallasii"))       {set.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(set.summ.wt, "Atherinopsis californiensis")) {set.summ.wt$`Atherinopsis californiensis` <- 0}
if (!has_name(set.summ.wt, "Etrumeus acuminatus"))   {set.summ.wt$`Etrumeus acuminatus` <- 0}

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
         "JackMack"   = "Trachurus symmetricus",
         "RndHerring" = "Etrumeus acuminatus") %>% 
  left_join(select(set.clusters, key.set, cluster, haul)) %>% 
  select(-key.set, -vessel.name, -totalCount, -lat, -long, -AllCPS) %>%
  group_by(cluster, haul) %>% 
  summarise_all(list(sum)) %>% 
  ungroup() %>% 
  mutate(AllCPS = rowSums(select(., -cluster, -haul))) %>% 
  right_join(select(set.clusters, -vessel.name)) %>% 
  replace(is.na(.), 0)

set.pie <- set.summ.wt %>% 
  select(cluster, haul, long, lat, Anchovy, JackMack, 
         Jacksmelt, PacHerring, PacMack, RndHerring, Sardine, AllCPS) %>% 
  project_df(to = crs.proj) %>% 
  mutate(
    label = paste("Cluster", cluster),
    popup = paste('<b>Cluster:', cluster, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'R. herring:', RndHerring, 'kg<br/>',
                  'All CPS:', AllCPS, 'kg'))

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.pie$r    <- pie.radius*log(set.pie$bin.level+1)
} else {
  set.pie$r    <- pie.radius
}

set.pos <- filter(set.pie, AllCPS > 0) %>% 
  arrange(desc(X))

set.zero <- filter(set.pie, AllCPS == 0)

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(set.pos) > 0) {
  set.pos <- set.pos %>% 
    replace(. == 0, 0.0000001) 
}

# Select positive clusters
super.clusters.ns <- filter(set.clusters, cluster %in% unique(set.pos$cluster)) %>% 
  select(-key.set, -vessel.name)

# Subset data from all platforms
lbc.lengths <- lbc.specimens %>% 
  select(key.set, scientificName, standardLength_mm, forkLength_mm, totalLength_mm, weightg) %>% 
  mutate(vessel.name = "LBC")

# Combine data from all platforms and filter unwanted vessels
lengths.seine <- lbc.lengths %>% 
  filter(vessel.name %in% seine.vessels)

# Estimate TS
ts.df.seine <- estimate_ts(lengths.seine$scientificName, lengths.seine$totalLength_mm)

# Add TS estimates to lengths data frame
lengths.seine <- bind_cols(lengths.seine, select(ts.df.seine, -species, -TL)) %>% 
  arrange(vessel.name, scientificName, id) %>% 
  filter(vessel.name %in% seine.vessels) %>% 
  # mutate(cluster = as.numeric(as.factor(key.set)))
  left_join(select(set.clusters, key.set, cluster, haul))

# Calculate cluster length frequencies for purse seine data ---------------
# Calculate numeric abundance by species and set
n.summ.set <- lengths.seine %>% 
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

# Create a data frame with abbreviations
trawl.ts.names <- data.frame(
  scientificName = c("Clupea pallasii","Engraulis mordax","Etrumeus acuminatus",
                     "Sardinops sagax","Scomber japonicus","Trachurus symmetricus"),
  shortName      = c("her","anch","rher",
                     "sar","mack","jack"))

# Remove seine TS data
if (exists("seine.ts")) rm(seine.ts)

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

# Write results to CSV
write.csv(n.summ.set, file = here("Output/n_summ_set.csv"), 
          quote = F, row.names = F)
write.csv(l.summ.set, file = here("Output/l_summ_set.csv"), 
          quote = F, row.names = F)
write.csv(seine.ts, file = here("Output/seine_ts_EstimateCPS.csv"), 
          quote = F, row.names = F, na = "0")

# Summarize target strength proportions by cluster and species
ts.summ.seine <- select(l.summ.set, cluster, haul, scientificName, 
                        meanwg, num, sigmawg, sigmaindiv) 

# Save to file
write_csv(ts.summ.seine, file = here("Output/ts_summ_cluster_seine.csv"))

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
ts.sub.rher <- filter(ts.summ, scientificName == "Etrumeus acuminatus") %>% 
  select(-scientificName)
names(ts.sub.rher) <- paste(names(ts.sub.rher), "rher", sep = ".")

# Combine all TS estimates
# Add sardine TS estimates to trawl clusters
clf.seine <- set.clusters %>%
  select(cluster, haul, lat, long, X, Y,) %>% 
  left_join(ts.sub.sar.seine,  by  = c("cluster" = "cluster.sar", "haul" = "haul.sar")) %>% 
  # Add anchovy TS estimates to clf
  left_join(ts.sub.anch.seine, by = c("cluster" = "cluster.anch", "haul" = "haul.anch")) %>%  
  # Add mackerel TS estimates to clf
  left_join(ts.sub.mack.seine, by = c("cluster" = "cluster.mack", "haul" = "haul.mack")) %>% 
  # Add jack mackerel TS estimates to clf
  left_join(ts.sub.jack.seine, by = c("cluster" = "cluster.jack", "haul" = "haul.jack")) %>% 
  # Add herring TS estimates to clf
  left_join(ts.sub.her.seine,  by  = c("cluster" = "cluster.her", "haul" = "haul.her")) %>% 
  # Add round herring TS estimates to clf
  left_join(ts.sub.rher,       by  = c("cluster" = "cluster.rher"))

# Calculate total CPS number in each cluster
catch.summ.num.seine <- clf.seine %>% 
  group_by(cluster, haul) %>% 
  summarise(CPS.num = sum(num.sar, num.anch, num.mack, 
                          num.jack, num.her, num.rher, na.rm = TRUE))

# Calculate total CPS weight in each cluster
cluster.summ.wt.seine <- lengths.seine %>% 
  filter(scientificName %in% cps.spp) %>% 
  group_by(cluster, haul) %>% 
  summarise(CPS.wg = sum(weightg))

# Replace NA's with 0's
clf.seine[is.na(clf.seine)] <- 0

# Save to file
write_csv(clf.seine, file = here("Output/clf_cluster_summary_seine.csv"))  

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
                      num.rher    * sigmaindiv.rher),
    # Calculate the weighted weight of each species
    weighted.wg  = (meanwg.sar  * num.sar  * sigmawg.sar +
                      meanwg.anch * num.anch * sigmawg.anch + 
                      meanwg.her  * num.her  * sigmawg.her +
                      meanwg.mack * num.mack * sigmawg.mack + 
                      meanwg.jack * num.jack * sigmawg.jack + 
                      meanwg.rher * num.rher * sigmawg.rher),
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
    prop.rher.wg = (meanwg.rher * sigmawg.rher  * num.rher) / weighted.wg)

# Replace all NaNs with zeros
ts.proportions.seine[atm:::is.nan.df(ts.proportions.seine)] <- NA

# Save to file
write_csv(ts.proportions.seine, file = here("Output/ts_proportions_raw_seine.csv"))

# Add cluster weights, numbers, and TS proportions
clf.seine <- clf.seine %>% 
  left_join(catch.summ.num.seine,  by = c('haul','cluster')) %>% 
  left_join(cluster.summ.wt.seine, by = c('haul','cluster')) %>% 
  left_join(ts.proportions.seine,  by = c('haul','cluster')) %>% 
  mutate(sample.type = "Purse seine")

# Replace 0's with 1's for sigmaindiv and sigmawg when proportions == 0
clf.seine$sigmaindiv.anch[clf.seine$prop.anch == 0] <- 1
clf.seine$sigmaindiv.her[clf.seine$prop.her   == 0] <- 1
clf.seine$sigmaindiv.jack[clf.seine$prop.jack == 0] <- 1
clf.seine$sigmaindiv.mack[clf.seine$prop.mack == 0] <- 1
clf.seine$sigmaindiv.sar[clf.seine$prop.sar   == 0] <- 1
clf.seine$sigmaindiv.rher[clf.seine$prop.rher == 0] <- 1

clf.seine$sigmawg.anch[clf.seine$prop.anch    == 0] <- 1
clf.seine$sigmawg.her[clf.seine$prop.her      == 0] <- 1
clf.seine$sigmawg.jack[clf.seine$prop.jack    == 0] <- 1
clf.seine$sigmawg.mack[clf.seine$prop.mack    == 0] <- 1
clf.seine$sigmawg.sar[clf.seine$prop.sar      == 0] <- 1
clf.seine$sigmawg.rher[clf.seine$prop.rher    == 0] <- 1

# Save to file
write_csv(clf.seine, file = here("Output/clf_ts_proportions_seine.csv"))

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
    } else if (i %in% c("Scomber japonicus", "Trachurus symmetricus", "Clupea pallasii","Etrumeus acuminatus")) {
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
    mutate(counts = f * spp.num,
           scientificName = i) %>% 
    filter(is.na(cluster) == FALSE)
  
  # Combine length frequency data for plotting later
  lf.final.seine <- bind_rows(lf.final.seine, lf.df.seine)
  
  # Reshape data frame by cluster for adding to clf
  lf.table.seine <- reshape2::dcast(lf.df.seine, cluster ~ lf.labels, value.var = 'counts', sum, margins = 'lf.labels')
  
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
