# Find midpoint of each haul as the mean lat/lon
haul.mid <- haul %>% 
  group_by(cluster, haul) %>% 
  summarise(
    lat  = mean(c(startLatDecimal, stopLatDecimal)),
    long = mean(c(startLongDecimal, stopLongDecimal)))

# Find midpoint of each haul cluster as the average of haul midpoints
cluster.mid <- haul.mid %>% 
  group_by(cluster) %>% 
  summarise(
    lat  = mean(lat),
    long = mean(long)) %>% 
  project_df(to = 3310)

# Summarize trawl catch by species
haul.summ.wt <- catch %>% 
  select(haul, cluster, scientificName, totalWeight) %>% 
  tidyr::spread(scientificName, totalWeight) 

# Add species with zero total weight
if (!has_name(haul.summ.wt, "Engraulis mordax"))      {haul.summ.wt$`Engraulis mordax`      <- 0}
if (!has_name(haul.summ.wt, "Sardinops sagax"))       {haul.summ.wt$`Sardinops sagax`       <- 0}
if (!has_name(haul.summ.wt, "Scomber japonicus"))     {haul.summ.wt$`Scomber japonicus`     <- 0}
if (!has_name(haul.summ.wt, "Trachurus symmetricus")) {haul.summ.wt$`Trachurus symmetricus` <- 0}
if (!has_name(haul.summ.wt, "Clupea pallasii"))       {haul.summ.wt$`Clupea pallasii`       <- 0}
if (!has_name(haul.summ.wt, "Atherinopsis californiensis")) {haul.summ.wt$`Atherinopsis californiensis` <- 0}

# Calculate total weight of all CPS species
haul.summ.wt <- haul.summ.wt %>%  
  replace(is.na(.), 0) %>% 
  mutate(AllCPS = rowSums(select(., -haul, -cluster))) %>%
  # mutate(AllCPS = rowSums(.[, 3:ncol(.)])) %>%
  rename("Jacksmelt"  = "Atherinopsis californiensis",
         "PacHerring" = "Clupea pallasii",
         "Anchovy"    = "Engraulis mordax",
         "Sardine"    = "Sardinops sagax",
         "PacMack"    = "Scomber japonicus",
         "JackMack"   = "Trachurus symmetricus") 

# Summarise catch by cluster
cluster.summ.wt <- haul.summ.wt %>% 
  select(-haul, -AllCPS) %>% 
  group_by(cluster) %>% 
  summarise_all(list(sum)) %>% 
  mutate(AllCPS = rowSums(select(., -cluster))) %>% 
  right_join(cluster.mid) %>% 
  replace(is.na(.), 0)

# Add lat/long to haul summary for plotting
haul.summ.wt <- haul.summ.wt %>% 
  right_join(haul.mid) %>% 
  replace(is.na(.), 0)

# Prepare catch data for plotting ----------------------------------------------
# Select and rename trawl data for pie charts
haul.pie <- haul.summ.wt %>% 
  select(haul, long, lat, Anchovy, JackMack, 
         Jacksmelt, PacHerring, PacMack, Sardine, AllCPS) %>% 
  # mutate(bin       = cut(AllCPS, trawl.breaks, include.lowest = TRUE),
  #        bin.level = as.numeric(bin)) %>% 
  arrange(haul) %>% 
  project_df(to = 3310) %>% 
  mutate(
    label = paste("Haul", haul),
    popup = paste('<b>Cluster:', haul, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'All CPS:', AllCPS, 'kg'))

cluster.pie <- cluster.summ.wt %>% 
  select(cluster, long, lat, Anchovy, JackMack, 
         Jacksmelt, PacHerring, PacMack, Sardine, AllCPS) %>% 
  # mutate(bin       = cut(AllCPS, trawl.breaks, include.lowest = TRUE),
  #        bin.level = as.numeric(bin)) %>% 
  project_df(to = 3310) %>% 
  mutate(
    label = paste("Cluster", cluster),
    popup = paste('<b>Cluster:', cluster, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'All CPS:', AllCPS, 'kg'))

# Filter for empty trawls
haul.zero    <- filter(haul.pie, AllCPS == 0)

cluster.zero <- filter(cluster.pie, AllCPS == 0)
