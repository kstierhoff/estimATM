# Summarize target strength proportions by trawl haul and species --------------
ts.summ.h <- select(l.summ.haul, haul, scientificName, 
                    meanwg, num, sigmawg, sigmaindiv) %>% 
  ungroup()

# Save to file
write.csv(ts.summ.h, file = here("Output/ts_summ_haul.csv"), 
          quote = FALSE, row.names = FALSE)

# Subset sardine results
ts.sub.sar <- filter(ts.summ.h, scientificName == "Sardinops sagax") %>% 
  select(-scientificName)
names(ts.sub.sar) <- paste(names(ts.sub.sar), "sar", sep = ".")
# Subset anchovy results
ts.sub.anch <- filter(ts.summ.h, scientificName == "Engraulis mordax") %>% 
  select(-scientificName)
names(ts.sub.anch) <- paste(names(ts.sub.anch), "anch", sep = ".")
# Subset mackerel results
ts.sub.mack <- filter(ts.summ.h, scientificName == "Scomber japonicus") %>% 
  select(-scientificName)
names(ts.sub.mack) <- paste(names(ts.sub.mack), "mack", sep = ".")
# Subset jack mackerel results
ts.sub.jack <- filter(ts.summ.h, scientificName == "Trachurus symmetricus") %>% 
  select(-scientificName)
names(ts.sub.jack) <- paste(names(ts.sub.jack), "jack", sep = ".") 
# Subset jack mackerel results
ts.sub.her <- filter(ts.summ.h, scientificName == "Clupea pallasii") %>% 
  select(-scientificName)
names(ts.sub.her) <- paste(names(ts.sub.her), "her", sep = ".") 

# Combine all TS estimates ----------------------------------------------------
# Add sardine TS estimates to individual trawls
hlf <- haul.mid %>% 
  left_join(ts.sub.sar,by  = c("haul" = "haul.sar")) %>% 
  # Add anchovy TS estimates to hlf
  left_join(ts.sub.anch,by = c("haul" = "haul.anch")) %>% 
  # Add mackerel TS estimates to hlf
  left_join(ts.sub.mack,by = c("haul" = "haul.mack")) %>% 
  # Add jack mackerel TS estimates to hlf
  left_join(ts.sub.jack,by = c("haul" = "haul.jack")) %>% 
  # Add herring TS estimates to hlf
  left_join(ts.sub.her,by  = c("haul" = "haul.her"))

# Calculate total CPS number in each haul
catch.summ.num.h <- hlf %>% 
  group_by(haul) %>% 
  summarise(CPS.num = sum(num.sar, num.anch, num.mack, 
                          num.jack, num.her, na.rm = TRUE))

# Calculate total CPS weight in each cluster
haul.summ.wt <- catch %>% 
  filter(scientificName %in% cps.spp) %>% 
  group_by(haul) %>% 
  summarise(CPS.wg = sum(totalWeight))

# Replace NA's with 0's
hlf[is.na(hlf)] <- 0

# Save to file
write_csv(hlf, file = here("Output/hlf_cluster_summary.csv"))

# Calculate species proportion (weighted average, by number and weight) 
# in each trawl haul  
ts.proportions.h <- hlf %>% 
  group_by(haul) %>% 
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
ts.proportions.h[atm:::is.nan.df(ts.proportions.h)] <- NA

# Save to file
write_csv(ts.proportions.h, file = here("Output/ts_proportions_raw_haul.csv"))

# Add cluster weights, numbers, and TS proportions
hlf <- hlf %>% 
  left_join(catch.summ.num.h,  by = 'haul') %>% 
  left_join(haul.summ.wt, by = 'haul') %>% 
  left_join(ts.proportions.h,  by = 'haul')

# Replace 0's with 1's for sigmaindiv and sigmawg when proportions == 0
hlf$sigmaindiv.anch[hlf$prop.anch == 0] <- 1
hlf$sigmaindiv.her[hlf$prop.her   == 0] <- 1
hlf$sigmaindiv.jack[hlf$prop.jack == 0] <- 1
hlf$sigmaindiv.mack[hlf$prop.mack == 0] <- 1
hlf$sigmaindiv.sar[hlf$prop.sar   == 0] <- 1

hlf$sigmawg.anch[hlf$prop.anch    == 0] <- 1
hlf$sigmawg.her[hlf$prop.her      == 0] <- 1
hlf$sigmawg.jack[hlf$prop.jack    == 0] <- 1
hlf$sigmawg.mack[hlf$prop.mack    == 0] <- 1
hlf$sigmawg.sar[hlf$prop.sar      == 0] <- 1

# Save to file
write_csv(hlf, file = here("Output/hlf_ts_proportions.csv"))

save(hlf, file = here("Output/hlf_ts_proportions.Rdata"))

# Save figures to compare results
# Get acoustic proportions for mapping
acoustic.prop.indiv.h <- hlf %>%
  ungroup() %>% 
  filter(!is.na(CPS.wg)) %>% 
  select(haul, lat, long, prop.anch, prop.jack, prop.her,
         prop.mack, prop.sar) %>% 
  replace(. == 0, 0.0000001) %>% 
  project_df(to = crs.proj)

acoustic.prop.wg.h <- hlf %>%
  ungroup() %>% 
  filter(!is.na(CPS.wg)) %>% 
  select(haul, lat, long, prop.anch.wg, prop.jack.wg, prop.her.wg,
         prop.mack.wg, prop.sar.wg) %>% 
  replace(. == 0, 0.0000001) %>% 
  project_df(to = crs.proj)

if (save.figs) {
  # Create trawl figure
  acoustic.prop.cluster.indiv.h <- base.map +
    # Plot transects data
    geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # plot ship track data
    geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = acoustic.prop.indiv.h, 
                    aes(X, Y, group = haul, r = pie.radius),
                    cols = c("prop.anch","prop.her","prop.jack",
                             "prop.mack","prop.sar"),
                    color = 'black', alpha = 0.8) +
    # Plot empty trawl locations
    geom_point(data = haul.zero, aes(X, Y),
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # Configure trawl scale
    scale_fill_manual(name = 'Species',
                      labels = c("Anchovy", "P. herring", "J. mackerel",
                                 "P. mackerel", "Sardine"),
                      values = c(anchovy.color, pac.herring.color, jack.mack.color,  
                                 pac.mack.color, sardine.color)) +
    # Plot panel label
    ggtitle("Acoustic Proportions (Number) by Haul") +
    coord_sf(crs = crs.proj, 
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
  
  # Create trawl figure
  acoustic.prop.cluster.wg.h <- base.map +
    # Plot transects data
    geom_sf(data = transects.sf, size = 0.5, colour = "gray70", 
            alpha = 0.75, linetype = "dashed") +
    # plot ship track data
    geom_sf(data = nav.paths.sf, colour = "gray50", size = 0.5, alpha = 0.5) +
    # Plot trawl pies
    geom_scatterpie(data = acoustic.prop.wg.h, 
                    aes(X, Y, group = haul, r = pie.radius),
                    cols = c("prop.anch.wg","prop.her.wg","prop.jack.wg",
                             "prop.mack.wg","prop.sar.wg"),
                    color = 'black', alpha = 0.8) +
    # Plot empty trawl locations
    geom_point(data = haul.zero, aes(X, Y),
               size = 3, shape = 21, fill = 'black', colour = 'white') +
    # Configure trawl scale
    scale_fill_manual(name = 'Species',
                      labels = c("Anchovy", "P. herring", "J. mackerel",
                                 "P. mackerel", "Sardine"),
                      values = c(anchovy.color, pac.herring.color, jack.mack.color,  
                                 pac.mack.color, sardine.color)) +
    # Plot panel label
    ggtitle("Acoustic Proportions (Weight) by Haul") +
    coord_sf(crs = crs.proj, 
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])), 
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"])))
  
  # Combine acoustic proportion maps
  acoustic.prop.comparison.h <- plot_grid(acoustic.prop.cluster.indiv.h, 
                                          acoustic.prop.cluster.wg.h,
                                        nrow = 1, labels = c("a)", "b)"))
  
  # Save trawl species proportions plots
  ggsave(acoustic.prop.comparison.h,
         filename = here("Figs/fig_acoustic_proportion_comparison_haul.png"),
         width = map.width*2, height = map.height)
  
  save(acoustic.prop.cluster.indiv.h, acoustic.prop.cluster.wg.h,
       file = here("Output/acoustic_proportion_comp_map_haul.Rdata"))
}
