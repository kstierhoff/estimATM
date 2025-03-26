# Examine relationship between nighttime backscatter and trawl catches

library(tidyverse)
library(here)
library(fs)
library(sf)
library(atm)
library(mapview)
library(surveyR)
library(plotly)
library(scatterpie)

# Load user settings ------------------------------------------------
process.csv <- FALSE
get.db <- FALSE

# Get project name from directory
prj.name <- last(unlist(str_split(here(), "/")))

# Get all settings files
settings.files <- dir(here("Doc/settings"))

# Source survey settings file
prj.settings <- settings.files[str_detect(settings.files, paste0("settings_", prj.name, ".R"))]

source(here("Doc/settings", prj.settings))

# Load trawl data -------------------------------------------
if (get.db) {
  # Source script to collect data from trawl database
  source(here("Code/collect_trawl_database.R"))
  
} else {
  # Load trawl data
  load(here("Data/Trawl/trawl_data_raw.Rdata"))
}

# Source script to format data from trawl database
source(here("Code/format_trawl_database.R"))

# Extract data from current survey
haul <- haul.all %>% 
  filter(cruise %in% cruise.name & ship %in% cruise.ship) %>% 
  # Remove bad trawls
  filter(!trawlPerformance %in% trawl.performance) %>% 
  arrange(haul) %>% 
  mutate(duration = difftime(haulBackTime, equilibriumTime, units = "mins"), # Calculate duration
         cluster  = cumsum(c(0, diff(equilibriumTime)) > 12) + 1,
         sample.type = "Trawl")  

# Find midpoint of each haul as the mean lat/long
haul.mid <- haul %>% 
  group_by(cluster, haul, sample.type) %>% 
  summarise(
    lat  = mean(c(startLatDecimal, stopLatDecimal)),
    long = mean(c(startLongDecimal, stopLongDecimal))) 

# Find midpoint of each haul cluster as the average of haul midpoints
cluster.mid <- haul.mid %>% 
  group_by(cluster, sample.type) %>% 
  summarise(
    lat  = mean(lat),
    long = mean(long))

# Filter catch data
catch <- catch.all %>% 
  left_join(select(spp.codes, species, scientificName, commonName)) %>% 
  filter(cruise %in% cruise.name & ship %in% cruise.ship & 
           scientificName %in% cps.spp) %>% 
  left_join(select(haul, haul, cluster)) %>% 
  mutate(key = paste(haul, scientificName),
         totalWeight = subSampleWtkg + remainingSubSampleWtkg,
         totalNum = (subSampleCount/subSampleWtkg)*totalWeight)

if (nrow(catch) > 0) {
  # Summarize trawl catch by species
  haul.summ.wt <- catch %>% 
    select(haul, cluster, scientificName, totalWeight) %>%
    pivot_wider(names_from = scientificName, values_from = totalWeight) 
  
  # Add species with zero total weight
  if (!has_name(haul.summ.wt, "Engraulis mordax"))      {haul.summ.wt$`Engraulis mordax`      <- 0}
  if (!has_name(haul.summ.wt, "Sardinops sagax"))       {haul.summ.wt$`Sardinops sagax`       <- 0}
  if (!has_name(haul.summ.wt, "Scomber japonicus"))     {haul.summ.wt$`Scomber japonicus`     <- 0}
  if (!has_name(haul.summ.wt, "Trachurus symmetricus")) {haul.summ.wt$`Trachurus symmetricus` <- 0}
  if (!has_name(haul.summ.wt, "Clupea pallasii"))       {haul.summ.wt$`Clupea pallasii`       <- 0}
  if (!has_name(haul.summ.wt, "Atherinopsis californiensis")) {haul.summ.wt$`Atherinopsis californiensis` <- 0}
  if (!has_name(haul.summ.wt, "Etrumeus acuminatus"))   {haul.summ.wt$`Etrumeus acuminatus` <- 0}
  if (!has_name(haul.summ.wt, "Other"))                 {haul.summ.wt$`Other` <- 0}
  
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
           "JackMack"   = "Trachurus symmetricus",
           "RndHerring" = "Etrumeus acuminatus",
           "Other"      = "Other") 
  
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
}

# Select and rename trawl data for pie charts
haul.pie <- haul.summ.wt %>% 
  select(haul, long, lat, Anchovy, JackMack, 
         Jacksmelt, Other, PacHerring, PacMack, 
         RndHerring, Sardine, AllCPS, sample.type) %>%
  # Create bins for defining point size in NASC plots
  mutate(bin       = cut(AllCPS, trawl.breaks, include.lowest = TRUE),
         bin.level = as.numeric(bin)) %>% 
  project_df(to = crs.proj)

cluster.pie <- cluster.summ.wt %>% 
  select(cluster, long, lat, Anchovy, JackMack, 
         Jacksmelt, Other, PacHerring, PacMack, 
         RndHerring, Sardine, AllCPS, sample.type) %>% 
  # Create bins for defining point size in NASC plots
  mutate(bin       = cut(AllCPS, trawl.breaks, include.lowest = TRUE),
         bin.level =  as.numeric(bin)) %>% 
  project_df(to = crs.proj)

# Filter for empty trawls
haul.zero    <- filter(haul.pie, AllCPS == 0)

cluster.zero <- filter(cluster.pie, AllCPS == 0)

# Process backscatter data --------------------------------
if (process.csv) {
  csv.files <- dir_ls("//swc-storage4-s/AST4/SURVEYS/20240625_Lasker_SummerCPS/PROCESSED/EV/CSV/NightTime Trawl Comparison", regexp = "Final 38 kHz CPS.csv")
  
  # Create an empty data frame
  nasc.cps <- data.frame()
  
  if (length(csv.files) > 0) {
    # Configure progress bar
    pb <- winProgressBar(title = "CSV File Processing Progress - CPS", 
                         label = "0% done", min = 0, max = 100, initial = 0)
    
    # Process all .CSV files
    for (i in 1:length(csv.files)) {
      # Process i-th file
      nasc.cps <- bind_rows(nasc.cps, extract_csv(csv.files[i]))
      
      # Update the progress bar
      info <- sprintf("%d%% done", round((i / length(csv.files)) * 100))
      setWinProgressBar(pb, round((i / length(csv.files)) * 100), label = info)
    }
    close(pb)
    
    # Calculate summary interval
    nasc.cps <- nasc.cps %>%
      mutate(int = cut(Interval, seq(1, max(Interval) + nasc.summ.interval, nasc.summ.interval),
                       labels = FALSE, include.lowest = TRUE)) %>% 
      arrange(datetime)
    
    # Save results
    save(nasc.cps, file = here("Output/nasc_cps_nighttime.Rdata"))
    write_csv(nasc.cps, file = here("Output/nasc_cps_nighttime.csv"))
  }
} else {
  load(file = here("Output/nasc_cps_nighttime.Rdata"))
}

# Summarize backscatter per trawl
nasc.summ.facet <- nasc.cps %>% 
  mutate(haul = as.numeric(str_extract(filename, pattern = "(\\d+)(?=-Final)"))) %>% 
  group_by(haul) %>% 
  summarise(nasc.05  = mean(NASC.5, na.rm = TRUE),
            nasc.10  = mean(NASC.10, na.rm = TRUE),
            nasc.15  = mean(NASC.15, na.rm = TRUE),
            nasc.20  = mean(NASC.20, na.rm = TRUE),
            nasc.25  = mean(NASC.25, na.rm = TRUE),
            nasc.30  = mean(NASC.30, na.rm = TRUE),) %>% 
  # mutate(haul = as.numeric(str_extract(filename, pattern = "(\\d+)(?=-Final)"))) %>% 
  pivot_longer(-haul, names_to = "nasc.depth", values_to = "mean.nasc")

nasc.summ <- nasc.cps %>% 
  group_by(filename) %>% 
  summarise(mean.nasc  = mean(NASC.30, na.rm = TRUE),
            sd.nasc    = sd(NASC.30, na.rm = TRUE),
            total.nasc = sum(NASC.30, na.rm = TRUE),
            cv.nasc = sd.nasc/mean.nasc,
            median.nasc = median(NASC.30, na.rm = TRUE)) %>% 
  mutate(haul = as.numeric(str_extract(filename, pattern = "(\\d+)(?=-Final)")))

# Use transects to set map to survey extent
map.bounds <- nasc.cps %>%
  st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
  st_transform(crs = crs.proj) %>%
  st_bbox() 

# Calculate pie radius based on latitude range
pie.radius <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*pie.scale)

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  haul.pie$r    <- pie.radius*log(haul.pie$bin.level+1)
  cluster.pie$r <- pie.radius*log(cluster.pie$bin.level+1)
} else {
  haul.pie$r    <- pie.radius
  cluster.pie$r <- pie.radius
}

# Filter for positive hauls and clusters
haul.pos <- filter(haul.pie, AllCPS > 0) %>% 
  arrange(desc(X))

cluster.pos <- filter(cluster.pie, AllCPS > 0) %>% 
  arrange(desc(X))

# Substitute very small value for species with zero catch, just for pie charts
if (nrow(haul.pos) > 0) {
  haul.pos <- haul.pos %>% 
    replace(. == 0, 0.0000001) 
  
  cluster.pos <- cluster.pos %>% 
    replace(. == 0, 0.0000001) 
}

# Convert haul data for plotting
haul.catch <- haul.summ.wt %>% 
  st_as_sf(coords = c("long", "lat"), crs = crs.geog) 

haul.catch <- project_sf(haul.catch, crs = crs.proj) 

# Summarize catch data
catch.summ <- catch %>% 
  group_by(haul) %>% 
  summarise(totalWeight = sum(totalWeight))

# Compile comp data
comp.data <- catch.summ %>% 
  left_join(select(nasc.summ, haul, mean.nasc, sd.nasc, total.nasc, cv.nasc, median.nasc)) %>% 
  filter(!is.na(mean.nasc)) %>% 
  left_join(select(haul, haul, long = startLongDecimal, lat = startLatDecimal))

comp.data.facet <- nasc.summ.facet %>% 
  left_join(catch.summ)

# Plot relationships
comp.sum.nasc <- ggplot(comp.data, aes(total.nasc, totalWeight, label = haul)) + 
  geom_point() +
  ggtitle("Total NASC vs. Total Catch Weight")

comp.mean.nasc <- ggplot(comp.data, aes(mean.nasc, totalWeight, label = haul)) + 
  geom_point() +
  ggtitle("Nean NASC vs. Total Catch Weight")

comp.cv.nasc <- ggplot(comp.data, aes(cv.nasc*100, totalWeight, label = haul)) + 
  geom_point() +
  ggtitle("CV NASC vs. Total Catch Weight")

comp.med.nasc <- ggplot(comp.data, aes(median.nasc, totalWeight, label = haul)) + 
  geom_point() +
  ggtitle("Median NASC.30 vs. Total Catch Weight")

ggplotly(comp.sum.nasc)
ggplotly(comp.mean.nasc)
ggplotly(comp.med.nasc)

# Add catch and nasc to haul.pie
haul.pie <- haul.pie %>% 
  left_join(select(comp.data, haul, totalWeight, mean.nasc, total.nasc))

haul.pos <- haul.pos %>% 
  left_join(select(comp.data, haul, totalWeight, mean.nasc, total.nasc)) %>% 
  mutate(weightNorm = totalWeight / max(totalWeight, na.rm = TRUE),
         nascNorm = total.nasc / max(total.nasc, na.rm = TRUE)) 

pie.spp <- sort(unique(catch$scientificName))

pie.scatter <- haul.pos %>% 
  select(haul, all_of(pie.cols[names(pie.cols) %in% pie.spp])) %>% 

ggplot() + 
  geom_scatterpie(data = filter(haul.pos, total.nasc > 0, totalWeight > 0), aes(total.nasc, totalWeight, group = haul),
                  cols = pie.cols[names(pie.cols) %in% pie.spp],
                  alpha = 0.8) 

ggplot() + 
  geom_scatterpie(data = filter(haul.pos, total.nasc > 0, totalWeight > 0), aes(nascNorm, weightNorm, group = haul),
                  cols = pie.cols[names(pie.cols) %in% pie.spp],
                  alpha = 0.8) +
  # geom_point(data = filter(haul.pos, AllCPS == 0)) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = unname(pie.labs[names(pie.labs) %in% pie.spp]),
                    values = unname(pie.colors[names(pie.colors) %in% pie.spp])) + 
  scale_x_continuous(breaks = seq(0,1,0.25), labels = round(seq(0,1,0.25) * max(haul.pos$total.nasc, na.rm = TRUE))) +
  scale_y_continuous(breaks = seq(0,1,0.25), labels = round(seq(0,1,0.25) * max(haul.pos$totalWeight, na.rm = TRUE))) +
  coord_equal() + 
  labs(x = "Sum of NASC",
       y = "Total Catch (kg)") 

ggsave(here::here("Figs/nasc-catch-scatter.png"),
       height = 8, width = 10)


ggplot(comp.data.facet, aes(mean.nasc, totalWeight)) + 
  geom_point() + 
  facet_wrap(~nasc.depth, nrow = 1, scales = "free_x")


ggsave(here::here("Figs/nasc-catch-scatter-facet.png"),
       height = 3, width = 10)
