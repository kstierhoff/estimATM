library(tidyverse)
library(readr)
library(lubridate)
library(here)
library(plotly)
library(sf)
library(mapview)

# Import specimen data ----------------------------------------------------
lm.specimens <- read_csv("Data/Seine/lm_catch.csv") %>% 
  mutate(date = mdy(date),
         key.set = paste(date, set),
         label = paste("Date:", date, "Set:", set, "Fish num:", fish_number),
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
  filter(!is.na(scientificName))

# Import specimen data ----------------------------------------------------
lm.sets <- read_csv("Data/Seine/lm_sets.csv") %>% 
  mutate(date = mdy(date),
         lat = lat_deg + lat_decmin/60,
         long = -(long_deg + long_decmin/60),
         key.set = paste(date, set))

lm.spec.summ <- lm.specimens %>%
  group_by(key.set, scientificName) %>% 
  summarise(totalWeight = sum(weightg),
            totalCount  = n()) %>% 
  left_join(select(lm.sets, key.set, lat, long)) %>% 
  filter(!is.na(lat)) 

lm.spec.summ.sf <- lm.spec.summ %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

set.summ.wt <- lm.spec.summ %>% 
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
  atm::project_df(to = 4326) %>% 
  mutate(
    label = paste("Set", key.set),
    popup = paste('<b>Set:', key.set, '</b><br/>',
                  'Anchovy:', Anchovy, 'kg<br/>',
                  'Sardine:', Sardine, 'kg<br/>',
                  'Jack Mackerel:', JackMack, 'kg<br/>',
                  'P. herring:', PacHerring, 'kg<br/>',
                  'P. mackerel:', PacMack, 'kg<br/>',
                  'All CPS:', AllCPS, 'kg'))

# Filter for empty trawls
set.zero    <- filter(set.pie, AllCPS == 0)

# Calculate pie radius based on latitude range
pie.radius <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*pie.scale)

scale.pies <- FALSE

# Calculate pie radius of each pie, based on All CPS landings
if (scale.pies) {
  set.pie$radius    <- pie.radius*set.pie$bin
} else {
  set.pie$radius    <- pie.radius
}

set.pos <- filter(set.pie, AllCPS > 0) %>% 
  arrange(desc(X))

ggplot() + 
  # Plot trawl pies
  scatterpie::geom_scatterpie(data = set.pos, aes(X, Y, group = key.set, r = 0.125),
                  cols = c("Anchovy", "JackMack", "Jacksmelt",
                           "PacHerring", "PacMack", "Sardine"),
                  color = 'black', alpha = 0.8) +
  coord_sf(crs = 3310) +
  theme_bw()
  

# Load nearshore backscatter data -----------------------------------------
load(here("Output/cps_nasc_prop_ns.Rdata"))

nasc.ns.paths <- nasc.nearshore %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(transect.name, vessel.name) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") %>% 
  ungroup()

# Map hauls and transect paths --------------------------------------------
mapview(filter(nasc.ns.paths, vessel.name == "LM"), zcol = "vessel.name", legend = F) + 
  mapview(lm.spec.summ, zcol = "scientificName", cex = "totalWeight")

# Real length/weigth relationship models for each species
lw.models <- read.csv(here("Data/Trawl/cps_lw_relationships.csv")) %>% 
  filter(model_type == "glm", season == "summer") %>% 
  select(-season)

# Get max TL for plotting L/W models
L.max <- lm.specimens %>% 
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

# Dataframe for storing results
lw.df <- data.frame()

# Generate length/weight curves
for (i in unique(lm.specimens$scientificName)) {
  # Subset model
  model.temp <- filter(lw.models, scientificName == i)
  
  # Create a length vector for each species
  L <- seq(0, L.max$max.TL[L.max$scientificName == i])
  
  # Calculate weights from lenths
  W <- model.temp$a*L^model.temp$b
  
  # Combine results
  lw.df <- bind_rows(lw.df, data.frame(model.temp, L, W))
}

# Estiamate missing weights from lengths -------------------------------------------------------
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


lm.specimens_long <- lm.specimens %>%
  pivot_longer(-scientificName, -date, -set, -weightg, names_to = "length_type", values_to = "length_mm")

lm.specimens_long <- lm.specimens %>%
  select(scientificName, weightg, totalLength_mm, forkLength_mm, standardLength_mm) %>% 
  pivot_longer(-scientificName, -weightg, 
               names_to = "length_type", values_to = "length_mm")

# lm_plot <- lm.specimens %>% 
#   select(set, scientificName, forkLength_mm, standardLength_mm, weightg) 

p <- ggplot() + 
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
  theme_bw() 

ggsave(p, filename = here("Figs/fig_LW_plots_LisaMarie.png"))

ggplotly(p)


p2 <- ggplot() + 
  geom_point(data = lm.specimens, aes(totalLength_mm, weightg, label = label), 
             colour = "blue") + 
  # geom_point(data = lm.specimens, aes(standardLength_mm, weightg, label = label), 
  #            colour = "red") + 
  facet_wrap(~scientificName, scales = "free") +
  theme_bw() 

ggplotly(p2)
