# Get start/end dates and lat/long for each sampling stratum and species

library(tidyverse)
library(fs)

# List directories
output.dirs <- c(
  "E:/CODE/R_packages/estimATM/1507SH",
  "E:/CODE/R_packages/estimATM/1606RL",
  "E:/CODE/R_packages/EstimateCPS/1707RL",
  "E:/CODE/R_packages/EstimateCPS/1807RL",
  "E:/CODE/R_packages/estimATM/1907RL",
  "E:/CODE/R_packages/estimATM/2103RL")

be.stratum.all <- data.frame()

# Extract estimates from all survey dirs
for (i in output.dirs) {
  # i = output.dirs[1]
  
  # Load biomass estimates
  load(file.path(i, "Output/biomass_bootstrap_estimates_stratum.Rdata")) 
  
  # Extract survey name
  be.stratum <- be.stratum %>% 
    mutate(Survey = tail(unlist(str_split(i, "/")), 1),
           key = paste(Species, Stratum))
  
  # Load NASC data (to get dates)
  load(file.path(i, "Output/nasc_final.Rdata"))
  
  nasc.summ <- nasc %>% 
    group_by(transect.name) %>% 
    summarise(
      lat.min    = min(lat),
      lat.max    = max(lat),
      date.start = lubridate::date(min(datetime)),
      date.end   = lubridate::date(max(datetime))
    )
  
  # Get stratum info
  strata.info <- read_csv(file.path(i, "Output/strata_final.csv")) %>% 
    left_join(select(nasc.summ, transect.name, date.start, date.end, lat.min, lat.max))
  
  strata.info.summ <- strata.info %>% 
    group_by(Species = scientificName, Stratum = stratum) %>% 
    summarise(
      lat_min = min(start.lat),
      lat_max = max(start.lat),
      date_min = min(date.start),
      date_max = max(date.start)
    ) %>% 
    mutate(key = paste(Species, Stratum)) %>% 
    ungroup()
  
  # Add strata info to biomass estimates
  be.stratum <- be.stratum %>% 
    left_join(select(strata.info.summ, lat_min:key))
  
  # Combine estimates
  be.stratum.all <- bind_rows(be.stratum.all, be.stratum)
}

# Rearrange columns
be.stratum.all <- select(be.stratum.all, Survey, everything(), -key) %>% 
  arrange(Species, Stock, Survey, Stratum)

write_csv(be.stratum.all, here::here("biomass_strata_all.csv"))

# # Summarise for anchovy only
# be.stratum.all %>% 
#   filter(Species == "Engraulis mordax", Stock == "Central") %>% 
#   group_by(Survey, Stock) %>% 
#   summarise(
#     n_strata = n(),
#     biomass.total = sum(biomass.mean.boot))

be.stratum.csna <- be.stratum.all %>% 
  filter(Species == "Engraulis mordax", Stock == "Central")

write_csv(be.stratum.csna, here::here("biomass_strata_csna.csv"))




