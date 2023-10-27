# Load output from estimateBiomass ----------------------------------------
# Load biomass estimate tables
load(here("Output/biomass_bootstrap_estimates_final.Rdata"))
# load(here("Output/biomass_bootstrap_estimates_final_ns.Rdata"))
load(here("Output/biomass_bootstrap_estimates_final_nse.Rdata"))

# Get survey estimates for all strata
be.all     <- be %>% 
  mutate(Region = "Core") %>% 
  select(Species, Stock, Region, everything()) %>% 
  filter(Stratum == "All")
# be.all.ns  <- be.ns %>%
#   mutate(Region = "Nearshore") %>%
#   select(Species, Stock, Region, everything()) %>%
#   filter(Stratum == "All")
be.all.nse <- be.nse %>% 
  mutate(Region = "NSE") %>% 
  select(Species, Stock, Region, everything()) %>% 
  filter(Stratum == "All")

# Summarise biomass for all regions included in the final estimates
be.all.var <- be.all %>% 
  # bind_rows(be.all.ns) %>%
  select(Species, Stock, biomass.sd) %>% 
  group_by(Species, Stock) %>%
  summarise(biomass.sd = sqrt(sum(biomass.sd^2)))

# Combine core and nearshore biomass
be.all.summ <- be.all %>% 
  # bind_rows(be.all.ns) %>%
  group_by(Species, Stock) %>% 
  select(-Region, -Stratum, -biomass.sd, -biomass.cv) %>% 
  summarise_all(list(sum)) %>% 
  left_join(be.all.var) %>% 
  mutate(biomass.cv = biomass.sd/Biomass*100)

# Load abundance and length summaries
load(here("Output/length_summary_all.Rdata")) # Length and weight ranges
load(here("Output/length_frequency_summary.Rdata"))
load(here("Output/abundance_table_all.Rdata"))
# load(here("Output/abundance_table_all_os.Rdata"))
# load(here("Output/abundance_table_all_ns.Rdata"))

# Summarise length-disaggregated abundances to describe length ranges
length.summ <- abund.summ %>% 
  filter(biomass != 0) %>% 
  group_by(scientificName = Species, stock = Stock) %>% 
  summarise(L.min = min(TL),
            L.max = max(TL))

# Combine abundance summaries for tables
abund.summ.all <- abund.summ %>% 
  mutate(Region = "Core") %>% 
  # bind_rows(abund.summ.ns) %>%
  # bind_rows(abund.summ.os) %>% 
  filter(!is.nan(abundance)) %>% 
  ungroup() %>% 
  select(Species, Stock, Region, SL, abundance) %>%
  pivot_wider(names_from = Region, 
              values_from = abundance, 
              values_fn = list(abundance = sum)) %>% 
  select(Species, Stock, SL, all_of(estimate.regions)) 
  
# Load trawl information
load(here("Output/haul_info.Rdata"))
load(here("Output/catch_info.Rdata"))
load(here("Output/species_codes.Rdata"))

# Load NASC summaries
## RL
load(here("Output/nasc_summ_tx.Rdata"))

## Offshore
if (file.exists(here("Output/nasc_summ_tx_os.Rdata"))) {
  load(here("Output/nasc_summ_tx_os.Rdata"))
  nasc.summ.sd.os <- filter(nasc.summ.os, str_detect(vessel.name, "SD"))
  nasc.summ.rl.os <- filter(nasc.summ.os, str_detect(vessel.name, "RL"))  
}

## Nearshore
if (file.exists(here("Output/nasc_summ_tx_ns.Rdata"))) {
  load(here("Output/nasc_summ_tx_ns.Rdata"))
}

## Saildrone
load(here("Data/Nav/nav_data_saildrone.Rdata"))

# Load transect spacing
load(here("Output/transect_spacing.Rdata"))

# Load calibration results
# load(here("Output/cal_output_table.Rdata"))

# Load purse seine data
# load(here("Output/purse_seine_specimens.Rdata"))
# load(here("Output/purse_seine_sets.Rdata"))

