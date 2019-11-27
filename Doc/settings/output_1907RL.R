# Load output from estimateBiomass ----------------------------------------
# Load biomass estimate tables
load(here("Output/biomass_bootstrap_estimates_final.Rdata"))
load(here("Output/biomass_bootstrap_estimates_final_os.Rdata"))
load(here("Output/biomass_bootstrap_estimates_final_ns.Rdata"))
load(here("Output/biomass_bootstrap_estimates_final_nse.Rdata"))

# Get survey estiamtes for all strata
be.all     <- filter(be,     Stratum == "All")
be.all.os  <- filter(be.os,  Stratum == "All")
be.all.ns  <- filter(be.ns,  Stratum == "All")
be.all.nse <- filter(be.nse, Stratum == "All")

# Load abundance and length summaries
# load(here("Output/length_summary_all.Rdata")) # Length and weight ranges
load(here("Output/length_frequency_summary.Rdata"))
load(here("Output/abundance_table_all.Rdata"))
load(here("Output/abundance_table_all_os.Rdata"))
load(here("Output/abundance_table_all_ns.Rdata"))

# Summarise length-disaggregated abundances to describe length ranges
length.summ <- abund.summ %>% 
  filter(biomass != 0) %>% 
  group_by(scientificName = Species, stock = Stock) %>% 
  summarise(L.min = min(TL),
            L.max = max(TL))

# Combine abundance summaries for tables
abund.summ.all <- abund.summ %>% 
  mutate(Region = "Core") %>% 
  bind_rows(abund.summ.ns) %>% 
  bind_rows(abund.summ.os) %>% 
  filter(!is.nan(abundance)) %>% 
  ungroup() %>% 
  select(Species, Stock, Region, SL, abundance) %>%
  pivot_wider(names_from = Region, 
              values_from = abundance, 
              values_fn = list(abundance = sum)) %>% 
  select(Species, Stock, SL, estimate.regions) 

# Load trawl information
load(here("Output/haul_info.Rdata"))
load(here("Output/catch_info.Rdata"))
load(here("Output/species_codes.Rdata"))

# Load NASC summaries
load(here("Output/nasc_summ_tx.Rdata"))

if (file.exists(here("Output/nasc_summ_tx_os.Rdata"))) {
  load(here("Output/nasc_summ_tx_os.Rdata"))
  nasc.summ.sd.os <- filter(nasc.summ.os, str_detect(vessel.name, "SD"))
  nasc.summ.rl.os <- filter(nasc.summ.os, str_detect(vessel.name, "RL"))  
}

if (file.exists(here("Output/nasc_summ_tx_ns.Rdata"))) {
  load(here("Output/nasc_summ_tx_ns.Rdata"))
}

# Load transect spacing
load(here("Output/transect_spacing.Rdata"))

# Load calibration results
load(here("Output/cal_output_table.Rdata"))
