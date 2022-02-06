library(tidyverse)

load("C:/KLS/CODE/R_packages/estimATM/2107RL/Output/haul_info.Rdata")
load("C:/KLS/CODE/R_packages/estimATM/2107RL/Output/lengths_final.Rdata")

stock.break.anch <- 40.46 # Latitude of Cape Mendocino
stock.break.sar  <- 34.46 # Latitude of Pt. Conception (or change based on SST)

# Add spatial info to lengths and define stocks
lengths <- lengths %>% 
  left_join(select(haul, haul, lat = startLatDecimal, long = startLongDecimal)) %>% 
  mutate(stock = case_when(
    scientificName == "Engraulis mordax" & lat >= stock.break.anch ~ "Northern",
    scientificName == "Engraulis mordax" & lat <  stock.break.anch ~ "Central",
    scientificName == "Sardinops sagax"  & lat >= stock.break.sar  ~ "Northern",
    scientificName == "Sardinops sagax"  & lat <  stock.break.sar  ~ "Southern",
    scientificName %in% c("Clupea pallasii","Scomber japonicus","Trachurus symmetricus") ~ "All")) %>% 
  filter(!is.na(stock), scientificName %in% c("Sardinops sagax","Engraulis mordax")) %>% 
  mutate(group = paste(scientificName, stock, sep = "-"))

ggplot(lengths, aes(standardLength_mm)) +
  geom_histogram() + 
  facet_wrap(~group, nrow = 1, scales = "free") +
  theme_bw()
  
lengths.sub <- lengths %>% 
  select(cruise, ship, haul, lat, long, scientificName, stock, standardLength_mm, weightg)

write_csv(lengths.sub, file = here::here("Output/lenghts_for_S-K_report_2107RL.csv"))
