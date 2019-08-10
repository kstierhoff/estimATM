# Load libraries
# library(here)
# library(tidyverse)

# Load and format L/L model output
ll.data <- readRDS(here("Data/Trawl/LL_conversions.RDS")) %>%
  rename(scientificName = Species) %>% 
  gather(type, coef, -scientificName) %>% 
  arrange(scientificName, type)

# Load and format L/W model output
lw.data <- readRDS(here("Data/Trawl/LW_conversions.RDS")) %>% 
  rename(var = Coef) %>% 
  filter(!str_detect(var, "Naive"), !str_detect(var, "Bias")) %>% 
  gather(species, coef, -var) %>% 
  mutate(
    scientificName = case_when(
      species == "Sardine" ~ "Sardinops sagax",
      species == "Anchovy" ~ "Engraulis mordax",
      species == "Jackmac" ~ "Trachurus symmetricus",
      species == "Pacmac"  ~ "Scomber japonicus",
      species == "Herring" ~ "Clupea pallasii"),
    term = case_when(
      str_detect(var, "Slope")     ~ "slope",
      str_detect(var, "Intercept") ~ "intercept"),
    model_type = case_when(
      str_detect(var, "[^G]LM")  ~ "OLS",
      str_detect(var, "^LM")  ~ "OLS",
      str_detect(var, "GLM") ~ "GLM"),
    season = case_when(
      str_detect(var, "Spring")  ~ "Spring",
      str_detect(var, "Summer")  ~ "Summer",
      TRUE ~ "Both")) %>% 
  arrange(scientificName, model_type, desc(term)) %>% 
  replace_na(list(coef = 0)) %>% # Replace NA with 0 for species with no intercept
  select(scientificName, model_type, season, term, coef)

# Export to CSV
write_csv(lw.data, here("Output/ll_coefficients.csv"))
write_csv(lw.data, here("Output/lw_coefficients.csv"))

