# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,lubridate,here)

# Load log book data
logs <- read_csv(here("Data/Seine/lbc_logs.csv")) %>% 
  mutate(datetime = with_tz(mdy_hms(paste(date, as.character(time)), tz = "America/Los_Angeles"), tzone = "UTC"))

# Load backscatter data
load(here("Output/nasc_nearshore_final.Rdata"))

# Filter and format
nav <- nasc.nearshore %>% 
  filter(vessel.name == "LBC") %>% 
  select(datetime, long, lat, transect)

# Plot nav
ggplot(nav, aes(long, lat, group = transect)) + 
  geom_path() + 
  coord_map()
