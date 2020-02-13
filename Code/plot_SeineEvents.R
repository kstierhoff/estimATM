# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,lubridate,here)

# Load log book data
logs <- read_csv(here("Data/Seine/lbc_logs.csv")) %>% 
  mutate(datetime = with_tz(mdy_hms(paste(date, as.character(time)), tz = "America/Los_Angeles"), tzone = "UTC"))
