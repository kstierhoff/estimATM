# Format data for preliminary eDNA/ATM survey comparisons

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")
if (!require("pak")) install.packages("pak")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,lubridate,here)

# Newer versions of ggrepel does not work with rotated axes; use this version until CRAN version is fixed
# install_version("ggrepel", version = "0.9.4", repos = "http://cran.us.r-project.org")

# Install and load required packages from Github -------------------------------
if (!require("atm")) pkg_install("SWFSC/atm")
if (!require("surveyR")) pkg_install("SWFSC/surveyR")
pacman::p_load_gh("SWFSC/atm")
pacman::p_load_gh("SWFSC/surveyR")

# Load cluster files
clf.ts.prop <- read_csv(here("Output/clf_ts_proportions.csv"))
cluster.pie <- read_csv(here("Output/cluster_pie.csv"))
haul.info   <- read_csv(here("Output/haul_info.csv"))

# Summarize hauls by cluster
haul.summ <- haul.info %>% 
  group_by(cluster) %>% 
  summarise(
    time.mean = mean(equilibriumTime),
    time.min = min(equilibriumTime),
    time.max = max(equilibriumTime)
  )

# Add times to cluster files
clf.ts.prop <- clf.ts.prop %>% 
  left_join(haul.summ) %>% 
  arrange(cluster) %>% 
  select(cluster, time.mean:time.max, everything())

cluster.pie <- cluster.pie %>% 
  left_join(haul.summ) %>% 
  arrange(cluster) %>% 
  select(cluster, time.mean:time.max, everything())

# Write to file
write_csv(clf.ts.prop, file = here("Output/clf_ts_prop_time.csv"))
write_csv(cluster.pie, file = here("Output/cluster_pie_time.csv"))
