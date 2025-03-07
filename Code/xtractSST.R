# Code to extract SST data along a vessel track
pacman::p_load(tidyverse, rerddapXtracto, here)

# Load backscatter data from the NOAA vessel
load(here("Data/Backscatter/nasc_all.Rdata"))

nav <- select(nasc, datetime, long, lat) %>% 
  mutate(date  = lubridate::date(datetime),
         hour  = lubridate::hour(datetime),
         min  = lubridate::minute(datetime)) %>% 
  filter(min %in% c(0, 30)) %>%
  group_by(date, hour, min) %>%
  slice(1) %>% 
  ungroup() %>% 
  arrange(datetime)

xpos <- nav$long+360
ypos <- nav$lat
tpos <- nav$datetime

sstInfo <- rerddap::info('jplMURSST41') # noaacwBLENDEDsstDaily jplMURSST41
sstData <- rxtracto(sstInfo, parameter = 'analysed_sst', 
                       xcoord = xpos, ycoord = ypos, tcoord = tpos, 
                       xlen = .2, ylen = .2, progress_bar = TRUE)

# Plot FSV data
myPlot <- plotTrack(sstData, xpos, ypos, tpos, plotColor = 'thermal')
myPlot

# Save FSV plot
ggsave(myPlot, filename = here("Figs/fig_sst_rxtracto.png"))

# Load backscatter data from the nearshore vessel
load(here("Data/Backscatter/nasc_nearshore.Rdata"))

nav.ns <- select(nasc.nearshore, datetime, long, lat) %>% 
  mutate(date  = lubridate::date(datetime),
         hour  = lubridate::hour(datetime),
         min  = lubridate::minute(datetime)) %>% 
  filter(minute %in% c(0, 30)) %>%
  group_by(date, hour, minute) %>%
  slice(1) %>% 
  ungroup() %>% 
  arrange(datetime)

xpos <- nav.ns$long+360
ypos <- nav.ns$lat
tpos <- nav.ns$datetime

sstData.ns <- rxtracto(sstInfo, parameter = 'analysed_sst', 
                   xcoord = xpos, ycoord = ypos, tcoord = tpos, 
                   xlen = .2, ylen = .2, progress_bar = TRUE)

# Plot nearshore data
myPlot.ns <- plotTrack(sstData.ns, xpos, ypos, tpos, plotColor = 'thermal')
myPlot.ns

# Save nearshore plot
ggsave(myPlot.ns, filename = here("Figs/fig_sst_rxtracto_nearshore.png"))
