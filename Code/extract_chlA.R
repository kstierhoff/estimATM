# Load libraries
library(tidyverse)
library(rerddapXtracto)
library(rerddap)
library(here)
library(xts)

# Load ship track
load(here("Data/Nav/nav_data.Rdata"))

# Reduce nav data to 30 minute resolution
nav <- nav %>% 
  mutate(group = align.time(time, n = 60*30)) %>% 
  group_by(group) %>% 
  slice(1)

# Extract data
xpos <- nav$long
ypos <- nav$lat
tpos <- as.character(lubridate::date(nav$time))
zpos <- rep(0., length(xpos))
chlInfo <- rerddap::info('erdMWchla14day_LonPM180')
chl1 <- rxtracto(chlInfo, parameter = 'chlorophyll', 
                   xcoord = xpos, 
                   ycoord = ypos, 
                   tcoord = tpos, 
                   zcoord = zpos, 
                   xlen = .2, ylen = .2)

plotTrack(chl1, xpos, ypos, tpos, plotColor = 'algae')

chl1.df <- data.frame(chl1, xpos, ypos, tpos) %>% 
  select(lat = ypos, long = xpos, date = tpos, chlorophyll = mean.chlorophyll)

write_csv(chl1.df, here("Output/nav_chlorphyll_2207RL.csv"))
