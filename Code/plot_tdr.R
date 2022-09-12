# This is a precursor to plotTDR.Rmd in Doc
# Used to extract TDR data

library(oce)
library(here)
library(fs)
library(tidyverse)

# TDR file regex pattern
tdr.pattern <- "2207RL*.*rsk"

# List RSK files
## Kite
rsk.files.kite <- dir_ls(tdr.dir.kite, 
                    recurse = tdr.recurse, regexp = tdr.pattern)

## Footrope
rsk.files.foot <- dir_ls(here("Data/TDR/Footrope"), 
                         recurse = tdr.recurse, regexp = tdr.pattern)

# Read a kite and footrope file
rsk.kite <-read.rsk(rsk.files.kite[20])
rsk.foot <-read.rsk(rsk.files.foot[20])

# Convert to tibble
tbl.kite <- as_tibble(rsk.kite@data) %>% 
  mutate(filename = path_file(rsk.kite@metadata$filename),
         cruise = str_extract(filename, "^\\d{4}\\w{2}"),
         haul = as.numeric(str_sub(filename, nchar(cruise) + 1, nchar(cruise) + 3)),
         depth = -pressure,
         loc   = "Kite")

tbl.foot <- as_tibble(rsk.foot@data) %>% 
  mutate(filename = path_file(rsk.kite@metadata$filename),
         cruise = str_extract(filename, "^\\d{4}\\w{2}"),
         haul = as.numeric(str_sub(filename, nchar(cruise) + 1, nchar(cruise) + 3)),
         depth = -pressure,
         loc   = "Footrope")

# Combine data for plotting
tbl.all <- bind_rows(tbl.kite, tbl.foot)

# Plot results
ggplot(tbl.all) + 
  geom_line(aes(time, -pressure + 10, colour = loc)) + 
  geom_hline(yintercept = 0, colour = "black") +
  ylim(min(-tbl.all$pressure), 0) +
  labs(x = "Time (UTC)",
       y = "Pressure (dbar)",
       title = unique(tbl.all$filename)) +
  theme_bw()

