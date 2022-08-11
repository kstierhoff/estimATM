# Plot lengths for specimens with collected otoliths
# Load libraries -----------------------------------------------------
library(tidyverse)
library(odbc)

# Set the theme for all plots
theme_set(theme_bw())

# User defined variables ----------------------------------------------
trawl.source <- "SQL" # Change to "SQL" if you want to connect to the lab's SQL server
# Update to the Access db location on your PC
trawl.db.path   <- here::here("Data/Trawl") 
trawl.db.access <- "TrawlDataEntry2207RL.accdb"

# Import data from the database ----------------------------------------
if (trawl.source == "SQL") {
  # Configure ODBC connection to TRAWL database on SQL server
  trawl.con  <- dbConnect(odbc(), 
                          Driver = "SQL Server", 
                          Server = "161.55.235.187", 
                          Database = "Trawl", 
                          Trusted_Connection = "True")
} else if (trawl.source == "Access") {
  # Configure ODBC connection to TRAWL database
  trawl.con  <- dbConnect(odbc(), 
                          Driver = "Microsoft Access Driver (*.mdb, *.accdb)", 
                          DBQ = file.path(trawl.db.path, trawl.db.access))
}

# Import trawl database tables
catch.all	     <- tbl(trawl.con,"Catch") %>% collect()
haul.all       <- tbl(trawl.con,"Haul") %>% collect()
lengths.all    <- tbl(trawl.con,"Specimen") %>% collect()
lengthFreq.all <- tbl(trawl.con,"LengthFrequency") %>% collect()
spp.codes      <- tbl(trawl.con,"SpeciesCodes") %>% collect()

# Close database channel
dbDisconnect(trawl.con)

# Filter for CPS species only ------------------------------------------------
# You'll need to update "lengths" to whatever you read your "specimen" table as

lengths <- lengths.all %>% 
  left_join(select(spp.codes, species, scientificName)) %>%
  filter(scientificName %in% c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                               "Scomber japonicus","Trachurus symmetricus")) 

# Compute mean length by each unique haul
length.summ <- lengths %>% 
  group_by(cruise, ship, haul, species) %>% 
  summarise(mean_FL = mean(forkLength_mm, na.rm = TRUE),
            mean_SL = mean(standardLength_mm, na.rm = TRUE),
            mean_TL = mean(totalLength_mm, na.rm = TRUE))
