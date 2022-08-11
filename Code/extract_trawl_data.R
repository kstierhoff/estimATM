# Extract and plot length data from Trawl database for TS analysis
# K. Stierhoff (2022-08-11)

# Load libraries -----------------------------------------------------
library(tidyverse)
library(odbc)
library(atm) 
library(lubridate)

# May need to install {atm} from github
# install_github("kstierhoff/atm")

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

# Assign trawls to season
haul.all <- haul.all %>% 
  mutate(season = case_when(
    month(equilibriumTime) < 6 ~ "spring",
    TRUE ~ "summer"))

# Filter for CPS species only ------------------------------------------------
lengths <- lengths.all %>% 
  left_join(select(spp.codes, species, scientificName)) %>%
  left_join(select(haul.all, cruise, ship, haul, season)) %>%
  filter(scientificName %in% c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                               "Scomber japonicus","Trachurus symmetricus")) %>% 
  # Convert native length to total length, which us used to estimate weight
  mutate(
    totalLength_mm = case_when(
      scientificName == "Clupea pallasii" ~ 
        convert_length("Clupea pallasii", .$forkLength_mm, "FL", "TL"),
      scientificName == "Engraulis mordax" ~ 
        convert_length("Engraulis mordax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Sardinops sagax" ~ 
        convert_length("Sardinops sagax", .$standardLength_mm, "SL", "TL"),
      scientificName == "Scomber japonicus" ~ 
        convert_length("Scomber japonicus", .$forkLength_mm, "FL", "TL"),
      scientificName == "Trachurus symmetricus" ~ 
        convert_length("Trachurus symmetricus", .$forkLength_mm, "FL", "TL")))

# Compute mean length by each unique haul
length.summ <- lengths %>% 
  group_by(cruise, ship, haul, species) %>% 
  summarise(mean_FL = mean(forkLength_mm, na.rm = TRUE),
            mean_SL = mean(standardLength_mm, na.rm = TRUE),
            mean_TL = mean(totalLength_mm, na.rm = TRUE))

# Get TL/W regression data
# Get max TL for plotting L/W models
L.max <- lengths %>% 
  group_by(scientificName) %>% 
  summarise(max.TL = max(totalLength_mm, na.rm = TRUE))

lw.df <- data.frame()

# Generate length/weight curves
for (i in unique(L.max$scientificName)) {
  # Create a length vector for each species
  L <- seq(0, L.max$max.TL[L.max$scientificName == i])
  
  # Calculate weights from lengths
  W <- estimate_weight(i, L, season = "summer")
  
  # Combine results
  lw.df <- bind_rows(lw.df, data.frame(scientificName = i, L, W))
}

# Plot L/W data from all surveys -----------------------------------
lw.plot <- ggplot() + 
  # Plot L/W data from all years/surveys
  geom_point(data = lengths, 
             aes(totalLength_mm, weightg),
             colour = "gray70", alpha = 0.3) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df, 
            aes(L, W), linetype = 'dashed') +
  # Plot L/W data for current survey
  geom_point(data = lengths, 
             aes(totalLength_mm, weightg, group = season, fill = season), 
             shape = 21, alpha = 0.9) + 
  scale_fill_manual(name = "Season", values = c("pink","blue")) +
  # scale_fill_manual(name = "Sex", values = c("pink","lightblue","green")) +
  # Facet by species
  facet_grid(scientificName~season, scales = "free") +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_text(face = "italic"))
