# Plot lengths for specimens with collected otoliths
# Load libraries -----------------------------------------------------
library(tidyverse)
library(cowplot)
library(odbc)

# Set the theme for all plots
theme_set(theme_bw())

# User defined variables ----------------------------------------------
trawl.source <- "Access" # Chante to "SQL" if you want to connect to the lab's SQL server
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

otoliths <- lengths.all %>% 
  left_join(select(spp.codes, species, scientificName)) %>%
  filter(scientificName %in% c("Engraulis mordax","Sardinops sagax",
                               "Scomber japonicus","Trachurus symmetricus")) 

# Plot species with standard length measurements
otoliths.sl <- ggplot() + 
  geom_histogram(data = filter(otoliths,
                               scientificName %in% c("Engraulis mordax","Sardinops sagax")),
                 aes(standardLength_mm),
                 colour = "black", fill = "gray50",
                 binwidth = 10, breaks = seq(0, 200, 10)) +
  geom_histogram(data = filter(otoliths, 
                               scientificName %in% c("Engraulis mordax","Sardinops sagax"),
                               !is.na(otolithNumber)),
                 aes(standardLength_mm, fill = scientificName),
                 colour = "black",
                 binwidth = 10, breaks = seq(0, 200, 10),
                 show.legend = FALSE) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  facet_wrap(~scientificName, scales = "free") + 
  scale_fill_manual(values = c("Sardinops sagax" = "#FF0000", 
                               "Engraulis mordax" = "#00CD66")) +
  labs(x = "Standard length (mm)", y = "Count") +
  theme(strip.background.x = element_blank(),
        strip.text.x       = element_text(face = "bold.italic")) 

# Plot species with fork length measurements
otoliths.fl <- ggplot() + 
  geom_histogram(data = filter(otoliths,
                               scientificName %in% c("Scomber japonicus","Trachurus symmetricus")),
                 aes(forkLength_mm),
                 colour = "black", fill = "gray50",
                 binwidth = 50, breaks = seq(0, 600, 50)) +
  geom_histogram(data = filter(otoliths, 
                               scientificName %in% c("Scomber japonicus","Trachurus symmetricus"),
                               !is.na(otolithNumber)),
                 aes(forkLength_mm, fill = scientificName),
                 colour = "black",
                 binwidth = 50, breaks = seq(0, 600, 50),
                 show.legend = FALSE) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  facet_wrap(~scientificName, scales = "free") + 
  scale_fill_manual(values = c("Scomber japonicus" = "#00FFFF", 
                               "Trachurus symmetricus" = "#0000FF")) +
  labs(x = "Fork length (mm)", y = "Count") +
  theme(strip.background.x = element_blank(),
        strip.text.x       = element_text(face = "bold.italic")) 

# Combine plots
otoliths.all <- plot_grid(otoliths.sl,
                          otoliths.fl,
                          nrow = 2)

# Save plot
ggsave(otoliths.all,
       filename = "fig_otolith_sample_distribution.png", 
       height = 9, width = 11)
