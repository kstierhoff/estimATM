# This script is used to generate a high-resolution look-up table for Juan's new
# habitat model, which is used by Matlab to estimate the potential habitat of
# the northern stock of Pacific Sardine using satellite-sensed measures of SST
# and Chlorophyll.

# Load required libraries
library(mgcv)   # Needed for the predict() function
library(here)   # Needed for saving to relative path

# Load habitat GAM model
mdl <- readRDS(here("Sandbox/habitat.full.update.2022.rds"))
                                        
# Cycle through all iterations of SST and CHL and generate table of predictions
SST <- seq(min(mdl$model$temp), max(mdl$model$temp), length.out = 1000)
log.chl <- seq(min(mdl$model$log.chl), max(mdl$model$log.chl), length.out = 1000)

# Create a dataframe containing all possible combinations of SST and CHL
df <- expand.grid(SST, log.chl)
names(df) <- c("temp", "log.chl")

# Add a duration of 30 minutes
df$duration <- 30

# Predict sardine presence using GAM model
df$sardine.presence <- predict(mdl, newdata  = df, type = "response")

# Write results to a CSV file
write.csv(df, file = here("Data/Habitat/sardineHabitatModel_20230223.csv"), row.names = FALSE)
