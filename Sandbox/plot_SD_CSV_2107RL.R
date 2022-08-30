library(tidyverse)
library(here)
library(atm)
library(fs)
library(mapproj)

# List all CSV files
csv.files.cps <- dir_ls(here("Data/Backscatter/SD"), 
                        regexp = "_CPS-Final 38 kHz CPS.csv",
                        ignore.case = TRUE)

# Create an empty data frame
nasc.cps <- data.frame()

if (length(csv.files.cps) > 0) {
  # Configure progress bar
  pb <- winProgressBar(title = "CSV File Processing Progress - CPS", 
                       label = "0% done", min = 0, max = 100, initial = 0)
  
  # Process all .CSV files
  for (i in 1:length(csv.files.cps)) {
    # Process i-th file
    nasc.cps <- bind_rows(nasc.cps, extract_csv(csv.files.cps[i]))
    
    # Update the progress bar
    info <- sprintf("%d%% done", round((i / length(csv.files.cps)) * 100))
    setWinProgressBar(pb, round((i / length(csv.files.cps)) * 100), label = info)
  }
  close(pb)
}

# Get intervals with bad lat/long values
bad.nasc.cps <- filter(nasc.cps, lat == 999, long == 999)

# Remove bad positions
nasc.cps <- nasc.cps %>%
  filter(lat != 999, long != 999) %>% 
  mutate(vessel.orig = as.factor(str_extract(filename, "SD\\d{4}"))) 


# Plot results
ggplot(nasc.cps, aes(long, lat, colour = vessel.orig)) + 
  geom_point() + 
  coord_map() +
  theme_bw()



