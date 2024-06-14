# Load necessary library
library(dplyr)

# Step 1: Create the data frame
data <- read.csv("C:/ABB/CODE/Github/estimATM/2407RL/Data/Nav/waypoints_2407LM_nearshore7.csv")


# Step 2: Define the directory where you want to save the CSV files
output_directory <- "C:/ABB/CODE/Github/estimATM/2407RL/Output/routes/Nearshore_LM_7nm"

# Step 3: Define a function to filter and save the data
save_filtered_data <- function(data, prefix, output_directory) {
  # Filter the data based on the prefix
  filtered_data <- data %>%
    filter(grepl(paste0("^", prefix), Station))
  
  # Create the file path
  file_path <- file.path(output_directory, paste0(prefix, "N.csv"))
  
  # Write the filtered data to a CSV file
  write.csv(filtered_data, file_path, row.names = FALSE)
}

# Step 4: Extract unique prefixes and save data
# Use substr to extract the first three characters from the Station column
unique_prefixes <- unique(substr(data$Station, 1, 3))
for (prefix in unique_prefixes) {
  save_filtered_data(data, prefix, output_directory)
}
