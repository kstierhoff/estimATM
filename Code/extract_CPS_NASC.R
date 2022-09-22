# A utility for extracting CPS backscatter (cps.nasc) from Echoview CSV files
# Source the script and respond to the prompts
# If a path to Exported Images is provided, corresponding echograms will 

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, here, fs)

# Install and load required packages from Github -------------------------------
pacman::p_load_gh("kstierhoff/atm")

# Set theme for plotting
theme_set(theme_bw())

# Close any open graphics windows
graphics.off()

# Load function used to estimate CPS NASC --------------------------------------
source(here::here("Code/estimate_CPS_NASC.R"))

# Configure input and output paths ---------------------------------------------
## CSV input path (source)
path.in  <- here("Data/Backscatter/RL")

## CSV output path (destination) and new file suffix
path.out <- here("Data/Backscatter/RL")
suffix.out <- "_nasc_cps"

## Path to echogram images exported from Echoview
path.img <- "C:/SURVEY/2207RL/PROCESSED/EV/Exported_Images"

# Run the function to estimate CPS backscatter --------------------------------
estimate.cps.nasc(path.input = path.in, 
                  pattern = "CPS-Final 38 kHz CPS.csv", 
                  path.output = path.out, 
                  suffix = suffix.out,
                  path.img = path.img,
                  pattern.img = "CPS-38 kHz CPS for Image Export.png",
                  expand.right = TRUE,
                  expand.left = TRUE,
                  expansion = 2, max.range = 350, root = 2,  scaling = 0.1,
                  jpeg = FALSE)

# Create final figure showing results of processing ---------------------------
# List new nasc_cps.csv files
masked.file.list <- list.files(path = path.out, pattern = suffix.out, recursive = FALSE)
print(masked.file.list)

# Ask for the nasc_cps.csv file to plot
print("Type the order of the file you want to plot:")
masked.file.num <- scan(n = 1)

# Create file info variables
new.masked.filename <- masked.file.list[masked.file.num]
new.masked.path <- file.path(path.out, new.masked.filename)
new.masked.plot <- paste0(path_ext_remove(path_file(new.masked.path)), ".png")

# Read new nasc_cps.csv file
new.masked.file <- read_csv(new.masked.path) %>% 
  arrange(NASC)

## Summarize file for plotting the seabed depth
seabed.depth <- new.masked.file %>% 
  group_by(Dist_M) %>% 
  summarize(max.depth = -max(Depth_mean))

## Plot results
cps.nasc.bubble <- ggplot(new.masked.file) +
  # Plot the seabed
  geom_line(data = seabed.depth,
            aes(Dist_M, max.depth), alpha = 0.5) +
  # Plot the surface
  geom_hline(yintercept = 1) +
  # Plot the top habitat line
  geom_line(aes(Dist_M, -top.habitat), colour = "red", linetype = "dashed") +
  # Plot the bottom habitat line
  geom_line(aes(Dist_M, -bottom.habitat), colour = "blue", linetype = "dashed") +
  # Plot NASC that was removed
  geom_point(data = filter(new.masked.file, NASC != cps.nasc),
    aes(Dist_M, -Depth_mean, size = NASC),
    shape = 21, fill = NA, alpha = 0.8, show.legend = FALSE) +
  # Plot NASC that was retained
  geom_point(data = filter(new.masked.file, NASC > 0, NASC == cps.nasc),
    aes(Dist_M, -Depth_mean, size = NASC, fill = NASC),
    shape = 21, alpha = 0.9, show.legend = TRUE) +
  # Configure axes and scales
  scale_x_continuous(position = "top", breaks = seq(0, max(new.masked.file$Dist_M), 2000), expand = c(0,0)) +
  scale_y_continuous(breaks = -rev(seq(0, signif(max(new.masked.file$Depth_mean), 1), 50))) +
  scale_fill_viridis_c() +
  # Configure labels and title
  labs(title = new.masked.filename,
       x = "Echoview distance (M)", 
       y = "Mean depth (m)") + 
  # Set themes
  theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 1.2))

# Save the plot
ggsave(cps.nasc.bubble,
       filename = file.path(path.out, new.masked.plot),
       width = 15, height = 7
)

# Open the newly created plot
shell.exec(file.path(path.out, new.masked.plot))
