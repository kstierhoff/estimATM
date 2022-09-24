# A utility for extracting CPS backscatter (cps.nasc) from Echoview CSV files
# Source the script and respond to the prompts
# If a path to Exported Images is provided, corresponding echograms will 

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, here, fs)

# Install and load required packages from Github -------------------------------
pacman::p_load_gh("kstierhoff/atm")

# Close any open graphics windows
graphics.off()

# Load function used to estimate CPS NASC --------------------------------------
source(here::here("Code/estimate_CPS_NASC.R"))

# Configure input and output paths ---------------------------------------------
## CSV input path (source)
path.in  <- here("Data/Backscatter/RL")

## CSV output path (destination)
path.out <- here("Data/Backscatter/RL")

## Path to echogram images exported from Echoview
path.img <- "C:/SURVEY/2207RL/PROCESSED/EV/Exported_Images"

# Run the function to estimate CPS backscatter --------------------------------
extractNASC(path.in = path.in, # CSV file source
            pattern.in = "_CPS-Final 38 kHz CPS.csv", # CSV file regex
            path.out = path.out, # Processed file destination
            suffix.out = "_nasc_cps.csv", # Suffix applied to processed CSV files
            path.img = path.img,  # Location of exported image files
            pattern.img = "_CPS-38 kHz CPS for Image Export.png", # Exported image regex
            expand.right = T,    # Expand right side of plot
            expand.left  = F,    # Expand left side of plot
            expansion = 2,       # Constant for expanding axes
            max.range = 250,     # Depth range for bubble plots
            root = 2,            # Constant for controlling bubble size (2)
            scaling = 0.1,       # Constant for controlling bubble size (0.1)
            jpeg = FALSE,        # Save intermediate plots from line picks
            x11.w = 1600,        # Width of graphics window (px)
            x11.h = 600)         # Height of graphics window (px)
