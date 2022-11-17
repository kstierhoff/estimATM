# A utility for extracting CPS backscatter (cps.nasc) from Echoview CSV files
# Source the script (Ctrl+Shift+S) and respond to the prompts

# If a path to Exported Images is provided (path.img), corresponding
# echograms will be displayed to assist echo scrutiny

# To specify file paths by copy/paste from Windows Explorer, put path in r"(PATH)"

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

# Extract only CPS backscatter -------------------------------------------------
extractNASC(
  # Most used options ----------------------------------------------------------
  path.in     = "\\\\swc-storage3-s\\AST3\\SURVEYS\\20220627_LISA-MARIE_SummerCPS\\PROCESSED\\EV\\CSV", # CSV file source                                                                                                 e
  pattern.in  = "_CPS-Final 38 kHz CPS.csv", # CSV file regex
  path.out    = "\\\\swc-storage3-s\\AST3\\SURVEYS\\20220627_LISA-MARIE_SummerCPS\\PROCESSED\\EV\\CSV", # Processed file destination
  suffix.out  = "_nasc_cps.csv",             # Suffix applied to processed CSV files
  path.img    = "\\\\swc-storage3-s\\AST3\\SURVEYS\\20220627_LISA-MARIE_SummerCPS\\PROCESSED\\EV\\Exported_Images", # Location of exported image files, or NULL
  pattern.img = "_CPS-Final 38 kHz CPS.png", # Exported image regex
  # Lesser used options --------------------------------------------------------
  expansion   = 2,     # Constant for expanding axes
  max.range   = 350,   # Depth range for bubble plots
  root        = 2,     # Constant for controlling bubble size (2)
  scaling     = 0.1,   # Constant for controlling bubble size (0.1)
  jpeg        = FALSE, # Save intermediate plots from line picks
  x11.w       = 1600,  # Width of graphics window (px)
  x11.h       = 600)   # Height of graphics window (px)

# # Configure input and output paths ---------------------------------------------
# ## CSV input path (source)
# path.in  <- here("Data/Backscatter/RL") # KLS laptop
# # path.in <- "C:/SURVEY/2207RL/PROCESSED/EV/CSV/LASKER" # Echoview PC
