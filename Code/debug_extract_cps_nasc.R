# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, here, fs)
# Install and load required packages from Github -------------------------------
pacman::p_load_gh("kstierhoff/atm")

# Most used options ----------------------------------------------------------
path.in     = "\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_SAILDRONE_SummerCPS\\PROCESSED\\EV\\CSV" # CSV file source                                                                                                 e
pattern.in  = "_CPS-Final 38 kHz CPS.csv" # CSV file regex
path.out    = "\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_SAILDRONE_SummerCPS\\PROCESSED\\EV\\CSV" # Processed file destination
suffix.out  = "_nasc_cps.csv"             # Suffix applied to processed CSV files
path.img    = "\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_SAILDRONE_SummerCPS\\PROCESSED\\EV\\Exported_Images" # Location of exported image files, or NULL
pattern.img = "_CPS-Final 38 kHz CPS.png" # Exported image regex
# Lesser used options --------------------------------------------------------
expansion   = 2     # Constant for expanding axes
max.range   = 350   # Depth range for bubble plots
root        = 2     # Constant for controlling bubble size (2)
scaling     = 0.1   # Constant for controlling bubble size (0.1)
jpeg        = FALSE # Save intermediate plots from line picks
x11.w       = 1600  # Width of graphics window (px)
x11.h       = 600   # Height of graphics window (px)
