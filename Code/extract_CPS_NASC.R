# A utility for extracting CPS backscatter (cps.NASC)
# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(here,fs)

# Install and load required packages from Github -------------------------------
## atm
pacman::p_load_gh("kstierhoff/atm")

# Load CTDapp function
source(here::here("Code/estimate_CPS_NASC.R"))

# Configure input and output paths
path.in  <- here("Data/Backscatter/RL")
path.out <- path.in

# List CSV files (this is just for viewing the files; it's not used in the code)
list.files(path = path.in, pattern = "CPS-Final 38kHz CPS.csv", recursive = FALSE)

# Run the function to estimate CPS backscatter
estimate.cps.nasc(path.input = path.in, 
                  pattern = "CPS-Final 38kHz CPS.csv", 
                  path.output = path.out, 
                  expand.right = TRUE, 
                  expansion = 4)

# Close both graphics devices
dev.off()
dev.off()
