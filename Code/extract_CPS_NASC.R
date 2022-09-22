# A utility for extracting CPS backscatter (cps.NASC)
# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse, here,fs)

# Install and load required packages from Github -------------------------------
## atm
pacman::p_load_gh("kstierhoff/atm")

# Load CTDapp function
source(here::here("Code/estimate_CPS_NASC.R"))

# Configure input and output paths
path.in  <- here("Data/Backscatter/RL")
path.out <- here("Data/Backscatter/RL")
# path.in  <- "C:/SURVEY/2107RL/PROCESSED/EV/CSV/SAILDRONE/SD1059"
# path.out <- "C:/SURVEY/2107RL/PROCESSED/EV/CSV/SAILDRONE/SD1059"
# path.in  <- "C:/Users/josiah.renfree/Desktop/2107RL_SD_Reprocessing/EV/CSV/SD1059/CPSFilter_-13dB"
# path.out <- "C:/Users/josiah.renfree/Desktop/2107RL_SD_Reprocessing/EV/CSV/SD1059/CPSFilter_-13dB/POST-PROCESSED"

# List CSV files (this is just for viewing the files; it's not used in the code)
list.files(path = path.in, pattern = "CPS-Final 38 kHz CPS.csv", recursive = FALSE)

# Run the function to estimate CPS backscatter
estimate.cps.nasc(path.input = path.in, 
                  pattern = "CPS-Final 38 kHz CPS.csv", 
                  path.output = path.out, 
                  expand.right = T,
                  expand.left = F,
                  expansion = 2, max.range = 350, root = 2,  scaling = 0.5)

# Close both graphics devices
dev.off()
dev.off()

# Recreate Juan's original bubble plots ----------------------------------------
# List new nasc_cps.csv files
list.files(path = path.out, pattern = "CPS-Final 38 kHz CPS_nasc_cps.csv", recursive = FALSE)

masked.file.list <- list.files(path = path.out, pattern = "CPS-Final 38 kHz CPS_nasc_cps.csv", recursive = FALSE)
print(masked.file.list)

print("Type the order of the file you want to integrate")
masked.file.num <- scan(n = 1)

# Create file info
new.masked.filename <- masked.file.list[masked.file.num]
new.masked.path <- file.path(path.out, new.masked.filename)
new.masked.plot <- paste0(fs::path_ext_remove(fs::path_file(new.masked.path)), ".png")

# Read new nasc_cps.csv file
new.masked.file <- read_csv(new.masked.path)

## Summarize file for plotting the seabed depth
max.layer.depth <- new.masked.file %>% 
  group_by(Dist_M) %>% 
  summarize(max.depth = -max(Depth_mean))

## Create the bubble plot
cps.nasc.bubble <- ggplot(new.masked.file) +
  geom_point(
    data = filter(new.masked.file, NASC > 0),
    aes(Dist_M, -Depth_mean, size = NASC),
    shape = 21, fill = NA, alpha = 0.5, show.legend = FALSE) +
  geom_point(
    data = filter(new.masked.file, cps.nasc > 0),
    aes(Dist_M, -Depth_mean, size = cps.nasc),
    alpha = 0.5, colour = "blue", show.legend = FALSE ) +
  geom_line(
    data = filter(max.layer.depth),
    aes(Dist_M, max.depth), alpha = 0.5 ) +
  geom_line(aes(Dist_M, -top.habitat), colour = "red", linetype = "dashed") +
  geom_line(aes(Dist_M, -bottom.habitat), colour = "blue", linetype = "dashed") +
  theme_bw() +
  labs(x = "Echoview distance (M)", y = "Mean depth (m)")

# Save the plot
ggsave(cps.nasc.bubble,
       filename = file.path(path.out, new.masked.plot),
       width = 15, height = 7
)
