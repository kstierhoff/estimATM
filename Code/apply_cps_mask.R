# Load libraries ------------------------------------------------
library(tidyverse)
library(here)
library(fs)
library(data.table)
library(cowplot)

# Define mask function --------------------------------------------
apply.cps.mask <- function(masked.file , new.integrated.file){
  if(ncol(masked.file) !=  (ncol(new.integrated.file) + 3))
    stop("The files are not compatible - wrong number of columns")
  
  if(nrow(masked.file) !=  (nrow(new.integrated.file) ))
    stop("The files are not compatible - wrong number of rows")
  
  if(sum( !is.na(    match( names(masked.file) , c("bottom.habitat", "top.habitat" , "cps.nasc")) )) != 3)
    stop("The files are not compatible - missing one or more of the masked file variables")
  
  new.integrated.file$cps.nasc <- new.integrated.file$NASC
  new.integrated.file$top.habitat <-  masked.file$top.habitat
  new.integrated.file$bottom.habitat <-  masked.file$bottom.habitat
  
  new.integrated.file$cps.nasc[new.integrated.file$Depth_mean < new.integrated.file$top.habitat | new.integrated.file$Depth_mean > new.integrated.file$bottom.habitat] <- 0
  
  return(new.integrated.file)
  
}

# File info ---------------------------------------------------------
## Data directory
csv.dir <- here("Data/Backscatter/SD")

## File names
masked.file.name         <- "2107SD1059_119_CPS-Final 38 kHz CPS_nasc_cps.csv"
new.integrated.file.name <- "2107SD1059_119-CPS-Final 38 kHz CPS.csv"

# Extract mask 
masked.file         <- fread(file.path(csv.dir, masked.file.name))
# Read newly integrate file
new.integrated.file <- fread(file.path(csv.dir, new.integrated.file.name))
# Apply mask to new results
new.masked.file     <- apply.cps.mask(masked.file , new.integrated.file) %>% 
  mutate(
    # Add original cps.nasc for plotting
    cps.nasc.mask = masked.file$cps.nasc,
    # Compute diff between cps.nasc.values for plotting
    cps.nasc.diff = cps.nasc - cps.nasc.mask)

# Create name for output file
new.masked.file.name <- paste0(fs::path_ext_remove(fs::path_file(file.path(csv.dir, masked.file.name))), "_mask.csv")
new.masked.plot.name <- paste0(fs::path_ext_remove(fs::path_file(file.path(csv.dir, masked.file.name))), "_mask.png")
new.masked.bubble.name <- paste0(fs::path_ext_remove(fs::path_file(file.path(csv.dir, masked.file.name))), "_bubble.png")

# Plot results --------------------------------
## Compare results from original
## Original NASC has to be the same or larger than CPS NASC
orig.data <- ggplot(new.masked.file, aes(NASC, cps.nasc)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  coord_equal() + 
  theme_bw()

# On the re-integrated files, cps.nasc has to be the same or larger than cps.nasc from the previous integration 
masked.data <- ggplot(new.masked.file, aes(cps.nasc.mask, cps.nasc)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  coord_equal() + 
  theme_bw()

# Combine plots for saving
compare.data <- plot_grid(orig.data, masked.data, nrow = 1, align = "hv")

ggsave(compare.data, 
       filename = file.path(csv.dir, new.masked.plot.name))

# Write files to CSV file
new.masked.file %>% 
  select(-cps.nasc.mask, -cps.nasc.diff) %>% 
  write_csv(file = file.path(csv.dir, new.masked.file.name))

# cps.nasc.map <- ggplot(new.masked.file, aes(Lon_M, Lat_M)) +
#   geom_point(aes(size = cps.nasc)) +
#   geom_point(aes(size = cps.nasc.mask), colour = "red") + 
#   # scale_colour_viridis_c(option = "inferno") +
#   coord_equal() +
#   theme_bw()

# Recreate Juan's original bubble plots
## Summarize file for plotting the seabed depth
max.layer.depth <- new.masked.file %>% 
  group_by(Interval) %>% 
  summarize(max.depth = -max(Depth_mean))

# min.layer.depth <- new.masked.file %>% 
#   filter(Exclude_above_line_range_mean != -9999) %>% 
#   group_by(Interval) %>% 
#   summarize(min.depth = -min(Exclude_above_line_range_mean))

## Create the bubble plot
cps.nasc.bubble <- ggplot(new.masked.file) +
  geom_point(data = filter(new.masked.file, NASC > 0),
             aes(Interval, -Depth_mean, size = NASC),
             alpha = 0.5, show.legend = FALSE) +
  geom_point(data = filter(new.masked.file, cps.nasc > 0),
             aes(Interval, -Depth_mean, size = cps.nasc),
             alpha = 0.5, colour = "blue", show.legend = FALSE) +
  geom_line(data = filter(max.layer.depth),
             aes(Interval, max.depth),
             alpha = 0.5) +
  # geom_line(data = filter(min.layer.depth),
  #           aes(Interval, min.depth),
  #           alpha = 0.5) +
  geom_line(aes(Interval, -top.habitat), colour = "red", linetype = "dashed") +
  geom_line(aes(Interval, -bottom.habitat), colour = "blue", linetype = "dashed") +
  theme_bw() +
  labs(x = "Interval", y = "Mean depth (m)")

# Save the plot
ggsave(cps.nasc.bubble, 
       filename = file.path(csv.dir, new.masked.bubble.name),
       width = 15, height = 7)

# Ignore below -----------------------------------------------------------------
# # 1036; also appears good
# masked.file         <- fread(file.path(csv.dir, "2107SD1036_070_CPS-Final 38 kHz CPS_nasc_cps.csv"))
# new.integrated.file <- fread(file.path(csv.dir, "2107SD1036_070-Final 38 kHz CPS.csv"))
# 
# # 1055; these appear good after a few tests
# masked.file         <- fread(file.path(csv.dir, "2107SD1055_133_CPS-Final 38 kHz CPS_nasc_cps.csv"))
# new.integrated.file <- fread(file.path(csv.dir, "2107SD1055_133-Final 38 kHz CPS.csv"))

# Name of original file
# 1059 sucks, no files match, different rows in each
# Might need to run these through Juan's extract_cps_nasc script again
# masked.file         <- fread(file.path(csv.dir, "2107SD1059_131-2_CPS-Final 38 kHz CPS_nasc_cps.csv"))
# new.integrated.file <- fread(file.path(csv.dir, "2107SD1059_131-2_CPS-Final 38 kHz CPS.csv"))

# Look at data frame dimensions
# dim(masked.file)
# dim(new.integrated.file)

# # these are a couple of diagnostic. Run them to make sure it's all working well.
# plot(test$cps.nasc , test$NASC)
# par(mfrow = c(1,2))
# # original NASC has to be the same or larger than CPS NASC
# plot(test$cps.nasc, test$NASC) 
# abline(0,1)
# # on the re-integrated files, cps.nasc has to be the same or larger than cps.nasc from the previous integration. 
# # In this case, it will be a straight line
# plot(masked.file$cps.nasc, test$cps.nasc) 
# abline(0,1)





