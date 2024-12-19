# Create gps.csv file for Echoview processing from SCS data

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")
if (!require("pak")) install.packages("pak")

# Load libraries
pacman::p_load(tidyverse, here, lubridate, fs)

# Install and load required packages from Github -------------------------------
if (!require("atm")) pkg_install("SWFSC/atm")
if (!require("surveyR")) pkg_install("SWFSC/surveyR")
pacman::p_load_gh("SWFSC/atm")
pacman::p_load_gh("SWFSC/surveyR")

# Specify nav file source
# nav.path <- "U:/SURVEYS/20220627_LASKER_SummerCPS/DATA/SCS/AST CONTINUOUS" # At sea
nav.path <- here("Data/SCS/Leg2/EventData/AST CONTINUOUS") # Shore side

# Specify path to save gps.csv file
dir_create("U:/SURVEYS/20220627_LASKER_SummerCPS/PROCESSED/REPROCESSING/GPS")
gps.path <- "U:/SURVEYS/20220627_LASKER_SummerCPS/PROCESSED/REPROCESSING/GPS"

# Set start and end date to reduce file size (makes matching faster, I think)
tx.name  <- "127C"
start_date <- ymd_hms("2022-07-26 00:00:00")
end_date   <- ymd_hms("2022-07-26 17:00:00")
# end_date <- ymd(Sys.Date())

# Read and format SCS data; filter for start and end date
nav.csv <- dir_ls(nav.path, regexp = "MOA.*.elg") %>% 
  map_df(read_csv) %>% 
  mutate(datetime  = mdy_hms(paste(Date, Time))) %>% 
  filter(between(datetime, start_date, end_date)) %>% 
  select(GPS_date  = Date, GPS_time = Time, latitude = "GP170-Lat",longitude = "GP170-Lon") %>% 
  mutate(latitude  = scs2dd(latitude),
         longitude = scs2dd(longitude),
         GPS_date  = mdy(GPS_date)) 

# Create gps.csv file from nav to replace missing data in Echoview
nav.gps <- nav.csv %>% 
  mutate(GPS_date = format(GPS_date, format = "%F"),
         GPS_time = format(GPS_time, format = "%T")) %>% 
  select(GPS_date, GPS_time, latitude, longitude)

# Write file
write_csv(nav.gps, file.path(gps.path, paste0(tx.name, ".gps.csv")))

# Plot results
ggplot(nav.csv, aes(longitude, latitude)) + geom_path()

# Save plot
ggsave(file.path(gps.path, paste0(tx.name, ".png")))

       
