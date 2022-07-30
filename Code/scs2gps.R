# Create gps.csv file for Echoview processing from SCS data

# Load libraries
pacman::p_load(tidyverse, here, lubridate, fs)
pacman::p_load_gh("kstierhoff/surveyR")

# Specify nav file source
# nav.path <- "//10.48.23.223/log40/EventData/AST CONTINUOUS" # At sea
nav.path <- here("Data/SCS") # Shore side

# Specify path to save gps.csv file
dir_create("C:/SURVEY/2207RL/PROCESSED/EV/GPS")
gps.path <- "C:/SURVEY/2207RL/PROCESSED/EV/GPS/120A.gps.csv"

# Set start and end date to reduce file size (makes matching faster, I think)
start_date <- ymd_hms("2022-07-28 12:12:01")
end_date   <- ymd_hms("2022-07-30 00:00:01")
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
write_csv(nav.gps, gps.path)

# Plot results
ggplot(nav.csv, aes(longitude, latitude)) + geom_path()
