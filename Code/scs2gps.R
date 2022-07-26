# Create gps.csv file for Echoview processing from SCS data

# Load libraries
pacman::p_load(tidyverse, here, lubridate)
pacman::p_load_gh("kstierhoff/surveyR")

# Set start and end date to reduce file size (makes matching faster, I think)
start_date <- ymd("2022-07-25")
end_date <- ymd("2022-07-30")
# end_date <- ymd(Sys.Date())

# Read and format SCS data; filter for start and end date.
nav.csv <- read_csv(here("Data/SCS/2207RL_GPS.csv")) %>% 
  select(GPS_date = Date, GPS_time = Time, latitude = "GP170-Lat",longitude = "GP170-Lon") %>% 
  mutate(latitude = scs2dd(latitude),
         longitude = scs2dd(longitude),
         GPS_date = mdy(GPS_date)) %>% 
  filter(between(GPS_date, start_date, end_date))

# Create gps.csv file from nav to replace missing data in Echoview
nav.gps <- nav.csv %>% 
  mutate(GPS_date = format(GPS_date, format = "%F"),
         GPS_time = format(GPS_time, format = "%T")) %>% 
  select(GPS_date, GPS_time, latitude, longitude)

# Write file
write_csv(nav.gps, here("Output/nav.gps.csv"))

# Plot results
ggplot(nav.csv, aes(longitude, latitude)) + geom_path()
