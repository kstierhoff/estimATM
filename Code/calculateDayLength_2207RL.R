# A script for computing sunrise, sunset, daylength, nightlenght, etc. for
# the 2022 Summer CCE survey (2207RL)

library(tidyverse)
library(photobiology)
library(lubridate)

# Create a data frame with survey dates and positions
df <- data.frame(date = lubridate::ymd(c("2022-7-05","2022-08-01","202209-11","2022-09-30")),
                 lat = c(44.2, 40, 33.7, 32),
                 long = c(-124.011, -123.6, -119.9, -117))

# Create data frame for data
survey_daynight <- data.frame()
  
for (i in 1:nrow(df)) {
  tmp <- day_night(date = df$date[i], 
                   geocode = data.frame(lat = df$lat[i], lon = df$long[i]),
                   twilight = "none")
  survey_daynight <- bind_rows(survey_daynight, tmp)
}

# Format results
survey_daynight <- survey_daynight %>% 
  mutate(sunrise_utc = ymd_hms(paste(day, hms::as_hms(sunrise*3600)), tz = "UTC") - minutes(0),
         sunset_utc  = sunrise_utc + daylength*3600 + minutes(0),
         sunrise_loc = with_tz(sunrise_utc, tzone = "America/Los_Angeles"),
         sunset_loc = with_tz(sunset_utc, tzone = "America/Los_Angeles")) %>% 
  select(day, sunrise_utc, sunset_utc, sunrise_loc, sunset_loc, daylength, nightlength) %>% 
  tibble()

# Write to file
write_csv(survey_daynight, here::here("Output/survey_daynight.csv"))

# Leg 1
day_length(
  date = df$date[1],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[1], lon = df$long[1]),
  # geocode = data.frame(lat = df$lat[1], lon = df$long[1]),
  twilight = "sunlight",
  unit.out = "hours"
)

# Leg 2
day_length(
  date = df$date[2],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[2], lon = df$long[2]),
  twilight = "sunlight",
  unit.out = "hours"
)

# Leg 3
day_length(
  date = df$date[3],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[3], lon = df$long[3]),
  twilight = "sunlight",
  unit.out = "hours"
)

# Leg 4
day_length(
  date = df$date[4],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[4], lon = df$long[4]),
  twilight = "sunlight",
  unit.out = "hours"
)
