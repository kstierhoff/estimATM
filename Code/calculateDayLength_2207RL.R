library(tidyverse)
library(photobiology)

df <- data.frame(date = lubridate::ymd(c("2022-7-05","2022-08-01","202209-11","2022-09-30")),
                 lat = c(44.2, 40, 33.7, 32),
                 long = c(-124.011, -123.6, -119.9, -117))

day_length(
  date = df$date[1],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[1], lon = df$long[1], address = "Victoria"),
  # geocode = data.frame(lat = df$lat[1], lon = df$long[1]),
  twilight = "sunlight",
  unit.out = "hours"
)

day_length(
  date = df$date[2],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[2], lon = df$long[2]),
  twilight = "sunlight",
  unit.out = "hours"
)

day_length(
  date = df$date[3],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[3], lon = df$long[3]),
  twilight = "sunlight",
  unit.out = "hours"
)

day_length(
  date = df$date[4],
  tz = "UTC",
  geocode = data.frame(lat = df$lat[4], lon = df$long[4]),
  twilight = "sunlight",
  unit.out = "hours"
)
