library(tidyverse)
library(photobiology)

df <- data.frame(date = lubridate::ymd(c("2021-07-12","2021-08-01","2021-09-11","2021-10-05")),
                 lat = c(48.5, 40, 33.7, 32),
                 long = c(-124.5, -123.6, -119.9, -117))

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
