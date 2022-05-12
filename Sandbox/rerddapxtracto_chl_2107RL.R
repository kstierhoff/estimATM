library(tidyverse)
library(lubridate)
library(here)
library(fs)
library(xts)

chl.files <- dir_ls(here("Data/SCS/Fluoro"), regexp = "*.Raw")

chl <- data.frame()

for (i in chl.files) {
  chl <- bind_rows(chl,
                   read_csv(i, col_names = c("date","time","value")) %>% 
                     rowwise() %>% 
                     mutate(chl_char = unlist(str_split(value, "="))[2],
                            chl = str_extract(chl_char, "\\d{1,3}\\.\\d{1,3}")) %>%
                     ungroup() %>% 
                     mutate(datetime = mdy_hms(paste(date, time))))
}

chl.summ <- chl %>% 
  mutate(time.align = align.time(datetime, n = 60),
         chl = as.numeric(chl)) %>% 
  group_by(time.align) %>% 
  summarise(chl_mean = mean(chl, na.rm = TRUE),
            chl_sd   = sd(chl, na.rm = TRUE))

# Load Lasker nav
load(here("Data/Nav/nav_data.Rdata"))

# Merge nav and chl data
nav2 <- nav %>%
  mutate(time.align = align.time(time, n = 60)) %>% 
  left_join(chl.summ)

ggplot() + 
  geom_point(data = nav2, aes(long, lat)) +
  geom_point(data = filter(nav2, !is.na(chl_mean)), aes(long, lat), colour = "green") +
  coord_map()

##xtracto
library("rerddap")
library("rerddapXtracto")

chl.modis.info <- info('erdMWchla8day_LonPM180')
chl.modis.info # it matches the location and time of our data set

sst.info <- rerddap::info('erdMWsstd8day_LonPM180')
sst.info # it matches the location and time of our data set

data <- filter(nav2, !is.na(chl_mean),
               lat > 40)

xpos <- data$long
ypos <- data$lat
tpos <- data$time
zpos <-  rep(0, length(tpos))

ggplot() + 
  geom_point(data = nav2, aes(long, lat)) +
  geom_point(data = filter(nav2, !is.na(chl_mean)), aes(long, lat), colour = "green") +
  coord_map()

ggplot() + 
  geom_point(data = data, aes(long, lat)) +
  geom_point(data = filter(data, !is.na(chl_mean)), aes(long, lat, size = chl_mean),
             shape = 21, fill = NA, colour = "green") +
  coord_map()

chl.modis <- rxtracto(chl.modis.info, parameter = 'chlorophyll', xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord  = zpos,                    xlen = .04, ylen = .04, progress_bar = TRUE) #
names(chl.modis)

sst.isst <- rxtracto(sst.info, parameter = 'sst', xcoord = xpos, ycoord = ypos, tcoord = tpos, zcoord  = zpos,
                     xlen = .04, ylen = .04, progress_bar = TRUE) #
names(sst.isst)

str(chl.modis)

# Add MODIS chl data
data$chl_modis <- chl.modis$`mean chlorophyll`
data$sst_viirs <- sst.isst$`mean sst`

ggplot(filter(data, !is.na(chl_modis))) + 
  geom_point(aes(chl_mean, log10(chl_modis))) + 
  coord_equal()

ggplot(filter(data, !is.na(SST))) + 
  geom_point(aes(SST, sst_viirs)) + 
  geom_abline(yintercept = 0, slope = 1) +
  coord_equal(xlim  = c(8, 18),
               ylim = c(8, 18))

data %>% 
  filter(is.na(chl_modis))

write_csv(data, here("Output/chlorophyll_kls.csv"))




