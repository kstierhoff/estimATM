# A script for replacing geographic position data (i.e., lat/long) in SAMOS files using SCS data

# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,lubridate,mapview,here,xts,fs,sf)

# Install and load required packages from Github -------------------------------
pacman::p_load_gh("kstierhoff/surveyR")

# Import from CSV
samos.file.in  <- "SAMOS-OBS_001.elg"
samso.file.out <- "SAMOS-OBS_fixed.elg"

moa.files <- dir_ls(regexp = here("Data/SCS/MOA.*.elg")) 

moa <- data.frame()

for (i in moa.files) {
  # i = moa.files[1]
  
  tmp <- read_csv(i) %>% 
    select(Date, Time, lat = `GP170-Lat`, long = `GP170-Lon`) %>% 
    mutate(lat = round(conv.dd(lat), 5),
           long = round(conv.dd(long), 5),
           datetime = mdy_hms(paste(Date, Time)),
           time.align = align.time(datetime, n = 30))
  
  moa <- bind_rows(moa, tmp)
}

# Arrange by date/time
moa <- arrange(moa, time.align)

# Plot data
ggplot(moa, aes(long, lat)) + geom_path()

samos <- read_csv(samos.file.in) %>% 
  mutate(time.align = align.time(mdy_hms(paste(Date, Time)), n = 30)) %>% 
  left_join(select(moa, time.align, datetime.moa = datetime, long, lat)) %>% 
  mutate(datetime = mdy_hms(paste(Date, Time)),
         diff.time = difftime(datetime, datetime.moa, units = "secs"),
         `SAMOS-Lat-VALUE` = lat,
         `SAMOS-Lon-VALUE` = long) %>%
  select(-X28, -time.align, -datetime.moa, -long, -lat, -datetime, -diff.time) %>% 
  filter(!is.na(`SAMOS-Lat-VALUE`), !is.na(`SAMOS-Lon-VALUE`))


ggplot(moa, aes(long, lat)) + geom_path() +
  geom_point(data = samos, aes(`SAMOS-Lon-VALUE`, `SAMOS-Lat-VALUE`), colour = "red") +
  coord_map()

write_csv(samos, samos.file.out)
