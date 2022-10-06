# Import FSV nav data from eLog files from SCS (e.g., MOA Continuous.elg)

# # Load libraries
# pacman::p_load(tidyverse, here, lubridate, fs)
# pacman::p_load_gh("kstierhoff/surveyR")
# 
# # SCS data file info
# scs.nav.path    <- "C:/SURVEY/2207RL/DATA/SCS" # Local
# scs.nav.dir     <- "AST CONTINUOUS"
# scs.nav.pattern <- "MOA.*.elg"
# scs.nav.recurse <- TRUE

if (get.nav) {
  # List MOA Continuous .elg files from the appropriate locations
  moa.files <- dir_ls(scs.nav.path, regexp = scs.nav.pattern, recurse = scs.nav.recurse) %>% 
    str_subset(scs.nav.dir)
  
  # Read and format SCS data
  nav <- moa.files %>% 
    map_df(read_csv, .id = "id") %>% 
    mutate(time  = mdy_hms(paste(Date, Time)),
           lat         = purrr::pluck(., gps.lat.hdr),
           long        = purrr::pluck(., gps.lon.hdr),
           SST         = purrr::pluck(., sst.hdr),
           SOG         = purrr::pluck(., sog.hdr),
           wind_dir    = purrr::pluck(., wind.dir.hdr),
           wind_speed  = purrr::pluck(., wind.speed.hdr),
           wind_brg = case_when(
             wind_dir < 180 ~ wind_dir + 180,
             TRUE ~ wind_dir - 180),
           wind_angle = (wind_dir/360)*2*pi,
           leg      = paste("Leg", 
                            cut(as.numeric(date(time)), 
                                leg.breaks, 
                                labels = FALSE))) %>% 
    select(time, lat, long, SST, SOG, wind_dir, wind_speed,
           wind_brg, wind_angle, leg) %>% 
    arrange(time)
  
  # Save unfiltered nav data
  saveRDS(nav, here("Data/Nav/nav_data_raw.rds"))
  
  # Filter nav data
  nav <- nav %>%
    filter(is.nan(SOG) == FALSE, SOG > 0, SOG < 15,
           between(lat, min(survey.lat), max(survey.lat)), 
           between(long, min(survey.long), max(survey.long)))
  
  # Convert nav to spatial
  nav.sf <- st_as_sf(nav, coords = c("long","lat"), crs = crs.geog) 
  
  # Cast nav to transects
  nav.paths.sf <- nav.sf %>% 
    group_by(leg) %>% 
    summarise(do_union = FALSE) %>% 
    st_cast("LINESTRING") %>% 
    mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))
  
  # Save results
  save(nav, nav.sf, nav.paths.sf, file = here("Data/Nav/nav_data.Rdata"))
  
} else {
  if (file.exists(here("Data/Nav/nav_data.Rdata"))) {
    # Load nav data
    load(here("Data/Nav/nav_data.Rdata")) 
  }
}





