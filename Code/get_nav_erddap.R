if (get.nav) {
  # Load existing nav data
  if (file.exists(here("Data/Nav/nav_data.Rdata"))) {
    load(here("Data/Nav/nav_data.Rdata"))
    
    # Calculate difference between max nav time and now
    nav.lag <- difftime(now(tzone = "UTC"), max(ymd_hms(nav$time)), units = "hours")
    
    # Get new erddap start date from max date
    erddap.survey.start <- max(date(nav$time))
  } else {
    nav.lag <- 24
  }
  
  if (nav.lag >= 24) {
    # Generate ERDDAP URL
    dataURL <- URLencode(paste(
      "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
      erddap.vessel, ".csv0?", erddap.vars,
      "&time>=", erddap.survey.start, "&time<=", erddap.survey.end,
      sep = ""))
    
    # Download and parse ERDDAP nav data
    nav.temp <- read_csv(dataURL, lazy = FALSE,
                         col_names = erddap.headers) %>% 
      mutate(long     = long - 360,
             SOG      = SOG * 1.94384,
             SST      = na_if(SST, NaN),
             wind_brg = case_when(
               wind_dir < 180 ~ wind_dir + 180,
               TRUE ~ wind_dir - 180),
             wind_angle = (wind_dir/360)*2*pi,
             leg      = paste("Leg", 
                              cut(as.numeric(date(time)), 
                                  leg.breaks, 
                                  labels = FALSE))) 
    
    # Append new nav data
    if (exists("nav")) {
      nav <- bind_rows(nav, nav.temp) %>% 
        distinct()
    } else {
      nav <- nav.temp
    }
  }
  
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
    st_cast("LINESTRING")
  
  # Save results
  save(nav, nav.sf, nav.paths.sf, file = here("Data/Nav/nav_data.Rdata"))
} else {
  if (exists(here("Data/Nav/nav_data.Rdata"))) {
    # Load nav data
    load(here("Data/Nav/nav_data.Rdata")) 
  }
}
