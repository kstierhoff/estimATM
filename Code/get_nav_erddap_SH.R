if (get.nav.sh) {
  
  # Load existing nav data
  if (file.exists(here("Data/Nav/nav_data_sh.Rdata"))) {
    load(here("Data/Nav/nav_data_sh.Rdata"))
    
    # Calculate difference between max nav time and now
    nav.lag.sh <- difftime(now(tzone = "UTC"), 
                           max(ymd_hms(nav.sh$time), na.rm = TRUE), 
                           units = "hours")
    
    # Get new ERDDAP start date from max date
    erddap.survey.start.sh.new <- date(tail(nav.sh$time,1))
  } else {
    nav.lag.sh <- 24
    # Set new ERDDAP start date equal to original
    erddap.survey.start.sh.new <- erddap.survey.start.sh
  }
  
  if (nav.lag.sh >= 24) {
    # Generate ERDDAP URL
    dataURL <- URLencode(paste0(
      "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
      erddap.vessel.sh, ".csv0?", erddap.vars,
      "&time>=", erddap.survey.start.sh.new, 
      "&time<=", erddap.survey.end.sh,
      "&flag=~", erddap.flags.sh))
    
    # Download and parse ERDDAP nav data
    nav.temp.sh <- read_csv(dataURL, lazy = FALSE,
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
                                  leg.breaks.sh, 
                                  labels = FALSE)),
             id       = seq_along(time)) 

    # Compute distance between each point, and remove points with unrealistic distances
    nav.temp.sh.sf <- nav.temp.sh %>% 
      st_as_sf(coords = c("long","lat"),crs = 4326) %>% 
      st_transform(crs = 3310) 
    
    # Append new nav data
    if (exists("nav.sh")) {
      nav.sh <- bind_rows(nav.sh, nav.temp.sh) %>% 
        distinct()
    } else {
      nav.sh <- nav.temp.sh
    }
  }
  
  # Save unfiltered nav data
  saveRDS(nav.sh, here("Data/Nav/nav_data_raw_sh.rds"))
  
  # Filter nav data
  nav.sh <- nav.sh %>%
    filter(is.na(ymd_hms(time)) == FALSE,
           is.nan(SOG) == FALSE, SOG > 0, SOG < 15,
           between(lat, min(survey.lat), max(survey.lat)), 
           between(long, min(survey.long), max(survey.long)))
  
  # Convert nav to spatial
  nav.sh.sf <- st_as_sf(nav.sh, coords = c("long","lat"), crs = crs.geog) 
  
  # Cast nav to transects
  nav.paths.sh.sf <- nav.sh.sf %>% 
    group_by(leg) %>% 
    summarise(do_union = FALSE) %>% 
    st_cast("LINESTRING") %>% 
    mutate(distance_nmi = as.numeric(st_length(.)*0.000539957))
  
  # Save results
  save(nav.sh, nav.sh.sf, nav.paths.sh.sf, file = here("Data/Nav/nav_data_sh.Rdata"))
  
} else {
  if (file.exists(here("Data/Nav/nav_data_sh.Rdata"))) {
    # Load nav data
    load(here("Data/Nav/nav_data_sh.Rdata")) 
  }
}

# Get most recent vessel position for plotting
nav.now.sh <- tail(nav.sh.sf, 1) %>% 
  mutate(label = paste("Last position:", time, "UTC"),
         popup = paste0('<b>Vessel name: </b>', 'Shimada', '<br/>',
                        '<b>Last position: </b>', time, ' UTC<br/>'))
