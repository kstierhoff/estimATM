if (get.nav) {
  
  # Load existing nav data
  if (file.exists(here("Data/Nav/nav_data.Rdata"))) {
    load(here("Data/Nav/nav_data.Rdata"))
    
    # Calculate difference between max nav time and now
    nav.lag <- difftime(now(tzone = "UTC"), 
                        max(ymd_hms(nav$time), na.rm = TRUE), 
                        units = "hours")
    
    # Get new ERDDAP start date from max date
    erddap.survey.start.new <- date(tail(nav$time,1))
  } else {
    nav.lag <- 24
    # Set new ERDDAP start date equal to original
    erddap.survey.start.new <- erddap.survey.start
  }
  
  if (nav.lag >= 24) {
    # Generate ERDDAP URL
    dataURL <- URLencode(paste0(
      "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
      erddap.vessel, ".csv0?", erddap.vars,
      "&time>=", erddap.survey.start.new, "&time<=", erddap.survey.end))

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
                                  labels = FALSE)),
             id       = seq_along(time)) %>%
      # Identify flags not equal to "Z"
      mutate(flag_sum = nchar(str_replace_all(flag, "Z", ""))) 
    
    # Remove bad values
    if (filter.nav) {
      # Remove data with bad flags
      nav.temp <- filter(nav.temp, flag_sum == 0)
    }
    
    # Compute distance between each point, and remove points with unrealistic distances
    nav.temp.sf <- nav.temp %>% 
      st_as_sf(coords = c("long","lat"),crs = 4326) %>% 
      st_transform(crs = 3310) 
    
    # Remove points that are too far apart
    if (filter.nav) {
      nav.temp.sf <- nav.temp.sf %>% 
        mutate(distance_to_next = as.numeric(
          na.omit(c(0, st_distance(geometry,
                                   lead(geometry, 
                                        default = NA),
                                   by_element = TRUE))))/1852) %>% 
        filter(distance_to_next < 20)
    }
    
    # Subset nav data that are in nav.temp.sf
    nav.temp <- nav.temp %>% 
      filter(id %in% nav.temp.sf$id)

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
    filter(is.na(ymd_hms(time)) == FALSE,
           is.nan(SOG) == FALSE, SOG > 0, SOG < 15,
           between(lat, min(survey.lat), max(survey.lat)), 
           between(long, min(survey.long), max(survey.long)))
  
  if (filter.nav) {
    nav <- nav %>%
      filter(flag_sum == 0)
  }
  
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
