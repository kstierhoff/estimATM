if (get.nav.sd) {
  # Load existing nav data
  if (file.exists(here("Data/Nav/nav_data_saildrone.Rdata"))) {
    load(here("Data/Nav/nav_data_saildrone.Rdata"))
    
    # Calculate difference between max nav time and now
    nav.lag.sd <- difftime(now(tzone = "UTC"), 
                           max(ymd_hms(nav.sd$datetime), na.rm = TRUE), 
                           units = "hours")
    
    # Get new erddap start date from max date
    erddap.survey.start.sd <- paste0(format(max(date(nav.sd$time)), ""), "T00%3A00%3A00Z")
  } else {
    nav.lag.sd <- 24
  }
  
  if (get.nav) {
    # Update only every 24 h to reduce processing time, especially at sea
    if (nav.lag.sd >= 24) {
      # Generate ERDDAP URL
      saildroneURL <- URLencode(paste0(
        erddap.url.sd, ".csv0?", erddap.vars.sd,
        "&time%3E=", erddap.survey.start.sd, "&time%3C=", erddap.survey.end.sd))
      
      # Download and parse ERDDAP nav data
      nav.temp.sd <- read_csv(saildroneURL, lazy = FALSE,
                              col_names = erddap.headers.sd) %>% 
        mutate(datetime = ymd_hms(time)) %>%
        filter(!is.nan(lat), !is.nan(long)) %>%
        filter(long != 0, lat != 0)
      
      # Append new nav data
      if (exists("nav.sd")) {
        nav.sd <- bind_rows(nav.sd, nav.temp.sd) %>% 
          distinct()
      } else {
        nav.sd <- nav.temp.sd
      }
    }
    
    # Convert saildrone nav to spatial
    nav.sd.sf <- st_as_sf(nav.sd, coords = c("long","lat"), crs = crs.geog)
    
    # Get most recent vessel position for plotting
    nav.now.sd <- nav.sd.sf %>% 
      group_by(saildrone) %>% 
      slice(n()) %>% 
      mutate(label = paste("Saildrone", saildrone, "Last position:", datetime, "UTC"),
             popup = paste0('<b>Saildrone: </b>', saildrone, '<br/>',
                            '<b>Last position: </b>', datetime, ' UTC<br/>'))
    
    # Convert saildrone nav to spatial
    nav.sd.paths.sf <- nav.sd.sf %>% 
      group_by(saildrone) %>% 
      summarise(do_union = FALSE) %>% 
      st_cast("LINESTRING") %>% 
      mutate(tracklength = st_length(.))
    
    # Save nav data
    save(nav.sd, nav.sd.sf, nav.sd.paths.sf, nav.now.sd, 
         file = here("Data/NAV/nav_data_saildrone.Rdata"))
    
    # Create gps.csv files from ERDDAP data
    for (i in unique(nav.sd$saildrone)) {
      saildrone.gps <- filter(nav.sd, saildrone == i) %>% 
        mutate(GPS_date = format(datetime, format = "%F"),
               GPS_time = format(datetime, format = "%T")) %>% 
        select(GPS_date, GPS_time, latitude = lat, longitude = long)
      
      write_csv(saildrone.gps, here("Output", paste(i, "_erddap.gps.csv", sep = "")))
    }
  } else {
    # Load nav data
    load(here("Data/NAV/nav_data_saildrone.Rdata"))
  }
}
