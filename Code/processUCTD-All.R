# A script for processing all UCTD casts, to be used in reports, etc.

## Process UCTD header files #####
# List raw UCTD ASCII files
uctd.hdr <- dir_ls(here("Data/UCTD"), regexp = uctd.hdr.pattern) %>% 
  path_filter(regexp = "_processed", invert = TRUE)

# Plot UCTD casts
if (length(uctd.hdr) > 0) {
  if (process.ctd) {
    # Create a data frame for header info  
    all.uctd.hdr <- data.frame()
    
    for (i in uctd.hdr) {
      # Extract header information
      all.uctd.hdr <- bind_rows(all.uctd.hdr,
                                extract_ctd_header(i, type = "UCTD"))
    }
    
    # # Add a column indicating survey vessel
    # all.uctd.hdr <- mutate(all.uctd.hdr, Vessel = str_match(all.uctd.hdr$cast, pattern = "(\\w{2})_")[,2])
    
    # Process UCTD cast files --------------------------------------------------
    # List processed UCTD cast files
    uctd.proc <- dir_ls(here("Data/UCTD"), pattern = uctd.cast.pattern) %>% 
      path_filter(regexp = "_processed")
    
    # create a data frame for storing results
    all.uctd.casts <- data.frame()
    
    # Process all UCTD casts
    for (i in uctd.proc) {
      all.uctd.casts <- bind_rows(all.uctd.casts, 
                                  extract_ctd_cast(i, type = "UCTD"))
    }
    
    # Arrange and filter results
    all.uctd.casts <- arrange(all.uctd.casts, cast, scan) %>% 
      filter(between(S, min.S, max.S)) %>%  # Remove bad salinity data
      filter(dZt < 7)
    
    # Save results
    save(all.uctd.casts, all.uctd.hdr, uctd.hdr, uctd.proc,
         file = here("Data/UCTD/uctd_data.Rdata"))
  } else {
    # Load results
    load(here("Data/UCTD/uctd_data.Rdata"))
  }
  
  # Generate ERDDAP URL
  # dataURL <- URLencode(paste0(
  #   "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
  #   erddap.vessel, ".csv0?", erddap.vars,
  #   "&time>=", 
  #   date(min(all.uctd.hdr$cast.date) - days(1)), 
  #   "&time<=", 
  #   date(max(all.uctd.hdr$cast.date) + days(1))))
  
  # Download and parse ERDDAP nav data
  uctd.nav <- nav
  # uctd.nav <- read_csv(dataURL, lazy = FALSE,
  #                      col_names = erddap.headers) %>% 
  #   mutate(long     = long - 360,
  #          SOG      = SOG * 1.94384,
  #          SST      = na_if(SST, NaN),
  #          wind_brg = case_when(
  #            wind_dir < 180 ~ wind_dir + 180,
  #            TRUE ~ wind_dir - 180),
  #          wind_angle = (wind_dir/360)*2*pi,
  #          leg      = paste("Leg", 
  #                           cut(as.numeric(date(time)), 
  #                               leg.breaks, 
  #                               labels = FALSE)))
  
  # Match UCTD headers to nav data
  nav.match.uctd <- data.frame()
  
  for (i in seq_along(all.uctd.hdr$cast)) {
    min.diff       <- which.min(abs(difftime(all.uctd.hdr$cast.date[i], uctd.nav$time)))
    nav.match.uctd <- bind_rows(nav.match.uctd, uctd.nav[min.diff, ])
  }
  
  # Combine header and nav data
  all.uctd.hdr <- all.uctd.hdr %>% 
    bind_cols(nav.match.uctd) %>% 
    mutate(lag = difftime(cast.date, time)) %>% 
    arrange(cast.date)
  
  # Summarize UCTD cast results
  uctd.summ <- all.uctd.casts %>% 
    group_by(cast) %>% 
    summarise(
      time = round(sum(dt),0),
      max.depth = round(min(Z),0)) %>%
    left_join(select(all.uctd.hdr, cast, cast.date, lat, long, SOG)) %>%
    arrange(cast.date) %>%
    mutate(cast.num = seq(1, n()),
           leg = paste("Leg", cut(as.numeric(date(cast.date)), leg.breaks, labels = FALSE)))
  
  # Write table to CSV
  write.csv(uctd.summ, file = here("Output/cast_summary_uctd.csv"), 
            quote = FALSE, row.names = FALSE)
  
  # Add SOG to all.uctd
  all.uctd.casts <- all.uctd.casts %>% 
    left_join(select(uctd.summ, cast, SOG)) %>% 
    mutate(survey = as.factor(survey.name))
  
  # Summarize uctd casts for water classification
  uctd.class <- 
    group_by(all.uctd.casts, cast) %>% 
    summarise(
      min.T = min(T),
      min.S = min(S),
      max.T = max(T),
      max.S = max(S)) %>% 
    # Assign classes based on salinity
    mutate(class = case_when(
      min.S <= 31.4 ~ "Type 1",
      min.S >= 33.4 ~ "Type 2",
      TRUE ~ "Type 3"))
  
  # Add water mass to the summary table and cast data for plotting
  all.uctd.casts <- all.uctd.casts %>% 
    left_join(select(uctd.class, cast, class)) %>% 
    left_join(select(uctd.summ, cast, leg))
  
  uctd.summ <- uctd.summ %>% 
    left_join(select(uctd.class, cast, class)) %>% 
    select(cast.num, cast, cast.date, lat, long, SOG, 
           time, max.depth, class, leg)
  
  # Remove unprocessed casts from the summary
  uctd.missing <- all.uctd.hdr %>% 
    filter(!cast %in% unique(all.uctd.casts$cast))
  
  # Exclude bad casts
  all.uctd.casts <- all.uctd.casts %>%
    filter(!cast %in% exclude.uctd)
  
  # Save cast data to CSV
  write.csv(all.uctd.casts, file = here("Output/cast_data_uctd.csv"), 
            quote = FALSE, row.names = FALSE)
  
  if (nrow(uctd.missing) > 0) {
  # Write table to CSV
  write.csv(uctd.missing, file = here("Output/unprocessed_uctd.csv"), 
            quote = FALSE, row.names = FALSE)
  }
} else {
  cat("No UCTD casts to process.")
}
