# A script for processing all UCTD casts, to be used in reports, etc.
# List raw CTD ASCII files
ctd.hdr <- dir_ls(here("Data/CTD"), regexp = ctd.hdr.pattern) %>% 
  path_filter(regexp = "_processed", invert = TRUE)

# Create empty figure if no CTD casts present
if (length(ctd.hdr) > 0) {
  if (process.ctd) {
    # Extract header info
    all.ctd.hdr <- data.frame()
    
    for (i in ctd.hdr) {
      all.ctd.hdr <- bind_rows(all.ctd.hdr,
                               extract_ctd_header(i, type = "CTD"))
    }
    
    # Process CTD cast files --------------------------------------------------
    # List processed CTD cast files
    ctd.proc <- dir_ls(here("Data/CTD"), regexp = ctd.cast.pattern) %>% 
      path_filter(regexp = "_processed")
    
    # Create a data frame for storing results
    all.ctd.casts <- data.frame()
    
    if (length(ctd.proc) > 0) {
      for (i in ctd.proc) {
        all.ctd.casts <- bind_rows(all.ctd.casts, 
                                   extract_ctd_cast(i, type = "CTD"))
      }
    }
    
    # Save results
    save(all.ctd.casts, all.ctd.hdr, ctd.hdr, ctd.proc, 
         file = here("Data/CTD/ctd_data.Rdata"))
    
  } else {
    load(here("Data/CTD/ctd_data.Rdata"))
  }
  
  # Generate ERDDAP URL
  dataURL <- URLencode(paste0(
    "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip",
    erddap.vessel, ".csv0?", erddap.vars,
    "&time>=", 
    date(min(all.ctd.hdr$cast.date) - days(1)), 
    "&time<=", 
    date(max(all.ctd.hdr$cast.date) + days(1))))
  
  # Download and parse ERDDAP nav data
  ctd.nav <- read_csv(dataURL, lazy = FALSE,
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
  
  # Match UCTD headers to nav data
  nav.match.ctd <- data.frame()
  
  for (i in 1:nrow(all.ctd.hdr)) {
    min.diff      <- which.min(abs(difftime(all.ctd.hdr$cast.date[i], ctd.nav$time)))
    nav.match.ctd <- bind_rows(nav.match.ctd, nav[min.diff, ])
  }
  
  # combine header and nav data
  all.ctd.hdr <- all.ctd.hdr %>% 
    bind_cols(nav.match.ctd) %>% 
    mutate(
      cast.num = seq(1, n()),
      lag = difftime(cast.date, time))
  
  if (nrow(all.ctd.casts) > 0) {
    # reorder all.ctd by cast and depth
    all.ctd.casts <- arrange(all.ctd.casts, cast, desc(Z)) %>% 
      # filter all.ctd to remove bad temperature and salinity 
      filter(between(T, min.T, max.T)) %>% 
      filter(between(S, min.S, max.S)) 
    
    # calculate max depth of each cast
    ctd.depth <- all.ctd.casts %>% 
      group_by(cast) %>% 
      summarise(max.depth = min(Z))
    
    # extract cast number from filename
    all.ctd.hdr <- all.ctd.hdr %>% 
      left_join(ctd.depth)
    
    # summarize uctd casts for water classification
    ctd.class <- all.ctd.casts %>% 
      group_by(cast) %>% 
      summarise(
        min.T = min(T),
        min.S = min(S),
        max.T = max(T),
        max.S = max(S)) %>% 
      # assign classes based on salinity
      mutate(class = case_when(
        min.S <= 31.4 ~ "Type 1",
        min.S >= 33.4 ~ "Type 2",
        TRUE ~ "Type 3"))
    
    all.ctd.hdr <- all.ctd.hdr %>% 
      left_join(select(ctd.class, cast, class)) %>% 
      mutate(leg = paste("Leg", cut(as.numeric(date(cast.date)), leg.breaks, labels = FALSE))) %>% 
      select(cast.num, cast, cast.date, lat, long, max.depth, class, leg)
    
    # add water mass to the summary table and cast data for plotting
    all.ctd.casts <- all.ctd.casts %>% 
      left_join(select(ctd.class, cast, class)) %>% 
      left_join(select(all.ctd.hdr, cast, leg))  
    
    # write table to CSV
    write.csv(all.ctd.hdr, file = here("Output/cast_summary_ctd.csv"), 
              quote = FALSE, row.names = FALSE)
    
    # Remove unprocessed casts from the summary
    ctd.missing <- all.ctd.hdr %>% 
      filter(!cast %in% all.ctd.casts$cast)
    
    # Exclude bad casts
    all.ctd.casts <- all.ctd.casts %>%
      filter(!cast %in% exclude.ctd)
    
    # Save cast data to CSV
    write.csv(all.ctd.casts, file = here("Output/cast_data_ctd.csv"), 
              quote = FALSE, row.names = FALSE)
    
  } else {
    ctd.missing <- all.ctd.hdr
  }
  
  if (nrow(ctd.missing) > 0) {
    # write table to CSV
    write.csv(ctd.missing, file = here("Output/unprocessed_ctd.csv"), 
              quote = FALSE, row.names = FALSE)  
  }
  
} else {
  cat("No CTD casts to process.")
}
