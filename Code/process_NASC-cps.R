if (process.csv) {
  if (!process.csv.all) {
    # Load already processed CSV files 
    if (file.exists(here("Output/processed_csv-cps.Rdata"))) {
      load(here("Output/processed_csv-cps.Rdata"))
    }
    
    # Load already processed backscatter data
    if (file.exists(here("Data/Backscatter/nasc_all-cps.Rdata"))) {
      load(here("Data/Backscatter/nasc_all-cps.Rdata"))
    }
  }
  
  # Process CSV files for each vessel
  for (i in nasc.vessels) {
    # List all CSV files
    nasc.files <- dir_ls(file.path(here("Data/Backscatter", i)),
                         regexp = nasc.pattern.cps[i],
                         ignore.case = TRUE)
    
    if (!process.csv.all) {
      # List only new CSV files
      nasc.files <- nasc.files[!fs::path_file(nasc.files) %in% processed.cps]
      
      # Load already processed vessel NASC data
      if (file.exists(here("Data/Backscatter", i, paste0("nasc_vessel_", i, "_RAW.rds")))) {
        nasc.vessel <- readRDS(here("Data/Backscatter", i, paste0("nasc_vessel_", i, "_RAW.rds")))
      }
    } else {
      # Remove existing nasc.vessel
      if (exists("nasc.vessel")) rm(nasc.vessel)
    }
    
    # Create or load vessel nasc data
    if (length(nasc.files) > 0) {
      # Configure progress bar
      pb <- tkProgressBar("R Progress Bar", "CSV File Processing", 0, 100, 0)
      
      # Process all .CSV files
      for (ii in seq_along(nasc.files)) {
        # Extract vessel nasc
        nasc.vessel.temp <- extract_csv(nasc.files[ii]) %>% 
          mutate(vessel.name = i,
                 sounder = sounder.type[i],
                 transect = str_replace(transect, nasc.pattern.cps[i],""))
        
        # Combine results
        if (exists("nasc.vessel")) {
          nasc.vessel <- bind_rows(nasc.vessel, nasc.vessel.temp)
        } else {
          nasc.vessel <- nasc.vessel.temp
        }
        
        # Update the progress bar
        pb.prog <- round(ii/length(nasc.files)*100)
        info <- sprintf("%d%% done", pb.prog)
        setTkProgressBar(pb, pb.prog, sprintf("CSV processing - %s (%s)", i, info), info)
      }
      
      # Close progress bar
      close(pb)
      
      # Save/load nasc.vessel (for debugging)
      saveRDS(nasc.vessel, 
              file = here("Data/Backscatter", i, 
                          paste0("nasc_vessel_", i, "_RAW.rds")))
      
      processed.cps.vessel <- unique(fs::path_file(nasc.vessel$filename))
      
      # Get intervals with bad lat/long values
      bad.nasc <- filter(nasc.vessel, lat == 999, long == 999) %>% 
        arrange(filename, datetime)
      
      if (nrow(bad.nasc) > 0) {
        write_csv(bad.nasc, here("Data/Backscatter", i, 
                                 paste0("nasc_vessel_", i, "_Bad.csv")))  
      }
      
      # Remove bad intervals
      nasc.vessel <- nasc.vessel %>% 
        filter(lat != 999, long != 999)
      
      # Save original transect name before modifying
      # Convert date and time to POSIXct
      nasc.vessel <- nasc.vessel %>% 
        mutate(datetime      = ymd_hms(paste(date, time), tz = "UTC"),
               leg = cut(as.numeric(date(datetime)), leg.breaks, labels = FALSE),
               transect.orig = transect)
      
      if (i == "SD") {
        # Set Saildrone number as vessel name
        nasc.vessel <- nasc.vessel %>% 
          mutate(vessel.orig = as.factor(str_extract(filename, "SD\\d{4}")),
                 id = seq_along(Interval)) %>% 
          filter(fct_match(vessel.orig, paste("SD", sd.numbers, sep = ""))) %>% 
          filter(between(long, min.long, max.long)) %>% 
          filter(between(lat,  min.lat,  max.lat))
      }
      
      # Remove letters from transect names (e.g., 105A, 105B, etc.)
      if (strip.tx.chars[i]) {
        nasc.vessel$transect <- gsub(tx.char.pattern[i], "", nasc.vessel$transect)  
      }
      
      # Remove numbers from transect names (e.g., 105-1, 105-2, etc.)
      if (strip.tx.nums[i]) {
        nasc.vessel$transect <- gsub(tx.num.pattern[i], "", nasc.vessel$transect)
      }
      
      # Remove transit data
      if (rm.transit[i]) {
        # Filter based on transit pattern
        nasc.transit <- nasc.vessel %>% 
          # Remove based on transit pattern
          filter(str_detect(transect.orig, nasc.pattern.transit[i])) %>%
          # Remove inter-transects e.g., 102.5
          filter(!str_detect(transect.orig, "\\d\\.5"))
        
        if (nrow(nasc.transit) > 0) {
          saveRDS(nasc.transit, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_transit.rds")))
          
          # Filter transit
          nasc.vessel <- nasc.vessel %>% 
            # Remove based on transit pattern
            filter(str_detect(transect.orig, nasc.pattern.transit[i], negate = TRUE)) %>%
            # Remove inter-transects e.g., 102.5
            filter(str_detect(transect.orig, "\\d\\.5", negate = TRUE)) 
        }
      }
      
      # Remove offshore transects data
      if (rm.offshore[i]) {
        # Filter based on offshore pattern
        nasc.offshore <- nasc.vessel %>% 
          filter(str_detect(transect.orig, nasc.pattern.offshore[i])) 
        
        # Save processed CSV data from each survey vessel
        if (nrow(nasc.offshore) > 0) {
          saveRDS(nasc.offshore, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_offshore.rds")))
          
          # Filter offshore transects
          nasc.vessel <- nasc.vessel %>% 
            filter(str_detect(transect.orig, nasc.pattern.offshore[i], negate = TRUE)) 
        }
      }
      
      # Remove offshore transects data
      if (rm.inshore[i]) {
        # Filter based on offshore pattern
        nasc.inshore <- nasc.vessel %>% 
          filter(str_detect(transect.orig, nasc.pattern.inshore[i])) 
        
        # Save processed CSV data from each survey vessel
        if (nrow(nasc.inshore) > 0) {
          saveRDS(nasc.inshore, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_inshore.rds")))
          
          # Filter offshore transects
          nasc.vessel <- nasc.vessel %>% 
            filter(str_detect(transect.orig, nasc.pattern.inshore[i], negate = TRUE)) 
        }
      }
      
      # Remove nearshore transects data
      if (rm.nearshore[i]) {
        nasc.nearshore <- nasc.vessel %>% 
          filter(str_detect(transect.orig, nasc.pattern.nearshore[i]) |
                   str_detect(tolower(filename), "nearshore")) 
        
        if (nrow(nasc.nearshore) > 0) {
          # Save processed CSV data from each survey vessel
          saveRDS(nasc.nearshore, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_", i, "_nearshore.rds")))
          
          nasc.vessel <- nasc.vessel %>% 
            filter(str_detect(transect.orig, nasc.pattern.nearshore[i], negate = TRUE)) %>% 
            filter(str_detect(tolower(filename), "nearshore", negate = TRUE))
        }
      }
      
      # Identify acoustic transects to include in processing, based on length
      tx.include <- nasc.vessel %>% 
        group_by(transect) %>% 
        summarise(L = length(Interval)/10*0.539957) %>%
        arrange(L) %>% 
        filter(L > min.tx.length[i]) 
      
      # Remove backscatter data from short transects
      nasc.vessel <- nasc.vessel %>% 
        filter(transect %in% tx.include$transect)
      
      if (nrow(nasc.vessel) > 0) {
        # Create new interval for summarizing and plotting backscatter 
        # and add vessel abbreviation to transect name
        nasc.vessel <- nasc.vessel %>% 
          mutate(int = cut(Interval, seq(1, max(Interval) + nasc.summ.interval,
                                         nasc.summ.interval),
                           labels = FALSE, include.lowest = TRUE),
                 transect.name = paste(vessel.name, transect))
        
        # Filter NASC per vessel -------------------------------------------------
        # Manually remove individual transects
        if (!is.na(tx.rm[i])) {
          nasc.vessel <- filter(nasc.vessel, !transect.orig %in% unlist(tx.rm[i])) 
        }
        
        # Set stop integration depth
        if (source.cps.nasc[i]) {
          # Use externally supplied cps.nasc with variable integration depth (from CTD.app)
          # Read file and create unique key for joining with nasc.vessel
          if (survey.name %in% c("1507SH","1606RL")) {
            cps.nasc.temp <- read.csv(data.cps.nasc[i]) %>% 
              mutate(key = paste(transect, interval))
            # mutate(key = paste(interval, round(dist_m, 6))) 
            
            if (survey.name == c("1507SH")) {
              # Join nasc.vessel and cps.nasc on transect number and interval
              nasc.vessel <- nasc.vessel %>% 
                mutate(time.align  = align.time(datetime, 1),
                       transect.num = as.numeric(substr(transect.name, 4, 5)),
                       key = paste(transect.num, Interval)) %>% 
                filter(!str_detect(tolower(filename), "krill")) %>% 
                left_join(select(cps.nasc.temp, key, cps.nasc)) %>% 
                mutate(cps.nasc.diff = NASC.50 - cps.nasc) %>% 
                mutate(cps.nasc = case_when(
                  is.na(cps.nasc) ~ NASC.50,
                  TRUE ~ cps.nasc))  
            } else if (survey.name == c("1606RL")) {
              # Join nasc.vessel and cps.nasc on transect number and interval
              nasc.vessel <- nasc.vessel %>% 
                mutate(time.align  = align.time(datetime, 1),
                       transect.num = as.numeric(substr(transect.name, 4, 6)),
                       key = paste(transect.num, Interval)) %>% 
                filter(!str_detect(tolower(filename), "krill")) %>% 
                left_join(select(cps.nasc.temp, key, cps.nasc)) %>% 
                mutate(cps.nasc.diff = NASC.50 - cps.nasc) %>% 
                mutate(cps.nasc = case_when(
                  is.na(cps.nasc) ~ NASC.50,
                  TRUE ~ cps.nasc))
            }
          } else {
            cps.nasc.temp <- read.csv(data.cps.nasc[i]) %>% 
              mutate(key = paste(lat, long, dist_m),
                     datetime      = ymd_hms(paste(date, time), tz = "UTC"),
                     time.align  = align.time(datetime, 1)) %>% 
              # Remove data from krill files (1807RL)
              filter(!str_detect(tolower(filename), "krill")) 
            
            # Join nasc.vessel and cps.nasc on datetime
            nasc.vessel <- nasc.vessel %>% 
              mutate(time.align  = align.time(datetime, 1)) %>% 
              # select(-cps.nasc) %>% 
              filter(!str_detect(tolower(filename), "krill")) %>% 
              left_join(select(cps.nasc.temp, time.align, cps.nasc))  
          }
        } else {
          # Use cps.NASC extracted using extract_cps_nasc.R
          if ("cps.NASC" %in% colnames(nasc.vessel)) {
            nasc.vessel$cps.nasc <- nasc.vessel$cps.NASC
          } else {
            # If cps.NASC not extracted, use fixed depth (nasc.depth.cps) defined in settings
            nasc.vessel$cps.nasc <- purrr::pluck(nasc.vessel, nasc.depth.cps)
          }
          
          # Remove surface noise, often from Saildrone backscatter
          if (rm.surface[i]) {
            nasc.vessel <- mutate(nasc.vessel, cps.nasc = cps.nasc - NASC.5)
          }
        }
        
        # Save processed CSV data from each survey vessel
        saveRDS(nasc.vessel, file = here("Data/Backscatter", i, 
                                         paste0("nasc_vessel_", i, ".rds")))
        
        # Combine results from different vessels
        if (exists("nasc.cps")) {
          nasc.cps <- bind_rows(nasc.csv, nasc.vessel)
        } else {
          nasc.cps <- nasc.vessel
        }
      }
      
      # Combine processed.cps.vessel
      if (exists("processed.cps")) {
        processed.cps <- unique(sort(c(processed.cps, processed.cps.vessel)))
      } else {
        processed.cps <- processed.cps.vessel
      }
    }
  }  
  
  # Save processed NASC file import
  save(nasc.cps, file = here("Data/Backscatter/nasc_all-cps.Rdata"))
  
  # Save processed CSV file names
  save(processed.cps, file =  here("Output/processed_csv.Rdata"))
  
} else {
  load(here("Data/Backscatter/nasc_all-cps.Rdata"))
}
