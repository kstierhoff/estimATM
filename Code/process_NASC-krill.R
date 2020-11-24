if (process.csv.krill) {
  if (!process.csv.all) {
    # Load already processed CSV files 
    if (file.exists(here("Output/processed_csv_krill.Rdata"))) {
      load(here("Output/processed_csv_krill.Rdata"))
    }
    
    # Load already processed backscatter data
    load(here("Data/Backscatter/nasc_all-krill.Rdata"))
  }
  
  # Process CSV files for each vessel
  for (i in nasc.vessels.krill) {
    # for (i in nasc.vessels) {
    # List all CSV files
    nasc.files.krill <- dir_ls(file.path(here("Data/Backscatter", i)),
                               regex = nasc.pattern.krill[i])
    
    if (!process.csv.all) {
      # List only new CSV files
      nasc.files.krill <- nasc.files.krill[!fs::path_file(nasc.files.krill) %in% processed.csv.krill]
      
      # Load already processed vessel NASC data
      if (file.exists(here("Data/Backscatter", i, paste0("nasc_vessel_krill_", i, "_RAW.rds")))) {
        nasc.vessel.krill <- readRDS(here("Data/Backscatter", i, paste0("nasc_vessel_krill_", i, "_RAW.rds")))
      }
    } 
    
    # Create or load vessel nasc data
    if (length(nasc.files.krill) > 0) {
      # Configure progress bar
      pb <- tkProgressBar("R Progress Bar", "CSV File Processing-Krill", 0, 100, 0)
      
      # Process all .CSV files
      for (ii in seq_along(nasc.files.krill)) {
        # Extract vessel nasc
        nasc.vessel.krill.temp <- extract_csv(nasc.files.krill[ii]) %>% 
          mutate(vessel.name = i,
                 sounder = sounder.type[i],
                 transect = str_replace(transect, nasc.pattern.krill[i],""))
        
        # Combine results
        if (exists("nasc.vessel.krill")) {
          nasc.vessel.krill <- bind_rows(nasc.vessel.krill, nasc.vessel.krill.temp)
        } else {
          nasc.vessel.krill <- nasc.vessel.krill.temp
        }
        
        
        # Update the progress bar
        pb.prog <- round(ii/length(nasc.files.krill)*100)
        info <- sprintf("%d%% done", pb.prog)
        setTkProgressBar(pb, pb.prog, sprintf("CSV processing (Krill) - %s (%s)", i, info), info)
      }
      
      # Close progress bar
      close(pb)
      
      # Save/load nasc.vessel.krill (for debugging)
      saveRDS(nasc.vessel.krill, 
              file = here("Data/Backscatter", i, 
                          paste0("nasc_vessel_krill_", i, "_RAW.rds")))
      
      processed.csv.vessel.krill <- unique(fs::path_file(nasc.vessel.krill$filename))
      
      # Get intervals with bad lat/long values
      bad.nasc <- filter(nasc.vessel.krill, lat == 999, long == 999) %>% 
        arrange(filename, datetime)
      
      if (nrow(bad.nasc) > 0) {
        write_csv(bad.nasc, here("Data/Backscatter", i, 
                                 paste0("nasc_vessel_krill_", i, "_Bad.csv")))  
      }
      
      # Remove bad intervals
      nasc.vessel.krill <- nasc.vessel.krill %>% 
        filter(lat != 999, long != 999)
      
      # Save original transect name before modifying
      # Convert date and time to POSIXct
      nasc.vessel.krill <- nasc.vessel.krill %>% 
        mutate(datetime      = ymd_hms(paste(date, time), tz = "UTC"),
               leg = cut(as.numeric(date(datetime)), leg.breaks, labels = F),
               transect.orig = transect)
      
      if (i == "SD") {
        # Set Saildrone number as vessel name
        nasc.vessel.krill <- nasc.vessel.krill %>% 
          mutate(vessel.orig = as.factor(str_extract(filename, "SD\\d{4}")),
                 id = seq_along(Interval)) %>% 
          filter(fct_match(vessel.orig, paste("SD", sd.numbers, sep = ""))) %>% 
          filter(between(long, min.long, max.long)) %>% 
          filter(between(lat,  min.lat,  max.lat))
      }
      
      # Remove letters from transect names (e.g., 105A, 105B, etc.)
      if (strip.tx.chars[i]) {
        nasc.vessel.krill$transect <- gsub(tx.char.pattern[i], "", nasc.vessel.krill$transect)  
      }
      
      # Remove numbers from transect names (e.g., 105-1, 105-2, etc.)
      if (strip.tx.nums[i]) {
        nasc.vessel.krill$transect <- gsub(tx.num.pattern[i], "", nasc.vessel.krill$transect)
      }
      
      # Remove transit data
      if (rm.transit[i]) {
        # Filter based on transit pattern
        nasc.transit.krill <- nasc.vessel.krill %>% 
          # Remove based on transit pattern
          filter(str_detect(transect.orig, nasc.pattern.transit[i])) %>%
          # Remove inter-transects e.g., 102.5
          filter(!str_detect(transect.orig, "\\d\\.5"))
        
        if (nrow(nasc.transit.krill) > 0) {
          saveRDS(nasc.transit.krill, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_krill_", i, "_transit.rds")))
          
          # Filter transit
          nasc.vessel.krill <- nasc.vessel.krill %>% 
            # Remove based on transit pattern
            filter(str_detect(transect.orig, nasc.pattern.transit[i], negate = TRUE)) %>%
            # Remove inter-transects e.g., 102.5
            filter(str_detect(transect.orig, "\\d\\.5", negate = TRUE)) 
        }
      }
      
      # Remove offshore transects data
      if (rm.offshore[i]) {
        # Filter based on offshore pattern
        nasc.offshore.krill <- nasc.vessel.krill %>% 
          filter(str_detect(transect.orig, nasc.pattern.offshore[i])) 
        
        # Save processed CSV data from each survey vessel
        if (nrow(nasc.offshore.krill) > 0) {
          saveRDS(nasc.offshore.krill, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_krill_", i, "_offshore.rds")))
          
          # Filter offshore transects
          nasc.vessel.krill <- nasc.vessel.krill %>% 
            filter(str_detect(transect.orig, nasc.pattern.offshore[i], negate = TRUE)) 
        }
      }
      
      # Remove offshore transects data
      if (rm.inshore[i]) {
        # Filter based on offshore pattern
        nasc.inshore.krill <- nasc.vessel.krill %>% 
          filter(str_detect(transect.orig, nasc.pattern.inshore[i])) 
        
        # Save processed CSV data from each survey vessel
        if (nrow(nasc.inshore.krill) > 0) {
          saveRDS(nasc.inshore.krill, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_krill_", i, "_inshore.rds")))
          
          # Filter offshore transects
          nasc.vessel.krill <- nasc.vessel.krill %>% 
            filter(str_detect(transect.orig, nasc.pattern.inshore[i], negate = TRUE)) 
        }
      }
      
      # Remove nearshore transects data
      if (rm.nearshore[i]) {
        nasc.nearshore.krill <- nasc.vessel.krill %>% 
          filter(str_detect(transect.orig, nasc.pattern.nearshore[i]) |
                   str_detect(tolower(filename), "nearshore")) 
        
        if (nrow(nasc.nearshore.krill) > 0) {
          # Save processed CSV data from each survey vessel
          saveRDS(nasc.nearshore.krill, 
                  file = here("Data/Backscatter", i, 
                              paste0("nasc_vessel_krill_", i, "_nearshore.rds")))
          
          nasc.vessel.krill <- nasc.vessel.krill %>% 
            filter(str_detect(transect.orig, nasc.pattern.nearshore[i], negate = TRUE)) %>% 
            filter(str_detect(tolower(filename), "nearshore", negate = TRUE))
        }
      }
      
      # Identify acoustic transects to include in processing, based on length
      tx.include <- nasc.vessel.krill %>% 
        group_by(transect) %>% 
        summarise(L = length(Interval)/10*0.539957) %>%
        arrange(L) %>% 
        filter(L > min.tx.length[i]) 
      
      # Remove backscatter data from short transects
      nasc.vessel.krill <- nasc.vessel.krill %>% 
        filter(transect %in% tx.include$transect)
      
      if (nrow(nasc.vessel.krill) > 0) {
        # Create new interval for summarizing and plotting backscatter 
        # and add vessel abbreviation to transect name
        nasc.vessel.krill <- nasc.vessel.krill %>% 
          mutate(int = cut(Interval, seq(1, max(Interval) + nasc.summ.interval,
                                         nasc.summ.interval),
                           labels = F, include.lowest = TRUE),
                 transect.name = paste(vessel.name, transect))
        
        # Filter NASC per vessel -------------------------------------------------
        # Manually remove individual transects
        if (!is.na(tx.rm[i])) {
          nasc.vessel.krill <- filter(nasc.vessel.krill, !transect.orig %in% unlist(tx.rm[i])) 
        }
        
        # Set stop integration depth
        if (source.cps.nasc[i]) {
          # Use externally supplied cps.nasc with variable integration depth (from CTD.app)
          # Read file and create unique key for joining with nasc.vessel.krill
          cps.nasc.temp <- read.csv(data.cps.nasc[i]) %>% 
            mutate(key = paste(lat, long, dist_m),
                   datetime = ymd_hms(paste(date, time))) 
          
          # Join nasc.vessel.krill and cps.nasc on datetime
          nasc.vessel.krill <- nasc.vessel.krill %>% 
            left_join(select(cps.nasc.temp, datetime, cps.nasc))
          
        } else {
          # Use fixed integration depth (NASC.50), with or without surface noise removal
          if (rm.surface[i]) {
            # Remove surface noise, often from Saildrone backscatter
            nasc.vessel.krill <- mutate(nasc.vessel.krill, cps.nasc = NASC.70 - NASC.5)
          } else {
            nasc.vessel.krill <- mutate(nasc.vessel.krill, cps.nasc = NASC.70)
          }
        }
        
        # Save processed CSV data from each survey vessel
        saveRDS(nasc.vessel.krill, file = here("Data/Backscatter", i, 
                                               paste0("nasc_vessel_krill_", i, ".rds")))
        
        # Combine results from different vessels
        if (exists("nasc.krill")) {
          nasc.krill <- bind_rows(nasc.krill, nasc.vessel.krill)
        } else {
          nasc.krill <- nasc.vessel.krill
        }
        
        # Combine processed.csv.vessel
        if (exists("processed.csv.krill")) {
          processed.csv.krill <- unique(sort(c(processed.csv.krill, processed.csv.vessel.krill)))
        } else {
          processed.csv.krill <- processed.csv.vessel.krill
        }
      }
    }  
    
    # Save processed NASC file import
    save(nasc.krill, file = here("Data/Backscatter/nasc_all-krill.Rdata"))
    
    # Save processed CSV file names
    save(processed.csv.krill, file =  here("Output/processed_csv_krill.Rdata"))
  }
} else {
  load(here("Data/Backscatter/nasc_all-krill.Rdata"))
}

# Determine transect order by min latitude/longitude
tx.order <- data.frame()

for (i in nasc.vessels.krill) {
  if (use.tx.number[i]) {
    # Calculate transect order per vessel
    tx.order.temp <- nasc.krill %>%
      filter(str_detect(vessel.name, i)) %>% 
      group_by(transect.name) %>% 
      summarise(max.lat  = max(lat),
                min.long = max(long)) %>% 
      mutate(rank.lat  = rank(max.lat), 
             rank.long = rev(rank(min.long)), 
             diff      = rank.lat - rank.long,
             transect.num = as.numeric(str_extract(transect.name,"[[:digit:]]+")),
             rank.final = rank(transect.num)) %>% 
      arrange(rank.final)
  } else {
    # Calculate transect order per vessel
    tx.order.temp <- nasc.krill %>%
      filter(str_detect(vessel.name, i)) %>% 
      group_by(transect.name) %>% 
      summarise(max.lat  = max(lat),
                min.long = max(long)) %>% 
      mutate(rank.lat  = rank(max.lat), 
             rank.long = rev(rank(min.long)), 
             diff      = rank.lat - rank.long,
             transect.num = as.numeric(str_extract(transect.name,"[[:digit:]]+")),
             rank.final = rank.lat) %>% 
      arrange(rank.final)
  }
  
  # Assign transect order based on rank latitude
  if (nrow(tx.order) == 0) {
    # If the first vessel, create transects from 0 to number of transects
    tx.order.temp$transect <- tx.order.temp$rank.final
  } else {
    # If not the first vessel, add rank latitude to largest existing transect number
    tx.order.temp$transect <- tx.order.temp$rank.final + max(tx.order$transect)
  }
  
  # Combine results from all vessels
  tx.order <- bind_rows(tx.order, tx.order.temp) %>% 
    arrange(transect)
}

# Add transect numbers to NASC
nasc.krill <- left_join(select(nasc.krill, -transect), 
                        select(tx.order, transect.name, transect)) %>% 
  project_df(to = crs.proj)


# Remove Saildrone overlap for 1807RL
if (prj.name %in% c("1807RL")) {
  # Subset Lasker backscatter
  nasc.krill.rl <- filter(nasc.krill, vessel.name == "RL")
  
  # Subset Saildrone backscatter and remove overlap with Lasker
  nasc.sd.sub <- filter(nasc.krill, vessel.name == "SD1024") %>% 
    st_as_sf(coords = c("long","lat"), crs = crs.geog) %>% 
    st_difference(filter(survey.polygons, vessel.name == "RL"))
  
  nasc.krill.sd <- nasc.krill %>% 
    filter(vessel.name == "SD1024") %>% 
    filter(fct_match(as.factor(datetime), as.factor(nasc.sd.sub$datetime)))
  
  nasc.krill <- bind_rows(nasc.krill.rl, nasc.krill.sd)
}

# Summarize nasc data
nasc.krill.summ <- nasc.krill %>% 
  group_by(vessel.name, transect.name, transect) %>% 
  summarise(
    start     = min(datetime),
    end       = max(datetime),
    duration  = difftime(end, start, units = "hours"),
    n_int     = length(Interval),
    distance  = length(Interval)*nasc.interval/1852,
    lat       = lat[which.min(long)],
    long      = long[which.min(long)],
    mean_nasc = mean(NASC.70)) %>% 
  arrange(vessel.name, start)

# Save NASC summary
save(nasc.krill.summ, file = here("Output/nasc_summ_tx_krill.Rdata"))
write_csv(nasc.krill, here("Output/nasc_krill.csv"))

# ggplot(nasc.krill, aes(long, lat, group = transect, colour = factor(transect))) +
#   geom_point(aes(size = NASC.70)) +
#   geom_path(colour = "black") +
#   coord_quickmap() +
#   theme(legend.position = "none")
