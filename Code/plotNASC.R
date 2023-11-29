# Plot NASC for each of the survey vessels. This script assumes that
# process_NASC has already been ran and that the necessary variables exist

# Cycle through each ascoustic vessel
for (i in nasc.vessels) {
  
  # Load the RAW NASC data for that vessel
  nasc.vessel.raw <- readRDS(paste0(here("Data/Backscatter"), "/", i, "/", "nasc_vessel_", i, "_RAW.rds"))
  
  # Get intervals with bad lat/long values
  bad.nasc <- filter(nasc, lat == 999, long == 999)
  write_csv(bad.nasc, here("Output/nasc_bad_cps.csv"))
  
  # Summarize nasc for reporting effort
  nasc.summ <- nasc %>% 
    group_by(transect.name, transect) %>% 
    summarise(
      distance = length(Interval)*100/1852,
      lat = lat[which.min(long)],
      lon = long[which.min(long)])
  
  # average NASC.70 data over new intervals or number of intervals in a 2 km radius
  nasc.summ.cps <- nasc %>%
    filter(lat != 999, long != 999) %>% 
    group_by(transect.name, transect, int) %>%
    summarise(
      bins    = length(int),
      bin.mid = as.integer(round(bins / 2)),
      lat     = lat[1],
      long    = long[1],
      NASC    = mean(cps.nasc)
    )
  
  # Average cps.nasc over defined interval
  # Summarize by filename, not transect, so that renamed (i.e., strip.tx.chars == T) transects get included.
  nasc.sf <- nasc %>%
    filter(lat != 999, long != 999) %>%
    # arrange(filename, datetime) %>% 
    select(vessel.name, filename, transect.name, transect, int, dist_m, datetime, lat, long, cps.nasc) %>% 
    group_by(vessel.name, filename, transect.name, transect, int) %>% 
    summarise(
      lat   = lat[1],
      long  = long[1],
      NASC  = mean(cps.nasc),
      label = paste0('Transect: ', transect[1], "; ",
                     'Distance: ', round(min(dist_m)), "-", round(max(dist_m)), ' m'),
      popup = paste0('<b>Transect: </b>', transect[1], '<br/>',
                     '<b>Time: </b>', min(datetime), " - ", max(datetime), ' UTC<br/>',
                     '<b>Distance: </b>', round(min(dist_m)), "-", round(max(dist_m)), ' m<br/>',
                     '<b>NASC: </b>', round(mean(NASC)), ' m<sup>2</sup> nmi<sup>-2</sup>')) %>%
    # Create bins for defining point size in NASC plots
    mutate(bin       = cut(NASC, nasc.breaks, include.lowest = T),
           bin.level =  as.numeric(bin)) %>% 
    filter(!is.na(bin)) %>% 
    st_as_sf(coords = c("long","lat"), crs = crs.geog) 
  
  nasc.plot.cps <- project_sf(nasc.sf, crs.proj)
  
  # Convert acoustic transects to sf
  nasc.tx.sf <- st_as_sf(nasc.sf, coords = c("long","lat"), crs = crs.geog) %>% 
    select(vessel.name, transect.name, transect) %>% 
    group_by(vessel.name, transect.name, transect) %>% 
    summarise(do_union = F) %>% 
    st_cast("LINESTRING") %>% 
    filter(!transect %in% tx.rm)
  
  # create acoustic transect labels
  nasc.tx.labels.cps <- nasc %>%
    group_by(transect.name, transect) %>%
    summarise(
      lat = lat[which.max(long)],
      long = max(long)
    )
  
  # List already processed CSV files and save
  processed.cps <- unique(nasc$filename)
  save(processed.cps, file =  here("Output/processed_cps.Rdata"))
}
