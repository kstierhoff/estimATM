# Process and format trawl data.

# Formats raw data from database for use in other analyses, depending on database source
# Trawl data are extracted using Code/collect_trawl_database.R
# Intended to be run following collect_trawl_database.R in scripts that also load settings from Doc/settings

if (trawl.source == "Access") {
  # Reformat haul data to match SQL
  haul.all <- haul.all %>% 
    arrange(haul) %>% 
    mutate(
      startLatDecimal  =   startLatitudeDegrees + (startLatitudeMinutes/60),
      startLongDecimal = -(startLongitudeDegrees + (startLongitudeMinutes/60)),
      stopLatDecimal   =   stopLatitudeDegrees + (stopLatitudeMinutes/60),
      stopLongDecimal  = -(stopLongitudeDegrees + (stopLongitudeMinutes/60))) %>%
    mutate(haulBackTime = case_when(
      haulBackTime < equilibriumTime ~ haulBackTime + days(1),
      TRUE ~ haulBackTime)) %>% 
    rename(duration = Duration, notes = Notes) %>% 
    mutate(deploymentTime = difftime(equilibriumTime, netInWaterTime, units = "mins"),
           recoveryTime   = difftime(netOnDeckTime, haulBackTime, units = "mins"),
           evolutionTime  = difftime(netOnDeckTime, netInWaterTime, units = "mins"))
  
  # Identify hauls where date of equilibriumTime or haulBackTime is incorrect
  eq.fix <- which(c(0, diff(haul.all$equilibriumTime)) < 0)
  hb.fix <- which(c(0, diff(haul.all$haulBackTime)) < 0)
  
  # Correct equilibriumTime or haulBackTime
  haul.all$equilibriumTime[eq.fix] <- haul.all$equilibriumTime[eq.fix] + days(1)
  haul.all$haulBackTime[eq.fix]    <- haul.all$haulBackTime[eq.fix] + days(1)
  
  # Reformat length frequency data to match SQL
  if (exists("lengthFreq.all")) {
    lengthFreq.all <- lengthFreq.all %>% 
      rename(length = Length, lengthType = LengthType, 
             sexUnknown = NotDetermined, male = Male, activeFemale = ActiveFemale, 
             inactiveFemale = InactiveFemale, totalFemale = TotalFemale, 
             subSampleNumber = SubSampleNumber)
  }
  
} else if (trawl.source == "SQL") {
  haul.all <- haul.all %>% 
    arrange(haul) %>% 
    mutate(
      equilibriumTime = ymd_hms(equilibriumTime),
      haulBackTime    = ymd_hms(haulBackTime)) %>% 
    mutate(deploymentTime = difftime(equilibriumTime, netInWaterTime, units = "mins"),
           recoveryTime   = difftime(netOnDeckTime, haulBackTime, units = "mins"),
           evolutionTime  = difftime(netOnDeckTime, netInWaterTime, units = "mins"))
  
} else if (trawl.source == "Excel") {
  # Format haul data
  haul.all <- haul.all %>% 
    mutate(
      startLatDecimal  =   DecLatitude,
      startLongDecimal =   DecLongitude,
      stopLatDecimal   =   DecLatitude,
      stopLongDecimal  =   DecLongitude,
      equilibriumTime  =   mdy_hms(paste(as.character(trawlDate),
                                         format(haul.all$EquilibriumTime, 
                                                format = "%H:%M:%S"))),
      haulBackTime     =   equilibriumTime + minutes(`Duration(dec)`*60))
  
  # Filter haul data for current survey
  haul.all <- haul.all %>% 
    select(cruise = Cruise,ship = Ship,haul = Haul,collection = Collection,
           startLatDecimal,startLongDecimal,stopLatDecimal,
           stopLongDecimal,equilibriumTime,haulBackTime) 
  
}

# Classify hauls by season (spring or summer)
haul.all <- haul.all %>% 
  mutate(season = case_when(
    month(equilibriumTime) < 6 ~ "spring",
    TRUE ~ "summer"))

# Compute totalWeight and totalNum, which don't reliably exist in either database
catch.all <- catch.all %>% 
  mutate(
    totalWeight = subSampleWtkg + remainingSubSampleWtkg,
    totalNum    = (subSampleCount/subSampleWtkg)*totalWeight)
