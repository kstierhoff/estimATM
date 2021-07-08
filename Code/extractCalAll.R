# Create output directory ------------------------------------------------------
dir_create(here("Data/Calibration"))
dir_create(here("Output/Calibration/All"))

# Import calibration directories -----------------------------------------------
# Configure ODBC connection
cal.con  <- dbConnect(odbc(), 
                      Driver = "SQL Server", 
                      Server = "161.55.235.187", 
                      Database = "AST", 
                      Trusted_Connection = "True")
# Collect table
dirs.all <- tbl(cal.con,"tbl_CAL_DIRS") %>% collect()

# Close connection
dbDisconnect(cal.con)

# Select only directories from current survey vessel
dirs <- filter(dirs.all, process == 1, ship_name == survey.vessel.long) %>% 
  arrange(path)

# Create data frames for storing results
cal.res.all   <- data.frame()
cal.info.all  <- data.frame()
cal.pings.all <- data.frame()

# Configure the progress bar
pb <- winProgressBar(title = "Calibration File Processing Progress", 
                     label = "0% done", min = 0, max = 100, initial = 0)

# pb1 <- txtProgressBar(min = 0, max = dim(dirs)[1], style = 3)
for (dd in 1:nrow(dirs)) {
  # get calibration file directories
  cal.files <- sort(c(list.files(as.character(dirs$path[dd]),pattern = ".txt", 
                                 full.names = TRUE),
                      list.files(as.character(dirs$path[dd]),pattern = ".xml", 
                                 full.names = TRUE)))
  
  # set survey info
  survey.vessel.temp   <- dirs$ship_name[dd]
  survey.name.temp     <- dirs$survey_name[dd]
  cal.group.temp       <- dirs$group_name[dd]
  
  # process all calibration results in CALIBRATION directory
  for (i in cal.files) {
    # Extract calibration results, information, and pings
    cal <- extract_cal(i, survey.vessel.temp, survey.name.temp, cal.group.temp)
    
    # combine all results
    cal.res.all   <- bind_rows(cal.res.all,   cal$cal.res)
    cal.info.all  <- bind_rows(cal.info.all,  cal$cal.info)
    cal.pings.all <- bind_rows(cal.pings.all, cal$cal.pings)
  }
  
  info <- sprintf("%d%% done", round((dd/nrow(dirs))*100))
  setWinProgressBar(pb, dd/nrow(dirs)*100, label = info)
}
close(pb)

# Format results
cal.res.all   <- arrange(cal.res.all, mdy(cal_date), txdr_freq)
cal.info.all  <- arrange(cal.info.all, mdy_hms(date_time))
cal.pings.all <- arrange(cal.pings.all, date_time)

# write all results to file
save(cal.res.all, cal.info.all, cal.pings.all, 
     file = here("Data/Calibration/cal_results_all.Rdata"))
