# Import calibartion directories -----------------------------------------------
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
dirs <- filter(dirs.all, process == 1, ship_name == survey.vessel.long)

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
  cal.files <- c(list.files(as.character(dirs$path[dd]),pattern = ".txt", 
                            full.names = TRUE),
                 list.files(as.character(dirs$path[dd]),pattern = ".xml", 
                            full.names = TRUE))
  
  # set survey info
  survey.vessel.temp   <- dirs$ship_name[dd]
  survey.name.temp     <- dirs$survey_name[dd]
  cal.group.temp       <- dirs$group_name[dd]
  
  # create temp data frames for exporting results
  cal.res.out   <- data.frame()
  cal.pings.out <- data.frame()
  cal.info.out  <- data.frame()
  
  # process all calibration results in CALIBRATION directory
  for (i in cal.files) {
    if (str_detect(i, ".xml")) {
      # Read cal files
      cal <- read_xml(i)
      
      # Extract group data
      Application          <- xtrct_df(cal, "Application")
      EnvironmentData      <- xtrct_df(cal, "EnvironmentData")
      Transducer           <- xtrct_df(cal, "Transducer")
      Transceiver          <- xtrct_df(cal, "Transceiver")
      TransceiverSettings  <- xtrct_df(cal, "TransceiverSetting")
      TargetReference      <- xtrct_df(cal, "TargetReference")
      SingleTargetSettings <- xtrct_df(cal, "SingleTargetDetectorSetting")
      PreviousModelParams  <- xtrct_df(cal, "PreviousModelParameters")
      CalibrationResults   <- xtrct_df(cal, "CalibrationResults")
      Hits                 <- xtrct_df(cal, "HitData")
      
      # Get calibration info ----------------------------------------------------
      cal.ver              <- Application$SoftwareVersion
      cal.date             <- ymd_hms(xtrct_df(cal, "Common")$TimeOfFileCreation)
      comments             <- as.character(NA)
      
      # Extract sounder info ------------------------------------------
      sounder.type         <- Transceiver$SoftwareVersion
      
      # Extract reference target info -------------------------------------------
      target.type          <- TargetReference$Name
      # For the response and frequency, take mid value of 1000 values
      target.response      <- as.numeric(unlist(str_split(TargetReference$Response,";"))) 
      target.frequency     <- as.numeric(unlist(str_split(TargetReference$Frequency,";")))
      target.speed.long    <- TargetReference$LongitudinalSoundSpeed
      target.speed.trans   <- TargetReference$TransversalSoundSpeed
      target.ts            <- target.response[length(target.response)/2]
      target.dev           <- SingleTargetSettings$TsDeviation
      target.mind          <- min(SingleTargetSettings$Range)
      target.maxd          <- max(SingleTargetSettings$Range)
      
      # Extract transducer (txdr) info ------------------------------------------
      txdr.type            <- Transducer$Name
      txdr.sn              <- Transducer$SerialNumber
      txdr.freq            <- CalibrationResults$Frequency/1000
      txdr.beam.type       <- str_replace(TransceiverSettings$BeamType, "BeamType","")
      txdr.gain            <- PreviousModelParams$Gain
      txdr.2way.ba         <- PreviousModelParams$EquivalentBeamAngle
      txdr.athw.ang.sens   <- PreviousModelParams$AngleSensitivityAthwartship
      txdr.alon.ang.sens   <- PreviousModelParams$AngleSensitivityAlongship
      txdr.athw.ba         <- PreviousModelParams$BeamWidthAthwartship
      txdr.alon.ba         <- PreviousModelParams$BeamWidthAlongship
      txdr.athw.oa         <- PreviousModelParams$AngleOffsetAthwartship
      txdr.alon.oa         <- PreviousModelParams$AngleOffsetAlongship
      txdr.sa.corr         <- PreviousModelParams$SaCorrection
      txdr.z               <- Transducer$TransducerDepth
      
      # Extract transceiver (gpt) info ------------------------------------------
      gpt.type             <- str_replace(Transceiver$Type, "TransceiverType","")
      gpt.pd               <- TransceiverSettings$PulseLength * 1000
      gpt.si               <- TransceiverSettings$SampleInterval * 1000
      gpt.power            <- TransceiverSettings$TransmitPower
      gpt.pulse.form       <- TransceiverSettings$PulseForm
      gpt.freq.start       <- TransceiverSettings$FrequencyStart
      gpt.freq.end         <- TransceiverSettings$FrequencyEnd
      gpt.rcr.bw           <- NA
      
      # Extract TS detection info ------------------------------------------
      ts.min.val           <- SingleTargetSettings$MinTSValue
      ts.min.spacing       <- SingleTargetSettings$MinSpacing
      ts.max.beam.comp     <- SingleTargetSettings$MaxGainCompensation
      ts.min.echo.l        <- SingleTargetSettings$MinEchoLength * 100
      ts.max.echo.l        <- SingleTargetSettings$MaxEchoLength * 100
      ts.max.phase.dev     <- SingleTargetSettings$MaxPhaseDeviation
      
      # Extract environment info ------------------------------------------
      env.c                <- EnvironmentData$SoundVelocity
      env.alpha            <- EnvironmentData$AbsorptionCoefficient * 1000
      
      # Extract beam model results ----------------------------------------------
      bm.txdr.gain         <- CalibrationResults$Gain
      bm.sa.corr           <- CalibrationResults$SaCorrection
      bm.athw.ba           <- CalibrationResults$BeamWidthAthwartship
      bm.alon.ba           <- CalibrationResults$BeamWidthAlongship
      bm.athw.oa           <- CalibrationResults$AngleOffsetAthwartship
      bm.alon.oa           <- CalibrationResults$AngleOffsetAlongship 
      
      # Extract data deviation from beam model results --------------------------
      dev.bm.rms           <- CalibrationResults$TsRmsError
      dev.bm.max           <- NA # "Max\\s*=\\s*\\S+\\s+dB"
      dev.bm.max.no        <- NA # "No.\\s*=\\s*\\S+\\s+"
      dev.bm.max.athw      <- NA # "Athw.\\s*=\\s*\\S+\\s+"
      dev.bm.max.alon      <- NA # "Along.\\s*=\\s*\\S+\\s+"
      dev.bm.min           <- NA # "Min\\s*=\\s*\\S+\\s+dB"
      dev.bm.min.no        <- NA # "No.\\s*=\\s*\\S+\\s+"
      dev.bm.min.athw      <- NA # "Athw.\\s*=\\s*\\S+\\s+"
      dev.bm.min.alon      <- NA # "Along.\\s*=\\s*\\S+\\s+"
      
      # Extract data deviation from polynomial model results --------------------
      dev.poly.rms         <- NA # "RMS\\s*=\\s*\\S+\\s+dB"
      dev.poly.max         <- NA # "Max\\s*=\\s*\\S+\\s+dB"
      dev.poly.max.no      <- NA # "No.\\s*=\\s*\\S+\\s+"
      dev.poly.max.athw    <- NA # "Athw.\\s*=\\s*\\S+\\s+"
      dev.poly.max.alon    <- NA # "Along.\\s*=\\s*\\S+\\s+"
      dev.poly.min         <- NA # "Min\\s*=\\s*\\S+\\s+dB"
      dev.poly.min.no      <- NA # "No.\\s*=\\s*\\S+\\s+"
      dev.poly.min.athw    <- NA # "Athw.\\s*=\\s*\\S+\\s+"
      dev.poly.min.alon    <- NA # "Along.\\s*=\\s*\\S+\\s+"
      
      # create a id for merging data tables
      id <- paste(format(cal.date, "%Y%m%d"),"_", 
                  format(Hits$Time[1], format = "%HH%MM%SS"),"_",
                  survey.vessel, "_", txdr.freq, "kHz_", cal.group, sep = "")
      
      # extract ping data from end of file
      # extract all rows below header (do not start with #)
      ping.data <- Hits %>% 
        select(ping_num = Number,
               date_time = Time,
               distance = Range,
               TS_c = TsComp,
               TS_u = TsUncomp,
               athw = Athwart,
               along = Along,
               sA = SaValue,
               outlier = IsSuspended) %>% 
        mutate(txdr_freq = txdr.freq,
               outlier = case_when(
                 outlier == "False" ~ 0,
                 outlier == "True"  ~ 1),
               `id` = id)
      
      # identify pings flagged as outliers (start with asterisk)
      outliers <- ping.data %>% 
        filter(outlier == 1)
      
      # create calibration file name (results from LOBE, SWING, etc.)
      cal.filename <- paste(here("Output/cal/all"),"/", id, ".xml", sep = "")
      
      # write calibration file with new filename
      write_xml(cal, file = cal.filename)
    } else {
      # Read calibration file
      cal <- readLines(i)
      cal.ver     <- str_extract(str_subset(cal, 
                                            pattern = 'Calibration  Version\\s+\\S+\\s*'),
                                 "[0-9].*[0-9]")
      # extract calibration date
      cal.date    <- as.POSIXct(
        str_extract(str_subset(cal, pattern = 'Date:\\s+\\S+'), "[0-9].*[0-9]"), 
        tz = "GMT", format = "%m/%d/%Y")
      if (is.na(cal.date) == T) { #use alternate date format
        cal.date  <- as.POSIXct(str_extract(
          str_subset(cal, pattern = 'Date:\\s+\\S+'),"[0-9].*[0-9]"),tz = "GMT",format = "%Y-%m-%d")
      }
      # extract comments; sub semi colons for commas
      comments           <- gsub(",",";",trim(str_extract(cal[which(cal == "#  Comments:") + 1],"[^#]+")))
      
      # extract reference target info
      target.ts          <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'TS\\s+\\S+\\sdB'))[1],"[-0-9].*[0-9]"))
      target.dev         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'TS Deviation\\s+\\S+\\sdB'))[1],"[-0-9].*[0-9]"))
      target.mind        <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Min. Distance\\s+\\S+\\sm')),"[-0-9].*[0-9]"))
      target.maxd        <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Max. Distance\\s+\\S+\\sm')),"[-0-9].*[0-9]"))
      # extract transducer (txdr) info
      txdr.type          <- str_extract(unlist(
        str_extract_all(cal, pattern = 'Transducer:\\s+\\S+')),"[A-Z]{2}[0-9]{2,3}.*")
      txdr.sn            <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Serial No.\\s+\\S+')),"[0-9]{1,6}"))
      # often, SN is left blank; this replaces with NA for missing values
      if (length(txdr.sn) == 0) txdr.sn <- NA
      txdr.freq          <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Frequency\\s+\\S+\\sHz')),"[0-9]{1,}"))/1000
      txdr.beam.type     <- str_extract(unlist(
        str_extract_all(cal, pattern = 'Beamtype\\s+\\S+')),"(Split)")
      txdr.gain          <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Gain\\s+\\S+\\sdB')),"[0-9]{1,}\\.[0-9]{1,}"))
      txdr.2way.ba       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Two Way Beam Angle\\s+\\S+\\s+dB')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.athw.ang.sens <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw. Angle Sens.\\s+\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.alon.ang.sens <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along. Angle Sens.\\s+\\S+')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.athw.ba       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw. Beam Angle\\s+\\d+.\\d+\\s?deg+')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.alon.ba       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along. Beam Angle\\s+\\d+.\\d+\\s?deg+')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.athw.oa       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw. Offset Angle\\s+[^=]\\S+\\s+deg')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.alon.oa       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along. Offset Angle\\s+\\S+\\s+deg')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.sa.corr       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'SaCorrection\\s+\\S+\\s+dB')),"[-0-9]{1,}\\.[0-9]{1,}"))
      txdr.z             <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Depth\\s+\\S+\\s+m')),"[-0-9]{1,}\\.[0-9]{1,}"))
      # extract transceiver (gpt) info
      gpt.type           <- trim(paste(str_extract_all(unlist(
        str_extract_all(cal, pattern = 'Transceiver:[^#]+')),"\\s+[a-zA-Z0-9_-]+")[[1]], collapse = ""))
      gpt.pd             <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Pulse Duration\\s+\\S+\\s+ms')),"[-0-9]{1,}\\.[0-9]{1,}"))
      gpt.si             <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Sample Interval\\s+\\S+\\s+m')),"[-0-9]{1,}\\.[0-9]{1,}")) # in meters
      gpt.power          <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Power\\s+\\S+\\s+W')),"[-0-9]{1,}"))
      gpt.rcr.bw         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Receiver Bandwidth\\s+\\S+\\s+kHz')),"[-0-9]{1,}\\.[0-9]{1,}"))*1000
      # extract sounder info
      sounder.type       <- trim(str_extract(cal[which(cal == "#  Sounder Type:") + 1],"[^#]+"))
      
      # extract TS detection info
      ts.min.val         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Min. Value\\s+\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      ts.min.spacing     <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Min. Spacing\\s+\\S+\\s+%')),
        "[-0-9]{1,}"))
      # Replace missing spacing data
      ifelse(length(ts.min.spacing) == 0, ts.min.spacing <- NA, ts.min.spacing <- NA)
      
      ts.max.beam.comp   <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Max. Beam Comp.\\s+\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      ts.min.echo.l      <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Min. Echolength\\s+\\S+\\s+%')),
        "[-0-9]{1,}"))
      ts.max.phase.dev   <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Max. Phase Dev.\\s+\\S+')),
        "[0-9]{1,6}"))
      ts.max.echo.l      <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Max. Echolength\\s+\\S+\\s+%')),
        "[-0-9]{1,}"))
      # extract environment info
      env.c              <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Sound Velocity\\s+\\S+\\s+m')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      env.alpha          <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Absorption Coeff.\\s+\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      # extract beam model results
      bm.txdr.gain       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Transducer Gain\\s*=\\s+\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      bm.sa.corr         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'SaCorrection\\s*=\\s+\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      bm.athw.ba         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw. Beam Angle\\s*=\\s?\\S+\\s?deg')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      bm.alon.ba         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along. Beam Angle\\s*=\\s?\\S+\\s?deg')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      bm.athw.oa         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw. Offset Angle\\s*=\\s*\\S+\\s+deg')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      bm.alon.oa         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along. Offset Angle\\s*=\\s*\\S+\\s+deg')),
        "[-0-9]{1,}\\.[0-9]{1,}"))
      # extract data deviation from beam model results
      dev.bm.rms         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'RMS\\s*=\\s*\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[1]
      dev.bm.max         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Max\\s*=\\s*\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[1]
      dev.bm.max.no      <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}"))[1]
      dev.bm.max.athw    <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[1]
      dev.bm.max.alon    <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[1]
      dev.bm.min         <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Min\\s*=\\s*\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[1]
      dev.bm.min.no      <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}"))[2]
      dev.bm.min.athw    <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[2]
      dev.bm.min.alon    <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[2]
      # extract data deviation from polynomial model results
      dev.poly.rms       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'RMS\\s*=\\s*\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[2]
      dev.poly.max       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Max\\s*=\\s*\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[2]
      dev.poly.max.no    <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}"))[3]
      dev.poly.max.athw  <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[3]
      dev.poly.max.alon  <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[3]
      dev.poly.min       <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Min\\s*=\\s*\\S+\\s+dB')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[2]
      dev.poly.min.no    <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'No.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}"))[4]
      dev.poly.min.athw  <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Athw.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[4]
      dev.poly.min.alon  <- as.numeric(str_extract(unlist(
        str_extract_all(cal, pattern = 'Along.\\s*=\\s*\\S+\\s+')),
        "[-0-9]{1,}\\.[0-9]{1,}"))[4]
      
      # extract ping data from end of file
      # extract all rows below header (do not start with #)
      ping.all <- unlist(str_extract_all(cal, pattern = '^[^#]+'))
      # identify pings flagged as outliers (start with asterisk)
      outliers <- grep("[\\*]{1}", ping.all)
      # trim whitespace from strings and write to text file
      ping.strings  <- trim(str_extract(unlist(
        str_extract_all(ping.all, pattern = '^[^#]+')), 
        "[^\\*]+"))
      write.table(ping.strings, file = here("Output/ping_strings.txt"), 
                  quote = F, row.names = F, col.names = F)
      # read text file and add column names
      ping.data <- read.delim(here("Output/ping_strings.txt"),
                              header = F, sep = "", as.is = T, fill = T)
      # remove temporary text file
      file.remove(here("Output/ping_strings.txt"))
      names(ping.data) <- c("ping_num","time","distance","TS_c",
                            "TS_u","athw","along","sA")
      # paste date and time and convert time to POSIXct
      ping.data$time <- as.POSIXct(paste(cal.date, ping.data$time, sep = " "), 
                                   tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
      colnames(ping.data)[2] <- "date_time"
      # create outliers vector and set outliers to T [1] for those with asterisks
      ping.data$outlier <- rep(0,dim(ping.data)[1])
      ping.data$outlier[outliers] <- 1
      ping.data$txdr_freq <- (txdr.freq)
      
      # create a id for merging data tables
      id <- paste(format(cal.date, "%Y%m%d"),"_", 
                  format(ping.data$date_time[1], format = "%HH%MM%SS"),"_",
                  survey.vessel, "_", txdr.freq, "kHz_", cal.group, sep = "")
      ping.data$id <- as.factor(id)
      
      # create calibration file name (results from LOBE, SWING, etc.)
      cal.filename <- paste(here("Output/cal/all"),"/", id, ".txt", sep = "")
      
      # write calibration file with new filename
      write.table(cal, file = cal.filename, row.names = F, col.names = F, quote = F)
    }
    
    # create a data frame for calibration results
    # create a cal_results.txt table, to hold summary data about transducer calibrations from cal/LOBE files
    cal.res <- data.frame(id,cal.ver,cal.date,comments,target.ts,target.dev,target.mind,target.maxd,
                          txdr.type,txdr.sn,txdr.freq,txdr.beam.type,txdr.gain,txdr.2way.ba,
                          txdr.athw.ang.sens,txdr.alon.ang.sens,
                          txdr.athw.ba,txdr.alon.ba,txdr.athw.oa,txdr.alon.oa,txdr.sa.corr,txdr.z,
                          gpt.type,gpt.pd,gpt.si,gpt.power,gpt.rcr.bw,sounder.type,
                          ts.min.val,ts.min.spacing,ts.max.beam.comp,ts.min.echo.l,ts.max.phase.dev,ts.max.echo.l,
                          env.c,env.alpha,
                          bm.txdr.gain,bm.sa.corr,bm.athw.ba,bm.alon.ba,bm.athw.oa,bm.alon.oa,
                          dev.bm.rms,dev.bm.max,dev.bm.max.no,dev.bm.max.athw,dev.bm.max.alon,dev.bm.min,dev.bm.min.no,
                          dev.bm.min.athw,dev.bm.min.alon,
                          dev.poly.rms,dev.poly.max,dev.poly.max.no,dev.poly.max.athw,dev.poly.max.alon,
                          dev.poly.min,dev.poly.min.no,dev.poly.min.athw,dev.poly.min.alon)
    
    names(cal.res) <- c("id","cal_ver","cal_date","comments","target_ts","target_dev","target_mind","target_maxd",
                        "txdr_type","txdr_sn","txdr_freq","txdr_beam_type","txdr_gain","txdr_2way_ba",
                        "txdr_athw_ang_sens","txdr_alon_ang_sens",
                        "txdr_athw_ba","txdr_alon_ba","txdr_athw_oa","txdr_alon_oa","txdr_sa_corr","txdr_z",
                        "gpt_type","gpt_pd","gpt_si","gpt_power","gpt_rcr_bw","sounder_type",
                        "ts_min_val","ts_min_spacing","ts_max_beam_comp","ts_min_echo_l",
                        "ts_max_phase_dev","ts_max_echo_l", "env_c","env_alpha",
                        "bm_txdr_gain","bm_sa_corr","bm_athw_ba","bm_alon_ba","bm_athw_oa","bm_alon_oa",
                        "dev_bm_rms","dev_bm_max","dev_bm_max_no","dev_bm_max_athw","dev_bm_max_alon",
                        "dev_bm_min","dev_bm_min_no","dev_bm_min_athw","dev_bm_min_alon","dev_poly_rms",
                        "dev_poly_max","dev_poly_max_no","dev_poly_max_athw","dev_poly_max_alon",
                        "dev_poly_min","dev_poly_min_no","dev_poly_min_athw","dev_poly_min_alon")
    
    # reformat cal.res$cal.date
    cal.res <- cal.res %>% 
      mutate(cal_date = format(cal_date, format = "%m/%d/%Y"))
    
    # Create a cal_info.txt table, to hold info about calibration files, 
    # including vessel, date, freq, file name,file path, etc.
    cal.info <- data.frame(id, vessel_name = survey.vessel.temp, 
                           survey_name = survey.name.temp, 
                           group_name = cal.group.temp, 
                           date_time = format(ping.data$date_time[1], format = "%m/%d/%Y %H:%M:%S"),
                           freq = txdr.freq, 
                           filepath = "\\\\swc-storage1.nmfs.local\\AST1\\CALIBRATIONS", 
                           filename = cal.filename)
    
    # append cal results
    cal.res.out   <- bind_rows(cal.res.out, cal.res) %>% arrange(txdr_freq)
    # append cal pings
    cal.pings.out <- bind_rows(cal.pings.out, ping.data)
    # append cal info
    cal.info.out  <- bind_rows(cal.info.out, cal.info) %>% arrange(freq)
  }
  
  # write processing results to file
  write.csv(cal.res.out,  file = paste(here("Output/cal/all"),"/",format(cal.date,"%Y%m%d"),
                                       "_",survey.vessel.temp,"_",survey.name.temp,"_",
                                       cal.group.temp,".res",sep = ""),row.names = F,quote = F)
  write.csv(cal.info.out, file = paste(here("Output/cal/all"),"/",format(cal.date,"%Y%m%d"),
                                       "_",survey.vessel.temp,"_",survey.name.temp,"_",
                                       cal.group.temp,".info",sep = ""),row.names = F,quote = F)
  write.csv(cal.pings.out,file = paste(here("Output/cal/all"),"/",format(cal.date,"%Y%m%d"),
                                       "_",survey.vessel.temp,"_",survey.name.temp,"_",
                                       cal.group.temp,".ping",sep = ""),row.names = F,quote = F)
  
  # combine all results
  cal.res.all   <- rbind(cal.res.all, cal.res.out)
  cal.info.all  <- rbind(cal.info.all, cal.info.out)
  cal.pings.all <- rbind(cal.pings.all, cal.pings.out)

  info <- sprintf("%d%% done", round((dd/nrow(dirs))*100))
  setWinProgressBar(pb, dd/nrow(dirs)*100, label = info)
}
close(pb)

# Format results
cal.res.all   <- arrange(cal.res.all, cal_date, txdr_freq)
cal.info.all  <- arrange(cal.info.all, date_time)
cal.pings.all <- arrange(cal.pings.all, date_time)

# write all results to file
save(cal.files, cal.res.all, cal.info.all, cal.pings.all, 
     file = here("Data/cal/cal_results_all.Rdata"))
