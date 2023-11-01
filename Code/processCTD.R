# Batch process (U)CTD data, then use the results to create Echoview-calibration
# files (.ecs) containing the sound speed profile as well as compensating the
# calibration parameters for the change in sound speed at the transducer

# Load required packages --------------------------------------------------

library(readr)    # For reading and writing plain text files
library(stringr)  # For processing strings


# User Settings -----------------------------------------------------------

# Directory of CTD files to process
dir.CTD <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\CTD_to_Process\\'

# Directory to store processed data results
dir.output <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\PROCESSED\\'

# Directory containing SBEDataProcessing Program Setup (.psa) files
# dir.PSA <- paste0(normalizePath(file.path(getwd(), 'CODE/PSA/')),'\\')
dir.PSA <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\PSA_CTD\\'

# CTD configuration file. If left blank (i.e., ''), the script will assume that
# the configuration file has the same name as the CTD input file, which is
# typically the case for CTD (not UCTD) casts.
file.con <- ''#C:\\Users\\josiah.renfree\\Desktop\\TEMP\\CTD_to_Process\\UCTD.con'

# Directory of Seabird SBEDataProcessing programs
dir.SBE <- 'C:\\Program Files (x86)\\Sea-Bird\\SBEDataProcessing-Win32\\'

# Template ECS file
ECS.template <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\ECS\\_2307RL_Template.ecs'

# ECS output directory
dir.ECS <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\ECS\\'

# Time to pause between SBADataProcessing programs, in seconds
pause <- 1

# Define depth of transducer, in meters
txducerDepth <- 6


# Process CTD data --------------------------------------------------------

# Find all raw data files in CTD directory
files.CTD <- list.files(path = dir.CTD, pattern = ".*\\.(hex|asc)")

# Loop through each file
for (i in files.CTD) {
  
  # Retain just the file name (i.e., remove extension)
  file.name <- tools::file_path_sans_ext(i)
  
  # Retain extension
  file.ext <- tools::file_ext(i)
  
  # If file.con is empty, set it to the filename using extension .xmlcon
  if (file.con == '') {
    file.con.ctd <- paste(dir.CTD, file.name, '.XMLCON', sep = '')
  } else {
    file.con.ctd <- file.con
  }
  
  # Convert to .cnv depending on if it's a .hex (CTD) or .asc (UCTD) file
  if (file.ext == "hex") {
    
    # Use DatCnv to convert from .hex to .cnv
    cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                   paste(dir.SBE, 'DatCnvW.exe', sep = ''),
                   paste(file.con.ctd, sep = ''),
                   paste(dir.CTD, file.name, '.hex', sep = ''),
                   dir.output,
                   paste(file.name, '.cnv', sep = ''),
                   paste(dir.PSA, 'DatCnv.psa', sep = ''))
    system("cmd.exe", input = cmd)
    Sys.sleep(pause)
  
  } else {
    
    # Use ASCII In to convert from .asc to .cnv
    cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                   paste(dir.SBE, 'ASCII_InW.exe', sep = ''),
                   paste(dir.CTD, file.name, '.asc', sep = ''),
                   dir.output,
                   paste(file.name, '.cnv', sep = ''),
                   paste(dir.PSA, 'ASCII_In.psa', sep = ''))
    system("cmd.exe", input = cmd)
    Sys.sleep(pause)
  }
  
  # Perform Filter to low-pass filter the data (i.e., remove jitter)
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'FilterW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'Filter.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Align CTD to align the temperature and conductivity sensors. For the
  # Lasker rosette CTD, the alignment is typically done in real-time, however we
  # will still run this program, albeit perhaps with a 0 s time offset.
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'AlignCTDW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'AlignCTD.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Loop Edit to flag bad scans and remove a surface soak. Scans are
  # marked bad if they don't meet some minimum velocity threshold
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'LoopEditW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'LoopEdit.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Derive to obtain depth (from pressure)
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DeriveW.exe', sep = ''),
                 paste(file.con.ctd, sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'DeriveDepth.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Bin Average to average into 1-m depth cells
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'BinAvgW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'BinAvg.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Derive again to obtain salinity, sound speed, average (harmonic)
  # sound speed, and density
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DeriveW.exe', sep = ''),
                 paste(file.con.ctd, sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'Derive.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform ASCII Out to output to a cleanly formatted .asc file
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'ASCII_OutW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '_processed.asc', sep = ''),
                 paste(dir.PSA, 'AsciiOut.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform SeaPlot to generate plot of CTD profile
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'SeaPlotW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.jpg', sep = ''),
                 paste(dir.PSA, 'SeaPlot.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Load results from processed CTD data
  data <- read.csv(paste(dir.output, file.name, '_processed.asc', sep = ''), 
                   header = T, sep = "\t")
  
  # Rename temperature column based on instrument used
  if (file.ext == "hex") {
    colnames(data)[colnames(data) == "T090C"] <- "Temperature"    # If CTD
  } else {
    colnames(data)[colnames(data) == "Tnc90C"] <- "Temperature"   # If UCTD
  }
  
  # Perform basic data error checks. Remove any rows that have a depth,
  # temperature, or salinity that is less than 0
  idx <- data$DepSM < 0 | data$Temperature <= 0 | data$Sal00 < 0
  data[idx,] <- NA
  
  # Obtain sound speed at transducer depth
  soundspeed.txducer <- data$SvCM[which.min(abs(data$DepSM-txducerDepth))]
  

  # Create ECS file ---------------------------------------------------------
  
  # Read template ECS file
  ECS <- read_file(ECS.template)
  
  # Get sound speed from template
  c_0 <- as.numeric(str_match(ECS, "SoundSpeed\\s*=\\s*([^\\s]+)")[,2])
  
  # Get calibration parameters that can be adjusted with sound speed
  g_0 <- as.numeric(str_match_all(ECS, "TransducerGain\\s*=\\s*([^\\s]+)")[[1]][,2])
  EBA_0 <- as.numeric(str_match_all(ECS, "TwoWayBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
  BW_minor_0 <- as.numeric(str_match_all(ECS, "MinorAxis3dbBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
  BW_major_0 <- as.numeric(str_match_all(ECS, "MajorAxis3dbBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
  
  # Get list of Tx variables
  # matches <- str_locate_all(ECS, '.*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)')[[1]]
  
  # Compensate calibration parameters by changes in sound speed
  for (j in 1:length(g_0)){
    
    # Compensate gain
    pattern <- paste("^(?:(?s).*?TransducerGain){", j-1, "}(?s).*?TransducerGain\\s*=\\s*([^\\s]+)", sep = '')
    temp <- regexec(pattern, ECS, perl = TRUE) 
    ECS <- paste0(str_sub(ECS, 1, temp[[1]][2]-1),
                      sprintf(g_0[j] + 20*log10(c_0 / soundspeed.txducer), fmt = '%#.4f'),
                      str_sub(ECS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))

    # Compensate EBA
    pattern <- paste("^(?:(?s).*?TwoWayBeamAngle){", j-1, "}(?s).*?TwoWayBeamAngle\\s*=\\s*(-\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS, perl = TRUE) 
    ECS <- paste0(str_sub(ECS, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(EBA_0[j] + 20*log10(soundspeed.txducer / c_0), fmt = '%#.4f'),
                      str_sub(ECS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))

    # Compensate Alongship (Minor) Beamwidth
    pattern <- paste("^(?:(?s).*?MinorAxis3dbBeamAngle){", j-1, "}(?s).*?MinorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS, perl = TRUE) 
    ECS <- paste0(str_sub(ECS, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(BW_minor_0[j] * (soundspeed.txducer / c_0), fmt = '%#.4f'),
                      str_sub(ECS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))

    # Compensate Athwarthip (Major) Beamwidth
    pattern <- paste("^(?:(?s).*?MajorAxis3dbBeamAngle){", j-1, "}(?s).*?MajorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS, perl = TRUE) 
    ECS <- paste0(str_sub(ECS, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(BW_major_0[j] * (soundspeed.txducer / c_0), fmt = '%#.4f'),
                      str_sub(ECS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
  }
  
  # Insert the sound speed profile
  ECS <- gsub('CtdDepthProfile\\s*=\\s*[^\n]*', 
              paste("CtdDepthProfile =", paste(sprintf("%.2f", data$DepSM), collapse = "; ")),
              ECS)
  ECS <- gsub('SoundSpeedProfile\\s*=\\s*[^\n]*', 
              paste("SoundSpeedProfile =", paste(sprintf("%.2f", data$SvCM), collapse = "; ")),
              ECS)
  
  # Values of temperature, salinity, and sound speed are still used to derive an
  # average value of absorption, so insert values averaged over the entire
  # available range
  
  # Replace the sound speed. We want the average sound speed over the mean
  # depth, in which case we should use the harmonic sound speed at that depth.
  ECS <- gsub('SoundSpeed\\s*=\\s*[^#]*', 
              sprintf('SoundSpeed = %.2f ', 
                      # data$AvgsvCM[which.min(abs(data$DepSM-mean(data$DepSM)))]),
                      tail(data$AvgsvCM, 1)),
              ECS)

  # Replace the temperature
  ECS <- gsub('Temperature\\s*=\\s*[^#]*', 
                  sprintf('Temperature = %.3f ', mean(data$Temperature)),
                  ECS)

  # Replace the salinity
  ECS <- gsub('Salinity\\s*=\\s*[^#]*', 
                  sprintf('Salinity = %.3f ', mean(data$Sal00)),
                  ECS)

  # Replace the average absorption depth
  ECS <- gsub('AbsorptionDepth\\s*=\\s*[^#]*', 
                  sprintf('AbsorptionDepth = %.3f ', mean(data$DepSM)),
                  ECS)

  # Write new ECS files
  write_file(ECS, paste(dir.ECS, file.name, ".ecs", sep = ''))

  # Copy CTD file to the PROCESSED directory
  file.copy(file.path(dir.CTD, i), dir.output)
}
