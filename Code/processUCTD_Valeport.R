# Batch process uCTD data files from the Valeport probe, then create an Echoview
# .ecs file that contains a sound speed profile

# Load required packages ---------------
#-----------------------------------

library(readr)    # For reading and writing plain text files
library(stringr)  # For processing strings

# User Settings -----------------------------------------------------------

# Directory of CTD files to process
dir.CTD <- 'C:\\SURVEY\\2407RL\\DATA\\UCTD\\Valeport\\'

# Template ECS file
ECS.template <- 'C:\\SURVEY\\2307RL\\PROCESSED\\EV\\ECS\\_2307RL_Template.ecs'

# ECS output directory
dir.ECS <- 'C:\\SURVEY\\2307RL\\PROCESSED\\EV\\ECS\\'

# Minimum depth for determining start of downcast (in meters)
minDepth <- 2


# Process CTD data --------------------------------------------------------

# Find all raw data files in CTD directory
files.CTD <- list.files(path = dir.CTD, pattern = "*.vp2")

# Loop through each file
for (i in files.CTD) {
  

  # Process CTD data --------------------------------------------------------

  # Retain just the file name (i.e., remove extension)
  file.name <- tools::file_path_sans_ext(i)
  
  # Read data file
  lines <- readLines(paste(dir.CTD, i, sep = ''))
  
  # Find where data starts
  table_start <- grep("\\[DATA\\]", lines)
  
  # Parse out the data
  data <- read.table(text = lines[c(table_start+1,(table_start+3):length(lines))],
                     header = TRUE,
                     sep = "\t",
                     stringsAsFactors = FALSE)

  # Retain only data from the downcast, i.e., when it drops below X meters to
  # when it is deepest
  idx_start <- which(data$Depth > minDepth)[1]   # First sample where depth > 2 m
  idx_stop <- which.max(data$Depth)       # Sample of maximum depth
  
  data <- data[idx_start:idx_stop,]
  
  
  
  

  # Perform basic data error checks
  idx <- data$DepSM < 0 | data$Tnc90C <= 0 | data$Sal00 < 0
  data[idx,] <- NA
  
  # For CPS, take the average sound velocity at 70 m then calculate the average
  # temperature, salinity, and depth
  idx <- data$DepSM <= 70
  avgSoundSpeed.CPS <- tail(data$AvgsvCM[idx], n = 1)
  avgTemperature.CPS <- mean(data$Tnc90C[idx], na.rm = T)
  avgSalinity.CPS <- mean(data$Sal00[idx], na.rm = T)
  avgDepth.CPS <- mean(data$DepSM[idx], na.rm = T)
  
  # For krill, take the average sound velocity at 350 m then calculate the
  # average temperature, salinity, and depth
  idx <- data$DepSM <= 350
  avgSoundSpeed.Krill <- tail(data$AvgsvCM[idx], n = 1)
  avgTemperature.Krill <- mean(data$Tnc90C[idx], na.rm = T)
  avgSalinity.Krill <- mean(data$Sal00[idx], na.rm = T)
  avgDepth.Krill <- mean(data$DepSM[idx], na.rm = T)
  
  
  
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
  
  # Create CPS and Krill-specific ECS files
  ECS.CPS <- ECS
  ECS.Krill <- ECS
  
  # For both CPS and Krill, replace the calibration parameters by compensating
  # for changes in sound speed
  for (j in 1:length(g_0)){
    
    # Compensate gain
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(g_0[j] + 20*log10(c_0 / avgSoundSpeed.CPS), fmt = '%#.4f'),
                  str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(g_0[j] + 20*log10(c_0 / avgSoundSpeed.Krill), fmt = '%#.4f'),
                      str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    
    # Compensate EBA
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?TwoWayBeamAngle\\s*=\\s*(-\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(EBA_0[j] + 20*log10(avgSoundSpeed.CPS / c_0), fmt = '%#.4f'),
                  str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(EBA_0[j] + 20*log10(avgSoundSpeed.Krill / c_0), fmt = '%#.4f'),
                  str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    
    # Compensate Alongship (Minor) Beamwidth
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?MinorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(BW_minor_0[j] * (avgSoundSpeed.CPS / c_0), fmt = '%#.4f'),
                  str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(BW_minor_0[j] * (avgSoundSpeed.Krill / c_0), fmt = '%#.4f'),
                      str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    
    # Compensate Athwarthip (Major) Beamwidth
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?MajorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.CPS, perl = TRUE)       # Find match
    ECS.CPS <- paste0(str_sub(ECS.CPS, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(BW_major_0[j] * (avgSoundSpeed.CPS / c_0), fmt = '%#.4f'),
                  str_sub(ECS.CPS, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
    temp <- regexec(pattern, ECS.Krill, perl = TRUE)       # Find match
    ECS.Krill <- paste0(str_sub(ECS.Krill, 1, temp[[1]][2]-1),   # Insert new value
                      sprintf(BW_major_0[j] * (avgSoundSpeed.Krill / c_0), fmt = '%#.4f'),
                      str_sub(ECS.Krill, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
  }

  # Replace the sound speed
  ECS.CPS <- gsub('SoundSpeed\\s*=\\s*[^#]*', 
                  sprintf('SoundSpeed = %.2f ', avgSoundSpeed.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('SoundSpeed\\s*=\\s*[^#]*', 
                    sprintf('SoundSpeed = %.2f ', avgSoundSpeed.Krill),
                    ECS.Krill)
  
  # Replace the temperature
  ECS.CPS <- gsub('Temperature\\s*=\\s*[^#]*', 
                  sprintf('Temperature = %.3f ', avgTemperature.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('Temperature\\s*=\\s*[^#]*', 
                    sprintf('Temperature = %.3f ', avgTemperature.Krill),
                    ECS.Krill)
  
  # Replace the salinity
  ECS.CPS <- gsub('Salinity\\s*=\\s*[^#]*', 
                  sprintf('Salinity = %.3f ', avgSalinity.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('Salinity\\s*=\\s*[^#]*', 
                    sprintf('Salinity = %.3f ', avgSalinity.Krill),
                    ECS.Krill)
  
  # Replace the average absorption depth
  ECS.CPS <- gsub('AbsorptionDepth\\s*=\\s*[^#]*', 
                  sprintf('AbsorptionDepth = %.3f ', avgDepth.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('AbsorptionDepth\\s*=\\s*[^#]*', 
                    sprintf('AbsorptionDepth = %.3f ', avgDepth.Krill),
                    ECS.Krill)
  
  # Write new ECS files
  # write_file(ECS, paste(dir.ECS, file.name, "_CPS.ecs", sep = ''))
  write_file(ECS.CPS, paste(dir.ECS, file.name, "_CPS.ecs", sep = ''))
  write_file(ECS.Krill, paste(dir.ECS, file.name, "_Krill.ecs", sep = ''))
  
  # Write simple text file describing differences in CPS and Krill sound speeds
  # and the ratio to use for adjusting the Integration Stop line in Echoview
  tmp <- paste(sprintf('CPS average sound speed = %.2f m/s\n', avgSoundSpeed.CPS),
               sprintf('Krill average sound speed = %.2f m/s\n', avgSoundSpeed.Krill),
               sprintf('Krill/CPS sound speed ratio = %.6f', avgSoundSpeed.Krill/avgSoundSpeed.CPS),
               sep = '')
  write_file(tmp, paste(dir.output, file.name, '_SoundSpeedRatio.txt', sep = ''))
  
  # Copy CTD file to the PROCESSED directory
  file.copy(file.path(dir.CTD, i), dir.output)
}
