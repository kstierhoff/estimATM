# Batch process uCTD data files from the Valeport probe, then create an Echoview
# .ecs file that contains a sound speed profile

# Load required packages ---------------
#-----------------------------------

library(readr)    # For reading and writing plain text files
library(stringr)  # For processing strings
library(psych)    # For computing harmonic mean

# User Settings -----------------------------------------------------------

# Directory of CTD files to process
dir.CTD <- 'C:\\SURVEY\\2407RL\\DATA\\UCTD\\Valeport\\'

# Template ECS file
ECS.template <- 'C:\\SURVEY\\2407RL\\PROCESSED\\EV\\ECS\\_2407RL_Template.ecs'

# ECS output directory
dir.ECS <- 'C:\\SURVEY\\2407RL\\PROCESSED\\EV\\ECS\\'

# Minimum depth for determining start of downcast (in meters)
minDepth <- 2


# Read template ECS file --------------------------------------------------

# Read template file
ECS <- read_file(ECS.template)

# Get sound speed from template
c_0 <- as.numeric(str_match(ECS, "SoundSpeed\\s*=\\s*([^\\s]+)")[,2])

# Get calibration parameters that can be adjusted with sound speed
g_0 <- as.numeric(str_match_all(ECS, "TransducerGain\\s*=\\s*([^\\s]+)")[[1]][,2])
EBA_0 <- as.numeric(str_match_all(ECS, "TwoWayBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
BW_minor_0 <- as.numeric(str_match_all(ECS, "MinorAxis3dbBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])
BW_major_0 <- as.numeric(str_match_all(ECS, "MajorAxis3dbBeamAngle\\s*=\\s*([^\\s]+)")[[1]][,2])


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
  idx_start <- which(data$Depth > minDepth)[1]  # First sample where depth > 2 m
  idx_stop <- which.max(data$Depth)             # Sample of maximum depth
  data <- data[idx_start:idx_stop,]             # Retain just downcast
  
  # Perform basic data error checks
  idx <- data$Depth < 0 |       # Depth is less than 0, or
    data$Temperature <= 0 |     # Temperature is less than 0, or
    data$Salinity < 0           # Salinity is less than 0
  data[idx,] <- NA
  
  # Obtain the sound speed at the transducer depth (typically 7.35 m for
  # Intermediate position) in order to compensate calibration parameters
  idx <- which.min(abs(data$Depth - 7.35))
  txdcr.c <- data$Sound.Velocity[idx]
  
  
  # Create ECS file ---------------------------------------------------------
  
  # Copy template ECS file
  ECS.new <- ECS

  # For both CPS and Krill, replace the calibration parameters by compensating
  # for changes in sound speed
  for (j in 1:length(g_0)){
    
    # Compensate gain
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?TransducerGain\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.new, perl = TRUE)       # Find match
    ECS.new <- paste0(str_sub(ECS.new, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(g_0[j] + 20*log10(c_0 / txdcr.c), fmt = '%#.4f'),
                  str_sub(ECS.new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))

    # Compensate EBA
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?TwoWayBeamAngle\\s*=\\s*(-\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.new, perl = TRUE)       # Find match
    ECS.new <- paste0(str_sub(ECS.new, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(EBA_0[j] + 20*log10(txdcr.c / c_0), fmt = '%#.4f'),
                  str_sub(ECS.new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))

    # Compensate Alongship (Minor) Beamwidth
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?MinorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.new, perl = TRUE)       # Find match
    ECS.new <- paste0(str_sub(ECS.new, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(BW_minor_0[j] * (txdcr.c / c_0), fmt = '%#.4f'),
                  str_sub(ECS.new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))

    # Compensate Athwarthip (Major) Beamwidth
    pattern <- paste("(?s)SourceCal T", j, 
                     ".*?MajorAxis3dbBeamAngle\\s*=\\s*(\\d*\\.*\\d*)", sep = '')
    temp <- regexec(pattern, ECS.new, perl = TRUE)       # Find match
    ECS.new <- paste0(str_sub(ECS.new, 1, temp[[1]][2]-1),   # Insert new value
                  sprintf(BW_major_0[j] * (txdcr.c / c_0), fmt = '%#.4f'),
                  str_sub(ECS.new, temp[[1]][2]+attr(temp[[1]], "match.length")[2]))
  }
  
  # Replace the sound speed profile parameters
  ECS.new <- gsub('CtdDepthProfile\\s*=\\s*[^#]*', 
                  paste('CtdDepthProfile = ', paste(sprintf("%.2f", data$Depth), collapse = ';')),
                  ECS.new)
  ECS.new <- gsub('SoundSpeedProfile\\s*=\\s*[^#]*', 
                  paste('SoundSpeedProfile = ', paste(sprintf("%.2f", data$Sound.Velocity), collapse = ';')),
                  ECS.new)

  # Temperature, salinity, and absorption depth are used to compute absorption
  # coefficients, so compute the average over the cast depth
  ECS.new <- gsub('Temperature\\s*=\\s*[^#]*', 
                  sprintf('Temperature = %.2f ', mean(data$Temperature)),
                  ECS.new)
  ECS.new <- gsub('Salinity\\s*=\\s*[^#]*', 
                  sprintf('Salinity = %.2f ', mean(data$Salinity)),
                  ECS.new)
  ECS.new <- gsub('AbsorptionDepth\\s*=\\s*[^#]*', 
                  sprintf('AbsorptionDepth = %.2f ', mean(data$Depth)),
                  ECS.new)
  
  # Also replace the single sound speed value with the harmonic mean of the
  # sound speed
  ECS.new <- gsub('SoundSpeed\\s*=\\s*[^#]*', 
                  sprintf('SoundSpeed = %.2f ', harmonic.mean(data$Sound.Velocity)),
                  ECS.new)

  # Write new ECS files
  write_file(ECS.new, paste(dir.ECS, file.name, '.ecs', sep = ''))

}
