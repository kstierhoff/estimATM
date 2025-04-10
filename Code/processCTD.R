# This script is used to batch process uCTD raw data files, then use the results
# from the CTD cast to create Echoview-calibration files (.ecs) containing the
# appropriate temperature, salinity, absorption depths, and sound speed.

# Load required packages --------------------------------------------------

library(readr)    # For reading and writing plain text files

# User Settings -----------------------------------------------------------

# Directory of CTD files to process
dir.CTD <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_LASKER_SummerCPS\\DATA\\CTD\\CTD_to_Process\\'

# Directory to store processed data results
dir.output <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_LASKER_SummerCPS\\DATA\\CTD\\PROCESSED\\REPROCESSED\\'

# Directory containing SBEDataProcessing Program Setup (.psa) files
dir.PSA <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_LASKER_SummerCPS\\DATA\\CTD\\PSA\\'

# CTD configuration file
# file.con <- 'C:\\SURVEY\\2207RL\\DATA\\CTD\\test.XMLCON'

# Directory of Seabird SBEDataProcessing programs
dir.SBE <- 'C:\\Program Files (x86)\\Sea-Bird\\SBEDataProcessing-Win32\\'

# Template ECS file
ECS.template <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_LASKER_SummerCPS\\PROCESSED\\EV\\ECS\\_2207RL_Template_20221012.ecs'

# ECS output directory
dir.ECS <- '\\\\swc-storage4-s\\AST4\\SURVEYS\\20220627_LASKER_SummerCPS\\PROCESSED\\REPROCESSING\\ECS\\CTD\\'

# Time to pause between SBADataProcessing programs, in seconds
pause <- 0.5


# Process CTD data --------------------------------------------------------

# Find all raw data files in CTD directory
files.CTD <- list.files(path = dir.CTD, pattern = "*.hex")

# Loop through each file
for (i in files.CTD) {
  
  # Retain just the file name (i.e., remove extension)
  file.name <- tools::file_path_sans_ext(i)
  
  # Use DatCnv to convert from .hex to .cnv
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DatCnvW.exe', sep = ''),
                 paste(dir.CTD, file.name, '.XMLCON', sep = ''),
                 paste(dir.CTD, file.name, '.hex', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'DatCnv.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(5)
  
  # Perform Filter
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'FilterW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'Filter.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Loop Edit
  cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'LoopEditW.exe', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'LoopEdit.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform Derive to obtain depth
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DeriveW.exe', sep = ''),
                 paste(dir.CTD, file.name, '.XMLCON', sep = ''),
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
  
  # Perform Derive to obtain salinity, sound speed, average sound speed, and
  # density
  cmd <- sprintf('"%s" /c"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
                 paste(dir.SBE, 'DeriveW.exe', sep = ''),
                 paste(dir.CTD, file.name, '.XMLCON', sep = ''),
                 paste(dir.output, file.name, '.cnv', sep = ''),
                 dir.output,
                 paste(file.name, '.cnv', sep = ''),
                 paste(dir.PSA, 'Derive.psa', sep = ''))
  system("cmd.exe", input = cmd)
  Sys.sleep(pause)
  
  # Perform ASCII Out
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
  
  # Perform basic data error checks
  idx <- data$DepSM < 0 | data$T090C <= 0 | data$Sal00 < 0
  data[idx,] <- NA
  
  # For CPS, take the average sound velocity at 70 m then calculate the average
  # temperature, salinity, and depth
  idx <- data$DepSM <= 70
  avgSoundSpeed.CPS <- tail(data$AvgsvCM[idx], n = 1)
  avgTemperature.CPS <- mean(data$T090C[idx], na.rm = T)
  avgSalinity.CPS <- mean(data$Sal00[idx], na.rm = T)
  avgDepth.CPS <- mean(data$DepSM[idx], na.rm = T)
  
  # For krill, take the average sound velocity at 350 m then calculate the
  # average temperature, salinity, and depth
  idx <- data$DepSM <= 350
  avgSoundSpeed.Krill <- tail(data$AvgsvCM[idx], n = 1)
  avgTemperature.Krill <- mean(data$T090C[idx], na.rm = T)
  avgSalinity.Krill <- mean(data$Sal00[idx], na.rm = T)
  avgDepth.Krill <- mean(data$DepSM[idx], na.rm = T)
  
  
  # Read template ECS file
  ECS <- read_file(ECS.template)
  
  # Replace the sound speed
  ECS.CPS <- gsub('SoundSpeed = [^#]*', 
                  sprintf('SoundSpeed = %.2f ', avgSoundSpeed.CPS),
                  ECS)
  ECS.Krill <- gsub('SoundSpeed = [^#]*', 
                  sprintf('SoundSpeed = %.2f ', avgSoundSpeed.Krill),
                  ECS)
  
  # Replace the temperature
  ECS.CPS <- gsub('Temperature = [^#]*', 
                  sprintf('Temperature = %.3f ', avgTemperature.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('Temperature = [^#]*', 
                    sprintf('Temperature = %.3f ', avgTemperature.Krill),
                    ECS.Krill)
  
  # Replace the salinity
  ECS.CPS <- gsub('Salinity = [^#]*', 
                  sprintf('Salinity = %.3f ', avgSalinity.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('Salinity = [^#]*', 
                    sprintf('Salinity = %.3f ', avgSalinity.Krill),
                    ECS.Krill)
  
  # Replace the average absorption depth
  ECS.CPS <- gsub('AbsorptionDepth = [^#]*', 
                  sprintf('AbsorptionDepth = %.3f ', avgDepth.CPS),
                  ECS.CPS)
  ECS.Krill <- gsub('AbsorptionDepth = [^#]*', 
                    sprintf('AbsorptionDepth = %.3f ', avgDepth.Krill),
                    ECS.Krill)
  
  # Write new ECS files
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
