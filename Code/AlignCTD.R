# Conduct some basic CTD processing steps to derive plots that will help
# determine what coefficients to use for the AlignCTD program.

# User Settings -----------------------------------------------------------

# Directory of CTD files to process
dir.CTD <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\CTD_to_Process\\'

# Directory to store processed data results
dir.output <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\PROCESSED\\'

# Directory containing SBEDataProcessing Program Setup (.psa) files
# dir.PSA <- paste0(normalizePath(file.path(getwd(), 'CODE/PSA/')),'\\')
dir.PSA <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\PSA_UCTD\\'

# CTD configuration file. If left blank (i.e., ''), the script will assume that
# the configuration file has the same name as the CTD input file, which is
# typically the case for CTD (not UCTD) casts.
file.con <- 'C:\\Users\\josiah.renfree\\Desktop\\TEMP\\CTD_to_Process\\UCTD.con'

# Directory of Seabird SBEDataProcessing programs
dir.SBE <- 'C:\\Program Files (x86)\\Sea-Bird\\SBEDataProcessing-Win32\\'

# Time to pause between SBADataProcessing programs, in seconds
pause <- 2


# Process CTD data --------------------------------------------------------

# Find all raw data files in CTD directory
files.CTD <- list.files(path = dir.CTD, pattern = ".*\\.(hex|asc)")

# Retain just the file name (i.e., remove extension)
file.name <- tools::file_path_sans_ext(files.CTD)

# Retain extension
file.ext <- tools::file_ext(files.CTD)

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
  Sys.sleep(pause + 5)

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
  
# Perform Filter to reduce noise
cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
               paste(dir.SBE, 'FilterW.exe', sep = ''),
               paste(dir.output, file.name, '.cnv', sep = ''),
               dir.output,
               paste(file.name, '.cnv', sep = ''),
               paste(dir.PSA, 'Filter.psa', sep = ''))
system("cmd.exe", input = cmd)
Sys.sleep(pause)

# Perform Derive to derive salinity
cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
               paste(dir.SBE, 'DeriveW.exe', sep = ''),
               paste(dir.output, file.name, '.cnv', sep = ''),
               dir.output,
               paste(file.name, '.cnv', sep = ''),
               paste(dir.PSA, 'Derive_Salinity.psa', sep = ''))
system("cmd.exe", input = cmd)
Sys.sleep(pause)


# Perform Split to retain downcast
cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
               paste(dir.SBE, 'SplitW.exe', sep = ''),
               paste(dir.output, file.name, '.cnv', sep = ''),
               dir.output,
               paste(file.name, '.cnv', sep = ''),
               paste(dir.PSA, 'Split.psa', sep = ''))
system("cmd.exe", input = cmd)
Sys.sleep(pause)

# Perform Align CTD
cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
               paste(dir.SBE, 'AlignCTDW.exe', sep = ''),
               paste(dir.output, 'd', file.name, '.cnv', sep = ''),
               dir.output,
               paste(file.name, '.cnv', sep = ''),
               paste(dir.PSA, 'AlignCTD.psa', sep = ''))
system("cmd.exe", input = cmd)
Sys.sleep(pause)
  
# Perform SeaPlot to generate plot of CTD profile
cmd <- sprintf('"%s" /i"%s" /o"%s" /f"%s" /p"%s" /s',
               paste(dir.SBE, 'SeaPlotW.exe', sep = ''),
               paste(dir.output, file.name, '.cnv', sep = ''),
               dir.output,
               paste(file.name, '.jpg', sep = ''),
               paste(dir.PSA, 'SeaPlot_AlignCTD.psa', sep = ''))
system("cmd.exe", input = cmd)
Sys.sleep(pause)
