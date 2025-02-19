# Collect seine data from the trawl database. 

# Extracts all data in all specified tables and saves to an output file
# Seine data are further processed and formatted in Code/format_seine_database.R
# Intended to be run in scripts that load settings from Doc/settings

if (seine.source == "SQL") {
  # Configure ODBC connection to TRAWL database
  seine.con  <- DBI::dbConnect(odbc::odbc(), 
                          Driver = "SQL Server", 
                          Server = "161.55.235.187", 
                          Database = "Trawl", 
                          Trusted_Connection = "True")
} else if (seine.source == "Access") {
  # Copy trawl Access database
  haul.db <- fs::dir_ls(file.path(survey.dir[survey.vessel.primary],
                              trawl.dir.access),
                    regexp = trawl.db.access)
  
  fs::file_copy(haul.db, here::here("Data/Trawl"), overwrite = TRUE)
  
  # Configure ODBC connection to TRAWL database
  seine.con  <- DBI::dbConnect(odbc::odbc(), 
                          Driver = "Microsoft Access Driver (*.mdb, *.accdb)", 
                          DBQ = file.path(here::here("Data/Trawl"), trawl.db.access))
}
# Import trawl database tables
sets.all       <- dplyr::tbl(seine.con,"Nearshore_Set") %>% dplyr::collect()
set.catch.all	     <- dplyr::tbl(seine.con,"Nearshore_Catch") %>% dplyr::collect()
set.lengths.all    <- dplyr::tbl(seine.con,"Nearshore_Specimen") %>% dplyr::collect()
if (DBI::dbExistsTable(seine.con, "LengthFrequency"))
  lengthFreq.all <- dplyr::tbl(seine.con,"LengthFrequency") %>% dplyr::collect()
spp.codes      <- dplyr::tbl(seine.con,"SpeciesCodes") %>% dplyr::collect()

# Close database channel
DBI::dbDisconnect(seine.con)

# Create directory for saving seine data
fs::dir_create(here("Data/Seine"))

# Save imported database data to .Rdata file
if (exists("lengthFreq.all")) {
  save(sets.all, set.catch.all, set.lengths.all, spp.codes, lengthFreq.all, 
       file = here::here("Data/Seine/seine_data_raw.Rdata"))
} else {
  save(sets.all, set.catch.all, set.lengths.all, spp.codes,
       file = here::here("Data/Seine/seine_data_raw.Rdata"))
}
