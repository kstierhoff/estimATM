# Collect trawl data from the trawl database. 

# Extracts all data in all specified tables and saves to an output file
# Trawl data are further processed and formatted in Code/format_trawl_database.R
# Intended to be run in scripts that load settings from Doc/settings

if (trawl.source == "SQL") {
  # Configure ODBC connection to TRAWL database
  trawl.con  <- DBI::dbConnect(odbc::odbc(), 
                          Driver = "SQL Server", 
                          Server = "161.55.235.187", 
                          Database = "Trawl", 
                          Trusted_Connection = "True")
} else if (trawl.source == "Access") {
  # Copy trawl Access database
  haul.db <- fs::dir_ls(file.path(survey.dir[survey.vessel.primary],
                              trawl.dir.access),
                    regexp = trawl.db.access)
  
  fs::file_copy(haul.db, here::here("Data/Trawl"), overwrite = TRUE)
  
  # Configure ODBC connection to TRAWL database
  trawl.con  <- DBI::dbConnect(odbc::odbc(), 
                          Driver = "Microsoft Access Driver (*.mdb, *.accdb)", 
                          DBQ = file.path(here::here("Data/Trawl"), trawl.db.access))
}
# Import trawl database tables
catch.all	     <- dplyr::tbl(trawl.con,"Catch") %>% dplyr::collect()
haul.all       <- dplyr::tbl(trawl.con,"Haul") %>% dplyr::collect()
lengths.all    <- dplyr::tbl(trawl.con,"Specimen") %>% dplyr::collect()
if (DBI::dbExistsTable(trawl.con, "LengthFrequency"))
  lengthFreq.all <- dplyr::tbl(trawl.con,"LengthFrequency") %>% dplyr::collect()
spp.codes      <- dplyr::tbl(trawl.con,"SpeciesCodes") %>% dplyr::collect()

# Close database channel
DBI::dbDisconnect(trawl.con)

# Save imported database data to .Rdata file
if (exists("lengthFreq.all")) {
  save(catch.all, haul.all, lengths.all, spp.codes, lengthFreq.all, 
       file = here::here("Data/Trawl/trawl_data_raw.Rdata"))
} else {
  save(catch.all, haul.all, lengths.all, spp.codes,  
       file = here::here("Data/Trawl/trawl_data_raw.Rdata"))
}
