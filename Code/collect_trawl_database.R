# Collect trawl data from the trawl database. 

# Extracts all data in all specified tables and saves to an output file
# Trawl data are further processed and formatted in Code/format_trawl_database.R
# Intended to be run in scripts that load settings from Doc/settings

if (trawl.source == "SQL") {
  # Configure ODBC connection to TRAWL database
  trawl.con <- DBI::dbConnect(odbc::odbc(),
                              DRIVER="SQL Server",
                              Encrypt = "Optional",
                              DATABASE="Trawl",
                              Trusted_Connection= "Yes",
                              SERVER=trawl.db.server)
  
} else if (trawl.source == "Access") {
  # Copy trawl Access database
  haul.db <- fs::dir_ls(file.path(survey.dir[survey.vessel.primary],
                              trawl.dir),
                    regexp = trawl.db.name)
  
  fs::file_copy(haul.db, here::here("Data/Trawl"), overwrite = TRUE)
  
  # Configure ODBC connection to TRAWL database
  trawl.con  <- DBI::dbConnect(odbc::odbc(), 
                          Driver = "Microsoft Access Driver (*.mdb, *.accdb)", 
                          DBQ = file.path(here::here("Data/Trawl"), trawl.db.name))
  
} else if (trawl.source == "CLAMS-Oracle") {
  # Configure ODBC connection to CLAMS Oracle database
  trawl.con <- DBI::dbConnect(odbc::odbc(), 
                         .connection_string = paste0("Driver={Oracle in instantclient_23_8}",
                                                     ";DBQ=", clams.dbq,
                                                     ";DSN=", clams.dsn,
                                                     ";UID=", clams.uid,
                                                     ";PWD=", clams.pw),
                         timeout = 10)
} else if (trawl.source == "CLAMS-SQLite") {
  # Configure ODBC connection to CLAMS SQLite database
  trawl.con      <- DBI::dbConnect(SQLite(), dbname = here("Data/Trawl", trawl.db.name))
} else if (trawl.source == "CLAMS-Postgres") {
  # Configure ODBC connection to CLAMS SQLite database
  trawl.con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = clams_db_name,
    host = clams_db_host,
    port = clams_db_port,
    user = clams_db_user,
    password = clams_db_pw,
    options = paste0("-c search_path=", clams_db_schema) # Set the schema here
  )
}

# Import trawl database tables
if (trawl.source %in% c("SQL","Access")) {
  catch.all	     <- dplyr::tbl(trawl.con,"Catch") %>% dplyr::collect()
  haul.all       <- dplyr::tbl(trawl.con,"Haul")  %>% dplyr::collect()
  lengths.all    <- dplyr::tbl(trawl.con,"Specimen") %>% dplyr::collect()
  if (DBI::dbExistsTable(trawl.con, "LengthFrequency"))
    lengthFreq.all <- dplyr::tbl(trawl.con,"LengthFrequency") %>% dplyr::collect()
  spp.codes      <- dplyr::tbl(trawl.con,"SpeciesCodes") %>% dplyr::collect()
  
  # Save imported database data to .Rdata file
  if (exists("lengthFreq.all")) {
    save(catch.all, haul.all, lengths.all, spp.codes, lengthFreq.all, 
         file = here::here("Data/Trawl/trawl_data_raw.Rdata"))
  } else {
    save(catch.all, haul.all, lengths.all, spp.codes,  
         file = here::here("Data/Trawl/trawl_data_raw.Rdata"))
  }
  
} else if (trawl.source %in% c("CLAMS-Postgres")) {
  catch.all	     <- dplyr::tbl(trawl.con,"v_catch") %>% dplyr::collect()
  haul.all       <- dplyr::tbl(trawl.con,"v_haul")  %>% dplyr::collect()
  lengths.all    <- dplyr::tbl(trawl.con,"v_specimen") %>% dplyr::collect()
  if (DBI::dbExistsTable(trawl.con, "LengthFrequency"))
    lengthFreq.all <- dplyr::tbl(trawl.con,"LengthFrequency") %>% dplyr::collect()
  spp.codes      <- dplyr::tbl(trawl.con,"species") %>% dplyr::collect()
  
  # Save imported database data to .Rdata file
  if (exists("lengthFreq.all")) {
    save(catch.all, haul.all, lengths.all, spp.codes, lengthFreq.all, 
         file = here::here("Data/Trawl/trawl_data_raw.Rdata"))
  } else {
    save(catch.all, haul.all, lengths.all, spp.codes,  
         file = here::here("Data/Trawl/trawl_data_raw.Rdata"))
  }
} else if (trawl.source %in% c("CLAMS-Oracle", "CLAMS-SQLite")) {
  # dbListTables(trawl.con)
  baskets       <- dplyr::tbl(trawl.con, "BASKETS") %>% dplyr::collect()
  catch.summary <- dplyr::tbl(trawl.con, "CATCH_SUMMARY") %>% dplyr::collect()
  events        <- dplyr::tbl(trawl.con, "EVENTS") %>% dplyr::collect()
  event.data    <- dplyr::tbl(trawl.con, "EVENT_DATA") %>% dplyr::collect()
  event.params  <- dplyr::tbl(trawl.con, "EVENT_PARAMETERS") %>% dplyr::collect()
  event.perf    <- dplyr::tbl(trawl.con, "EVENT_PERFORMANCE") %>% dplyr::collect()
  # event.stream <- dplyr::tbl(trawl.con, "EVENT_STREAM_DATA") %>% dplyr::collect() # Shouldn't need
  gear.accy     <- dplyr::tbl(trawl.con, "GEAR_ACCESSORY") %>% dplyr::collect()
  itis.codes    <- dplyr::tbl(trawl.con, "SPECIES_DATA") %>% dplyr::collect()
  measurements  <- dplyr::tbl(trawl.con, "MEASUREMENTS") %>% dplyr::collect()
  samples       <- dplyr::tbl(trawl.con, "SAMPLES") %>% dplyr::collect()
  ships         <- dplyr::tbl(trawl.con, "SHIPS") %>% dplyr::collect()
  spp.codes     <- dplyr::tbl(trawl.con, "SPECIES") %>% dplyr::collect()  
  specimens     <- dplyr::tbl(trawl.con, "SPECIMEN") %>% dplyr::collect()
  surveys       <- dplyr::tbl(trawl.con, "SURVEYS") %>% dplyr::collect()

  # Save tables
  save(baskets,catch.summary, events, event.data, event.params, event.perf, gear.accy, 
       itis.codes, measurements, samples, ships, specimens, spp.codes, surveys, 
       file = here::here("Data/Trawl/trawl_data_raw.Rdata"))
 
  # building catch.data table from multiple sources 
}

# Close database channel
DBI::dbDisconnect(trawl.con)


