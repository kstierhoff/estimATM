# Processing controls ----------------------------------------------------
## Settings in this section control various behaviors and tasks used in the main data processing scripts
### Biomass estimation
process.seine     <- T # Process purse seine data, if present
process.nearshore <- T # Process near backscatter data; typically TRUE
estimate.ns       <- T # Estimate biomass in the nearshore strata; T if nearshore surveyed
process.offshore  <- F # Process offshore backscatter data
estimate.os       <- F # Estimate biomass in the offshore strata; T if offshore surveyed
combine.regions   <- T # Combine nearshore/offshore plots with those from the core region

# Survey planning ---------------------------------------------------------
## This section controls and configures settings used by makeTransects and checkTransects for generating and checking survey transects
### Transect spacing (nautical miles)
tx.spacing.fsv  <- 10 # For Lasker 
tx.spacing.sd   <- tx.spacing.fsv/2 # For Saildrone
tx.spacing.ns   <- c("S" = 5, "N" = 7, "CI" = 2.5) # or NA
tx.break.ns     <- 64 # Northernmost transect sampled by the southern F/V, 64 in 2024, near Carmel
tx.spacing.os   <- 40 # Nearshore transect spacing, in nmi; set NA if calculating programatically

# Mainland buffer distance for FSV and Saildrone transects
sd.buffer  <- 6 # nmi
fsv.buffer <- 80 #limits offshore portion of lines (SCB)

# Minimum transect length
min.tx.length <- 0 # nmi
                                      
# UCTD spacing (nautical miles)
uctd.spacing   <- 15

### Transect removal and renumbering
rm.n.transects     <- 37 # Number of transects to remove from the start (if near Mexico)
rm.i.transects     <- NA # Remove specific transects from plan; else NA (for 2007RL: c(paste(90:117, "Nearshore")))
renumber.transects <- TRUE # Renumber transects to start at zero if transect are removed

# Locations to remove from planning (e.g., north, central, south, and mexico)
rm.location <- NA # c("mexico")

# Randomize
do.random <- TRUE
save.csv  <- TRUE
show.maps <- TRUE

## Used by processTransects.R -----------
# GPX file location
gpx.dir          <- here("Data/Nav")
gpx.file         <- "rosepoint_waypoints.gpx" #"rosepoint_waypoints.gpx"

# Define transit and survey speed (kn) for estimating progress
survey.speed     <- 9.5
transit.speed    <- 10
survey.direction <- "Northward" # Southward or Northward; to compute day lengths

# Beginning transit length (d)
transit.distance <- 0 # begining in san diego 2024
transit.duration <- ceiling(transit.distance/transit.speed/24)

# Leg waste (d) due to transit, late departures, and early arrivals
leg.waste <- c(2, 2, 2, 2) # Speculating, to make code work

# Remove transects to adjust survey progress
transects.rm <- NA # Numbered transects to remove

# Compute leg durations and breaks ----------------------------------------
# Define leg ends
leg.ends <- c(ymd("2024-06-25"), ymd("2024-07-17"),
              ymd("2024-07-22"), ymd("2024-08-12"),
              ymd("2024-08-17"), ymd("2024-09-07"),
              ymd("2024-09-12"), ymd("2024-09-30"))

# Compute days per leg
leg.days <- (leg.ends[seq(2, length(leg.ends), 2)] - leg.ends[seq(1,length(leg.ends) - 1, 2)]) + 1

# Calculate total days at sea (DAS)
total.das <- sum(leg.days)

# Leg durations used to split transects 
leg.length <- c(0, leg.days - leg.waste)

# Leg breaks
leg.breaks.gpx <- cumsum(as.numeric(leg.length))

# Region vector used to break transects for waypoint files
region.vec <- c(0, 32.5353, 34.7, 41.99, 48.490, 55)

## Used by estimateAcousticKm.Rmd ------
# Get nearshore vessels
nav.vessels.ns <- c("LM","LBC")

# Survey information ------------------------------------------------------
# Full survey name; only used in report title
survey.name.long       <- "Summer 2024 California Current Ecosystem Survey (CCES)"
survey.vessel.long     <- "Reuben Lasker" # Full vessel name: e.g., Bell M. Shimada
survey.vessel          <- "Lasker"        # Short vessel name; e.g., Shimada
survey.vessel.primary  <- "RL"            # Primary vessel abbreviation 
survey.name            <- "2407RL"        # SWFSC/AST survey name
survey.start           <- "30 June"       # Survey start date (discounts 5d sea trials in 2407RL)
survey.end             <- "30 September"  # Survey end date
survey.year            <- "2024"          # Survey year, for report
survey.season          <- "Summer"        # Survey season, for report
survey.das             <- 85              # Days at sea allocated
survey.landmark.n      <- "Winter Harbour, Vancouver Island, Canada" # Landmark - N extent of survey
survey.landmark.s      <- "Punta Eugenia, Baja CA, Mexico" # Landmark - S extent of survey
survey.twilight        <- "none"          # Sunset type for computing day/night (none, nautical, civil, astronomical)
survey.twilight.offset <- 30              # Twilight offset; minutes before sunrise/after sunset
survey.twilight.remove <- FALSE           # Remove twilight period (T/F)
daynight.filter        <- c("Day","Night")# A character string including "Day", "Night", or both

# Inport dates for classifying data by cruise leg (if desired) -----------------
# Use start dates of each leg + end date of last leg
leg.breaks <- as.numeric(lubridate::ymd(c("2024-06-26", "2024-07-22", 
                                          "2024-08-17", "2024-09-12",
                                          "2024-09-30")))

# Define ERDDAP data variables for primary NOAA vessel
erddap.url           <- "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip"
erddap.vessel        <- "WTEG"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.survey.start  <- "2024-06-24" # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2024-09-30" # End of survey for ERDDAP vessel data query
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed,windDirection,windSpeed,flag")
erddap.classes       <- c("character", "numeric", "numeric", "numeric","numeric","numeric","numeric","character")
erddap.headers       <- c("time", "lat","long","SST","SOG","wind_dir","wind_speed","flag")
erddap.flags         <- c('"ZZZZ.Z.Z..Z.*"')
survey.lat           <- c(27,51)
survey.long          <- c(-130,-113)

# Inport dates for classifying data by cruise leg (if desired) -----------------
# Use start dates of each leg + end date of last leg
leg.breaks.sh <- as.numeric(lubridate::ymd(c("2024-06-26", "2024-07-22", 
                                             "2024-08-17", "2024-09-12",
                                             "2024-09-30")))

# Define ERDDAP data variables for Shimada
erddap.vessel.sh        <- "WTED"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.survey.start.sh  <- "2023-10-09" # Start of survey for ERDDAP vessel data query
erddap.survey.end.sh    <- "2023-11-04" # End of survey for ERDDAP vessel data query
erddap.flags.sh         <- c('"ZZZZ.Z.Z..Z.*"')

# Survey plan info --------------------------------------------------------
wpt.filename         <- "waypoints_2407RL.csv"
wpt.types            <- c(Adaptive = "Adaptive", Carranza = "Carranza",
                          Compulsory = "Compulsory", Nearshore = "Nearshore",
                          Offshore = "Offshore", Saildrone = "Saildrone")
wpt.colors           <- c(Adaptive = "#FF0000", Carranza = "green",
                          Compulsory = "#000000", Nearshore = "#FF33F5", 
                          Offshore = "#FFA500", Saildrone = "#00FFFF") 
wpt.linetypes        <- c(Adaptive = "dashed", Carranza = "solid",
                          Compulsory = "solid", Nearshore = "solid", 
                          Offshore = "dashed", Saildrone = "dashed")

# Saildrone info -----------------------------------------------
# Select Saildrone numbers
sd.numbers <- c("1048", "1060", "1096")

# Set Saildrone filter method
sd.buffer.type   <- c("saildrone")
sd.buffer.dist   <- 1.5 # buffer distance (nmi) around planned transects to classify SD intervals
sd.filter.method <- "manual" # Options are c("buffer","manual")
sd.nasc.name     <- "cps_nasc_SD.csv"

# Define Saildrone sampling dates
survey.start.sd  <- "7 July" # Start of Saildrone survey
survey.end.sd    <- "15 October" # End of Saildrone survey

# Set date range
erddap.url.sd          <- "https://data.pmel.noaa.gov/pmel/erddap/tabledap/all_swfsc_2023"
erddap.survey.start.sd <- "2023-07-08T00%3A00%3A00Z"
erddap.survey.end.sd   <- "2023-10-15T23%3A59%3A00Z"
# Configure columns and classes
erddap.vars.sd         <- c("trajectory,latitude,longitude,SOG,time")
erddap.headers.sd      <- c("saildrone", "lat", "long", "SOG", "time")
erddap.classes.sd      <- c(rep("numeric", length(erddap.headers.sd) - 1),"character")

# Define date range for each Saildrone to remove overlapping transits
sd.date.range    <- data.frame(saildrone  = c(1048, 1060, 1096),
                               start.date = ymd(c("2023-07-08", "2023-07-08", "2023-07-08")),
                               end.date   = ymd(c("2023-10-15", "2023-10-15", "2023-10-15")))

# Adjust time in Saildrone gps.csv files, if problems with Mission Planner (e.g., 1907RL)
sd.time.offset   <- 0 # Hours to add/subtract from GPS data (typically 0)

# Filter variables for TRAWL and CUFES data on SQL Server ----------------------
cruise.name <- 202407 # May be a numeric or numeric vector (e.g., c(201704,201706,...))
cruise.ship <- c("RL", "SH") # May be a character or character vector (e.g., c("RL",",...))

# Growth model parameters ------------------------------------------------------
model.season  <- "summer" # spring or summer; for selecting growth model parameters
model.type    <- "glm"    # lm, nlm, or glm; for selecting growth model

# Mapping preferences -----------------------------------------------------
# Turn off S2 processing in sf
sf::sf_use_s2(FALSE)
mapviewOptions(basemaps = c("CartoDB.Positron","Esri.WorldImagery","Esri.OceanBasemap"))

# Coordinate reference systems for geographic and projected data
crs.geog <- 4326 # WGS84
crs.proj <- 3310 # California Albers Equal Area

# Default map height
map.height <- 10
# Map height for specific regions; used in makeTransects, checkTransects
map.height.region <- c(central = 12, mexico = 7, north = 7, south = 10)
map.label.size <- c(central = 2, mexico = 3, north = 3, south = 5)

# Leaflet tile options; set both to T if caching
useCachedTile  <- F # Use cached tiles
useCrossOrigin <- F # Use cross origin
leaflet.checkTransects.simple <- TRUE # Use a simple Leaflet for checkTransects

# Trawl proportion plots
scale.pies <- FALSE   # Scale pie charts (TRUE/FALSE)
pie.scale  <- 0.0125 # 0.01-0.02 works well for coast-wide survey (i.e., summer), larger values (~0.03) for spring

# Lookup table for renaming columns
pie.spp <- c("Jacksmelt"  = "Atherinopsis californiensis", "PacHerring" = "Clupea pallasii",
             "Anchovy"    = "Engraulis mordax", "Sardine"    = "Sardinops sagax",
             "PacMack"    = "Scomber japonicus", "JackMack"   = "Trachurus symmetricus",
             "RndHerring" = "Etrumeus acuminatus", "AllCPS" = "AllCPS")

# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego",
                "Ensenada","Punta Eugenia","El Rosario","Cabo San Lucas",
                "Punta Abreojos","San Carlos")

# Species, stock and strata for nearshore biomass plots -------------------
spp.common.ns <- "Northern Anchovy"
spp.ns        <- "Engraulis mordax"
stock.ns      <- "Central"
stratum.ns    <- 1

# Figure preferences ------------------------------------------------------
# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'
rnd.herring.color  <- '#F0B81D'
other.color        <- 'gray'

# Set gear type colors
seine.color <- "white"
trawl.color <- "black"

# Define species to be analysed
cps.spp            <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                        "Scomber japonicus","Trachurus symmetricus", 
                        "Etrumeus acuminatus")
# CUFES
cufes.start        <- "2023-07-03" # Start of survey for CUFES filtering
cufes.end          <- "2023-09-30" # End of survey for CUFES filtering
# For legend objects
cufes.date.range   <- c(start = ymd_hms("2023-07-01 19:30:00 UTC"), 
                        stop  = now())
cufes.breaks       <- c(0, 0.1, 1, 10, 25, 50, 250, 500, 10000) 
cufes.labels       <- c("<0.1", "0.1-1", "1-10", "10-25", "25-50", 
                        "50-250", "250-500", ">500")
cufes.sizes        <- c(0.5, 1, 2, 3, 4, 5, 6, 7)
cufes.plot.spp     <- c("AnchovyEggs","JackMackerelEggs","SardineEggs")
cufes.colors       <- c("AnchovyEggs"      = anchovy.color, 
                        "JackMackerelEggs" = jack.mack.color, 
                        "SardineEggs"      = sardine.color)
cufes.spp.labels   <- c("AnchovyEggs"      = "Anchovy",
                        "JackMackerelEggs" = "J. mackerel",
                        "SardineEggs"      = "Sardine")
# Trawl
# For legend objects
trawl.breaks       <- c(0, 1, 10, 25, 50, 500, 1000, 10000) 
trawl.labels       <- c("<1", "1-10", "10-25", "25-50", "50-500", "500-1000", ">1000") 
trawl.sizes        <- c(1, 2, 3, 4, 5, 6, 7) 

# For pie charts; subsetted using pie.spp, which is defined from the catch data
# Species columns
pie.cols <- c("Engraulis mordax" = "Anchovy", "Trachurus symmetricus" = "JackMack", 
                 "Atherinopsis californiensis" = "Jacksmelt", "Clupea pallasii" = "PacHerring", 
                 "Scomber japonicus" = "PacMack", "Etrumeus acuminatus" = "RndHerring", 
                 "Sardinops sagax" = "Sardine")

pie.cols.prop <- c("Engraulis mordax" = "prop.anch", "Trachurus symmetricus" = "prop.jack", 
                   "Atherinopsis californiensis" = "prop.jsmelt", "Clupea pallasii" = "prop.her", 
                   "Scomber japonicus" = "prop.mack", "Etrumeus acuminatus" = "prop.rher", 
                   "Sardinops sagax" = "prop.sar")

# Species labels
pie.labs <- c("Engraulis mordax" = "Anchovy", "Trachurus symmetricus" = "J. Mackerel", 
                "Atherinopsis californiensis" = "Jacksmelt", "Clupea pallasii" = "P. herring", 
                "Scomber japonicus" = "P. mackerel", "Etrumeus acuminatus" = "R. herring", 
                "Sardinops sagax" = "Sardine")

# Species colors
pie.colors <- c("Engraulis mordax" = anchovy.color, "Trachurus symmetricus" = jack.mack.color, 
                "Atherinopsis californiensis" = jacksmelt.color, "Clupea pallasii" = pac.herring.color, 
                "Scomber japonicus" = pac.mack.color, "Etrumeus acuminatus" = rnd.herring.color,
                "Sardinops sagax" = sardine.color)

# NASC
# For legend objects
nasc.breaks        <- c(0, 1, 200, 500, 2000, 5000, 20000, 50000, 20000000)
nasc.labels        <- c("0","1-200", "200-500", "500-2000", "2000-5000", 
                        "5000-20,000", "20,000-50,000", ">50,000")
nasc.scale         <- 0.75 # Scale percentage (smaller for larger scale, e.g., 0.5 v 0.75)
nasc.sizes         <- c(0.1, 0.25, 2, 3, 4, 5, 6, 7)*nasc.scale
nasc.colors        <- c("#000000", "#C2E6F2", "#1E90FF", "#FFFF00", "#FF8C00", 
                        "#FF0000", "#FFC0CB", "#FFFFFF")

# Acoustic biomass density map
dens.breaks        <- c(0, 1, 10, 100, 500, 1000, 10000, 50000, 1000000)
dens.labels        <- c("0-1", "1-10", "10-100", "100-500", "500-1000",
                        "1000-10,000", "10,000-50,000", ">50,000")
dens.colors        <- c("#000000", "#1E90FF", "#FFFF00", "#FF8C00", 
                        "#FF0000", "#FFC0CB", "#FFFFFF", "#00FF00") # for legend colors
dens.sizes         <- c(0.25, 1, 2.25, 3, 4.25, 5.5, 6.5, 7.5) # for legend sizes

# Catch map
# For legend objects
catch.breaks       <- c(0, 10, 100, 500, 1000)
catch.labels       <- c("0-10", "10-100", "100-500", "500-1000")
catch.pie.sizes    <- c(1, 2, 3, 4, 5, 6)

annotation.size <-  2.5    # Font size for annotations; try 4 for spring surveys, 2.5 for summer surveys

# Combining length-disaggregated abundance/biomass across regions ---------------
L.disagg.scales <- "fixed" # "fixed" or "free" scales?

# Cluster relative length frequency
# Set number of columns in facet plot
lf.ncols <- 5

# Data sources ------------------------------------------------------------
# Backscatter data info
# Survey vessels that collected acoustic data (a character vector of vessel abbreviations)
nasc.vessels           <- c("RL","LM","LBC") #c("RL","LBC","LM","SD") 
nasc.vessels.offshore  <- NA # c("SD")
nasc.vessels.nearshore <- c("LBC","LM") #,"LM"
nasc.vessels.krill     <- c("RL")

# Define columns to use for a fixed integration depth (if cps.nasc is not present)
# Options include 0-100 (by 5), 100, 150, 250, and 350 m.
# Defined by the atm::extract_csv() function.
nasc.depth.cps   <- "NASC.250"
nasc.depth.krill <- "NASC.350"

# Combine data from all vessels?
# Should data from different vessels be combined, e.g., for Lasker and Saildrone
# in the same strata? Or, in 2023, Lasker and Shimada when additional sea days were provided
merge.vessels <- c(Core = TRUE, # To combine RL and SH in 2407RL; perhaps SD too
                   OS = FALSE,
                   NS = FALSE)

# Combine data from all regions?
# For most cases, should include all vessels, as region shouldn't be used to stratify backscatter
# However, for LBC in 1907RL, Islands are stratified separately from mainland transects,
# so regions for that vessel are not merged
# Used in estimateNearshore.R
merge.regions <- NA_character_

# Interval length (m); from Echoview
nasc.interval          <-  100    

# Number of intervals over which to summarize NASC
nasc.summ.interval     <- 2000/nasc.interval 

# Echosounder type; e.g., EK60, EK80, other
sounder.type           <- c(RL  = "EK80",
                            SH  = "EK80") 

# Location of survey data on AST1, AST2, etc. (a vector of file paths)
# Root directory where survey data are stored; other paths relative to this
if (Sys.info()['nodename'] %in% c("SWC-FRD-AST1-D","SWC-KSTIERH1-L")) {
  survey.dir           <- c(RL  = "//swc-storage4-s/AST5/SURVEYS/20240625_LASKER_SummerCPS",
                            LBC = "//swc-storage4-s/AST4/SURVEYS/20240625_CARNAGE_SummerCPS",
                            LM  = "//swc-storage4-s/AST4/SURVEYS/20240625_LISA-MARIE_SummerCPS")
} else if (Sys.info()['nodename'] %in% c("RL4433188-CHL1")) {
  survey.dir           <- c(RL  = "C:/Users/Survey.RL/Desktop/2407RL_SWFSC",
                            LBC = "//swc-storage4-s/AST5/SURVEYS/20240625_CARNAGE_SummerCPS",
                            LM  = "//swc-storage4-s/AST5/SURVEYS/20240625_LISA-MARIE_SummerCPS")
} else {
  survey.dir           <- c(RL  = "//swc-storage4-s/AST5/SURVEYS/20240625_LASKER_SummerCPS",
                            LBC = "//swc-storage4-s/AST5/SURVEYS/20240625_CARNAGE_SummerCPS",
                            LM  = "//swc-storage4-s/AST5/SURVEYS/20240625_LISA-MARIE_SummerCPS")
}

# Backscatter data (within survey.dir, typically)
nasc.dir               <- c(RL  = "PROCESSED/EV/CSV",
                            LM  = "PROCESSED/EV/CSV",
                            LBC = "PROCESSED/EV/CSV",
                            SD  = "PROCESSED/EV/CSV",
                            SH  = "PROCESSED/EV/CSV") 

# Regex pattern for identifying CPS CSV files
nasc.pattern.cps       <- c(RL  = "Final 38 kHz CPS_nasc_cps.csv",
                            LM  = "Final 38 kHz CPS_nasc_cps.csv",
                            LBC = "Final 38 kHz CPS_nasc_cps.csv",
                            SD  = "Final 38 kHz CPS_nasc_cps.csv",
                            SH  = "Final 38 kHz CPS_nasc_cps.csv")
# Regex pattern for identifying krill CSV files
nasc.pattern.krill     <- c(RL  = "Juan Krill Final 120.csv",
                            LM  = "Juan Krill Final 120.csv",
                            LBC = "Juan Krill Final 120.csv",
                            SD  = "Juan Krill Final 120.csv",
                            SH  = "Juan Krill Final 120.csv")
# Regex pattern for identifying nearshore transects
nasc.pattern.nearshore <- c(RL  = "\\d{3}N",
                            LM  = "\\d{3}N",
                            LBC = "\\d{3}N",
                            SD  = "\\d{3}N",
                            SH  = "\\d{3}N")
# Regex pattern for identifying offshore transects
nasc.pattern.offshore  <- c(RL  = "\\d{3}O",
                            LM  = "\\d{3}O",
                            LBC = "\\d{3}O",
                            SD  = "\\d{3}O",
                            SH  = "\\d{3}O")
# Regex pattern for identifying inshore transits between transects
nasc.pattern.inshore   <- c(RL  = "\\d{3}I",
                            LM  = "\\d{3}I",
                            LBC = "\\d{3}I",
                            SD  = "\\d{3}I",
                            SH  = "\\d{3}I")
# Regex pattern for identifying transits
nasc.pattern.transit   <- c(RL  = "\\d{3}T",
                            LM  = "\\d{3}T",
                            LBC = "\\d{3}T",
                            SD  = "\\d{3}T",
                            SH  = "\\d{3}T")
# Recursively search NASC directories
nasc.recurse           <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = TRUE,
                            SH  = FALSE)
# Max NASC value for removing outliers
nasc.max               <- NA

# Purse seine data info
# Survey vessels that collected purse seine data
seine.vessels          <- c("LBC","LM")
seine.vessels.long     <- c("LBC" = "Long Beach Carnage",
                            "LM"  = "Lisa Marie")
# Use seine data to apportion nearshore backscatter
# If seine catches were believed to be representative, TRUE
# Else, FALSE (e.g., if sets were non-random or otherwise believed to be biased)
use.seine.data  <- TRUE

# Vessels for which to correct deep nasc that may be anchovy
deep.nasc.vessels <- c("LBC","LM")

# Which net data should be used to apportion nearshore backscatter?
# "Trawl" and/or "Seine"
catch.source.ns <- c("Purse seine", "Trawl")

# Define path to seine data directories for each vessel
seine.data.paths <- c("LBC"= file.path(survey.dir["LBC"], "DATA/SEINE/lbc_data_2407RL.xlsx"),
                      "LM" = file.path(survey.dir["LM"],  "DATA/SEINE/lm_data_2407RL.xlsx"))

# source.cps.nasc determines whether to use cps.nasc values from a separate file
# Since 2022, Code/extract_CPS_NASC.R is used to remove non-CPS backscatter and compute cps.nasc
# Prior to that, cps.nasc was produced using the CTDapp and supplied (usually by Juan) in an external file

# If F (typical, since 2002), the code will extract the CPS backscatter from the specified CSV files
# If cps.nasc is present in the specified CSV file, the values in that column are used
# If not, cps.nasc is set to a fixed depth manually defined by nasc.depth.cps

# If T, read cps.nasc from file defined in data.cps.nasc (below)
source.cps.nasc        <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE,
                            NS  = FALSE,
                            SH  = FALSE) # in the nearshore strata

# File containing CPS nasc from CTD app
data.cps.nasc          <- c(RL  = here("Data/Backscatter/nasc_cps_RL_2407RL.csv")) # in the nearshore strata 

# regex for matching character pattern
tx.char.pattern        <- c(RL  = "[^0-9]",
                            SH  = "[^0-9]",
                            LM  = "[^0-9]",
                            LBC = "[^0-9]",
                            SD  = "[^0-9]") 

# If T, strips numbers from transect names (i.e., would combine 105-1 and 105-2 to 105)
strip.tx.nums          <- c(RL  = TRUE,
                            SH  = TRUE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = TRUE) 

# If T, strips characters from transect numbers (i.e., would combine 105A and 105B to 105)
strip.tx.chars         <- c(RL  = TRUE,
                            SH  = TRUE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE) 

# If T, removes transects with names including "transit"
rm.transit             <- c(RL  = FALSE,
                            SH  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE)  

# If T, removes transects with names including "offshore"
rm.offshore            <- c(RL  = TRUE,
                            SH  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE) 

# If T, removes transects with names including "inshore"
rm.inshore             <- c(RL  = TRUE,
                            SH  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE)

# If T, removes transects with names including "nearshore"
rm.nearshore           <- c(RL  = TRUE,
                            SH  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE)

# If T, extracts nearshore intervals from vessels that sample close to shore
extract.nearshore      <- c(RL  = FALSE,
                            SH  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE)

# If T, subtracts NASC.5 from cps.nasc
rm.surface             <- c(RL  = FALSE,
                            SH  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE) 

# regex for matching number pattern
tx.num.pattern         <- c(RL  = "-\\d{1}",
                            SH  = "-\\d{1}",
                            LM  = "-\\d{1}",
                            LBC = "-\\d{1}",
                            SD  = "-\\d{1}")

# Use transect names for transect numbers
use.tx.number          <- c(RL  = TRUE,
                            SH  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE)

# Transects to manually exclude e.g., data.frame(vessel = "RL", transect = c("085","085-2"))
tx.rm                  <- list(RL  = NA,
                               SH  = NA,
                               LM  = NA,
                               LBC = c("LBC 24", "LBC 40"), # Short transects off S. CA
                               SD  = NA)

# Minimum acoustic transect length (nmi)
min.tx.length          <- c(RL  = 12, # Transect 149 off VI was 12.7 nmi and had been excluded
                            SH  = 12,
                            LM  = 1,
                            LBC = 1,
                            SD  = 1)

# Enforce nearest trawl cluster distance limits?
limit.cluster.dist     <- c(OS  = FALSE,
                            NS  = FALSE) 

# Define source of species proportions and length frequency data (either clf or hlf)
# Uses either haul or cluster data for a given region (NS or OS)
cluster.source <- c(OS = "cluster",
                    NS = "cluster")

# Manually exclude hauls or purse seine sets from the analysis
# List trawl hauls (e.g., c(1, 2,...n)), else NA
haul.rm     <- NA
# List sets (e.g., c("LBC 2024-07-16 25",...n)), else NA
key.set.rm  <- c("LBC 2024-07-16 25") # Set/landing 25/160 had no specimens due to a freezer failure

# Maximum distance to trawl clusters
cum.biomass.limit      <- 0.90 # Distance used to compute max.cluster.distance

# If limit.cluster.dist == TRUE, set proportions to zero at distances greater than max.cluster.dist
max.cluster.dist       <- 30

# Define transect spacing bins and values (nmi) used to characterize transect spacing
tx.spacing.bins <- c(0, 3, 6, 8, 15, 35, 70, 100)
tx.spacing.dist <- c(2.5, 5, 7, 10, 20, 40, 80)

# SCS data
scs.source             <- "ELG" # "CSV", "ELG", or "XLSX"
scs.pattern            <- "MOA*.*xlsx" # regex for MOA files

# SCS data info for extracting NAV data
scs.nav.path           <- "C:/SURVEY/2407RL/DATA/SCS" # Local
scs.nav.dir            <- "SAMOS"
scs.nav.pattern        <- "SAMOS-OBS.*.elg"
scs.nav.recurse        <- TRUE

# CUFES data
cufes.source           <- "SQLite" # "SQL" or "SQLite"
cufes.dir.sqlite       <- file.path(survey.dir[survey.vessel.primary], "DATA/BIOLOGICAL/CUFES")
cufes.db.sqlite        <- "cufes202407RL.sqlite" # CUFES SQLite database
cufes.date.format      <- "mdy" # mdy (1907RL and later) or ymd (earlier surveys)
cufes.vessels          <- c("RL")

# Trawl data
trawl.source           <- "SQL"    # "SQL" or "Access"
trawl.dsn              <- "TRAWL"  # DSN for Trawl database on SQL server
trawl.db.access        <- "TrawlDataEntry2407RL.accdb"
trawl.performance      <- c("Aborted") # Character vector; trawl performance to exclude
trawl.haul.exclude     <- NA # Numeric vector; haul numbers to exclude (e.g., for incomplete catch, etc.; NA if include all)

# Location of trawl Access database
if (Sys.info()['nodename'] %in% c("SWC-FRD-AST1-D","SWC-KSTIERH1-L")) {
  trawl.dir.access <- "DATA/BIOLOGICAL/HAUL"
} else if (Sys.info()['nodename'] %in% c("RL4433188-CHL1")) {
  trawl.dir.access <- ""
} else {
  trawl.dir.access <- "DATA/BIOLOGICAL/HAUL"
}

# CTD data
ctd.dir                <- file.path(survey.dir[survey.vessel.primary],"DATA/CTD")
ctd.hdr.pattern        <- "RL2203*.*hdr"
ctd.cast.pattern       <- ".*_processed.asc"
ctd.cast.depth         <- 350

# UCTD data   
uctd.dir               <- file.path(survey.dir[survey.vessel.primary],"DATA/UCTD/Valeport")
uctd.type              <- "Valeport" # "Valeport" or "Oceansciences"
uctd.hdr.pattern       <- ".*UCTD\\d{3}-\\d{1}.*.vp2"
uctd.cast.pattern      <- ".*UCTD\\d{3}-\\d{1}.*.vp2"
uctd.cast.depth        <- 300

# TDR data
tdr.dir.kite           <- here("Data/TDR/Kite")
tdr.dir.foot           <- here("Data/TDR/Footrope")
tdr.dir.stbd           <- here("Data/TDR/Starboard")
tdr.dir.port           <- here("Data/TDR/Port")
tdr.pattern            <- "2407RL*.*rsk"
tdr.recurse            <- TRUE # Recursively search TDR directory
tdr.tz                 <- "America/Los_Angeles" # Time zone setting for TDRs
# Time offset, in hours (usually -1, diff between PDT and PST in summer)
tdr.offset             <- rep(-1, 300) 
# tdr.offset             <- c(rep(-1, 72), # Time settings were updated after haul 73
#                             rep( 0, 20)) # Time settings were updated after haul 73
tdr.offset             <- setNames(tdr.offset, 1:length(tdr.offset)) # Add names from haul numbers
tdr.nav.source         <- "ERDDAP"
tdr.trawl.source       <- "Access"
tdr.cruise             <- c("202407") # Cruise name(s) for TDR files

# Biomass estimation settings ------------------------------------------
# Length bins and labels for calculating length frequencies 
length.min <- 1 # Minimum length bin for length frequencies
# (max. anchovy = 20 cm, sardine & herrings = 30 cm, Pac. mack = 40, and jack mack. = 60)
length.max <- data.frame("species" = c("Clupea pallasii","Engraulis mordax",
                                       "Sardinops sagax", "Scomber japonicus",
                                       "Trachurus symmetricus","Etrumeus acuminatus"),
                         "sl" = c(30,20,30,50,60,30))

# Species to generate point estimates
point.est.spp          <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")
# Species to generate point estimates
bootstrap.est.spp      <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")

# Number of bootstrap samples
boot.num <- 1000 # 1000 during final

# Generate biomass length frequencies
do.lf    <- TRUE

# Define regions to present in main Results
estimate.regions   <- c("Core", "Nearshore") 

# Define rules for selecting and pruning sampling strata -----------------------
# Defines breaks between strata
max.diff <- 3
# Defines minimum number of transects in a stratum
nTx.min <- 3

# Stratum pruning settings
nIndiv.min    <- 1
nClusters.min <- 1

# Use manually defined strata?
stratify.manually    <- FALSE
stratify.manually.os <- FALSE
stratify.manually.ns <- FALSE

# Manually define sampling strata for each species
# Create a new data frame with each species, stratum, and vector containing transects

if ("SD" %in% nasc.vessels) {
  # If including Saildrone
  strata.manual <- bind_rows(
    data.frame(
      scientificName = "Clupea pallasii",
      stratum = 1,
      transect = 57:62),
    data.frame(
      scientificName = "Clupea pallasii",
      stratum = 2,
      transect = 63:81),
    data.frame(
      scientificName = "Clupea pallasii",
      stratum = 3,
      transect = 82:87),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 1,
      transect = 1:9),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 2,
      transect = 10:53),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 3,
      transect = 54:62),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 4,
      transect = 63:81),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 5,
      transect = 82:86),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 1,
      transect = 1:7),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 2,
      transect = 9:14),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 3,
      transect = 23:32),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 4,
      transect = 57:62),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 5,
      transect = 63:81),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 6,
      transect = 82:87),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 1,
      transect = 2:5),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 4,
      transect = 58:62),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 5,
      transect = 63:66),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 6,
      transect = 70:75),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 1,
      transect = 1:9),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 2,
      transect = 11:53),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 3,
      transect = 54:62),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 4,
      transect = 63:81),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 5,
      transect = 82:87))
} else {
  strata.manual <- bind_rows( 
    # If not using Saildrone
    data.frame(
      scientificName = "Clupea pallasii",
      stratum = 1,
      transect = 32:45),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 1,
      transect = 1:31),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 2,
      transect = 32:44),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 1,
      transect = 1:7),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 2,
      transect = 9:12),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 3,
      transect = 16:22),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 4,
      transect = 32:45),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 1,
      transect = 2:5),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 2,
      transect = 35:38),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 1,
      transect = 1:31),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 2,
      transect = 35:45))
}

# Stock boundaries --------------------------------------------------------
stock.break.anch <- c("Cape Mendocino" = 40.80) # Latitude of Cape Mendocino
stock.break.sar  <- c("Pt. Conception" = 34.7) # Latitude of Pt. Conception, based on habitat model

# Transects used to define stock boundaries (primary or other)

# Used in estimateOffshore and estimateNearshore, where stock break using offshore transect ends is ambiguous
stock.break.source <- "primary" 

# Data collection settings ------------------------------------------------
# ER60 file info
raw.prefix    <- "2407RL_EK80"
raw.size      <- 2  # file size in gigabytes (GB)
raw.log.range <- c(RL  = "500, 500, 500, 300, and 200",
                   LBC = "500, 500, 500, and 300",
                   LM  = "500, 500, 500, and 300")  # depth of ER60 logging (m)
raw.log.range.night <- c(RL = "100")  # depth of ER60 logging (m)

# Echoview settings
er60.version  <- "v2.4.3" # ER60 version
ek80.version  <- "v23.6.2" # EK80 version
ev.version    <- "v14.0" # Echoview version
int.start        <-    5  # Integration start line depth (m)
int.stop         <-  350  # Integration start line depth (m)
cps.depth        <-   70  # Integration depth for CPS (m)
krill.depth      <-  350  # Integration depth for krill (m)
hake.depth       <-  750  # integration depth for hake (m)
speed.filter     <-    5  # Speed filter threshold (kn)
vmr.krill        <-  -45  # VMR value for krill formula operator (dB)
vmr.cps          <-  -45  # VMR value for CPS (with swim bladders) formula operator (dB)
bin.depth        <-    5  # Integration bin depth (m)
bin.length       <-  100  # Integration bin width (m)
adz.range        <-    3  # Range (m) of acoustic dead zone
nasc.freq        <-   38  # Echosounder frequency used to estimate CPS biomass

# Adaptive sampling information ------------------------------------------
compulsory.spacing      <- 20  # minimum transect spacing (nmi) for compulsory acoustic transects
adaptive.spacing        <- 10  # minimum transect spacing (nmi) for adaptive acoustic transects
adaptive.cluster.size   <- 5   # minimum number of consecutive transects to define a cluster
cufes.threshold.anchovy <- 1   # egg density, eggs per minute
cufes.threshold.sardine <- 0.3 # egg density, eggs per minute

# # Calibration information ------------------------------------------------
cal.vessels        <- c("RL", "LBC", "LM") # ,"LBC","LM"
cal.vessels.fm     <- c("RL") 
# Named vector of EK80 CW-mode calibration directories
cal.dir            <- c(RL  = "//swc-storage4-s/AST4/SURVEYS/20240625_LASKER_SummerCPS/DATA/EK80/CALIBRATION/RESULTS/Final-CW", 
                        LBC = "//swc-storage4-s/AST4/SURVEYS/20240625_CARNAGE_SummerCPS/DATA/EK80/CALIBRATION/RESULTS",
                        LM  = "//swc-storage4-s/AST4/SURVEYS/20240625_LISA-MARIE_SummerCPS/DATA/EK80/CALIBRATION/POST-SURVEY/RESULTS") 
# Location of Lasker (or primary vessel) calibration single-target detections (for polar plots)
single.targets.dir <- c(RL = "//swc-storage4-s/AST4/SURVEYS/20240625_LASKER_SummerCPS/DATA/EK80/CALIBRATION/EV_PROCESSING/CSV/singleTargets",
                        LM = "//swc-storage4-s/AST4/SURVEYS/20240625_LISA-MARIE_SummerCPS/DATA/EK80/CALIBRATION/POST-SURVEY/EV/SingleTargets",
                        LBC = "//swc-storage4-s/AST4/SURVEYS/20240625_CARNAGE_SummerCPS/DATA/EK80/CALIBRATION/EV/CSV")
sphere.TS <- list(RL = list("18" = -42.41, "38" = -42.40, "70" = -41.64, "120" = -39.80, "200" = -38.82, "333" = -36.78),
                  LM = list("38" = -42.36, "70" = -41.40, "120" = -39.72, "200" = -41.45),
                  LBC = list("38" = -42.41, "70" = -41.62, "120" = -39.74, "200" = -38.84))
# Named vector of EK80 FM-mode calibration directories
cal.dir.fm         <- c(RL  = "//swc-storage4-s/AST4/SURVEYS/20240625_LASKER_SummerCPS/DATA/EK80/CALIBRATION/RESULTS/Final-FM") 
# Named vector of EK80 CW-mode calibration dates
cal.datetime       <- c(RL  = "20 June", # Date/time of calibration
                        LBC = "16 May",
                        LM  = "4 June")
# Named vector of calibration dates, used to plot calibration time series
cal.plot.date      <- c(RL  = "2024-07-02",
                        LBC = "2024-07-02",
                        LM  = "2024-07-02") 
# Number of days around calibration date to look for results
cal.window         <- 400 
# Group and personnel conducting the calibrations
cal.group          <- c(RL  = "SWFSC",
                        LBC = "SWFSC",
                        LM  = "SWFSC")      
cal.personnel      <- c(RL  = "A. Beittel, D. Murfin, J. Renfree, and S. Sessions",
                        LBC = "A. Beittel, D. Murfin, J. Renfree, and S. Sessions",
                        LM  = "A. Beittel, D. Murfin, J. Renfree, and S. Sessions")
# Calibration location name, lat/long
cal.loc            <- c(RL  = "10th Avenue Marine Terminal, San Diego Bay",
                        LBC = "SWFSC Technology Development Tank",
                        LM  = "Gray's Harbor, WA") 
cal.lat.dd         <- c(RL  = 32.6936,
                        LBC = 32.8701,
                        LM  = 46.8885)    # Cal location latitude in decimal degrees (for mapping, e.g. with ggmap) 37.7865°N @ Pier 30-32
cal.lon.dd         <- c(RL  = -117.1503,
                        LBC = -117.2505,
                        LM  = -124.1319)   # Cal location longitude in decimal degrees (for mapping, e.g. with ggmap) -122.3844°W @ Pier 30-32
cal.lat            <- dd2decmin(cal.lat.dd)
cal.lon            <- dd2decmin(cal.lon.dd)
# Named vector of sphere type, name, and nominal depth below the transducer
cal.sphere         <- c(RL  = "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1)",
                        LBC = "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1)",
                        LM  = "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1)") # Cal sphere info
cal.sphere.name    <- c(RL  = "_Lasker_ sphere #1",
                        LBC = "_Lasker_ sphere #1",
                        LM  = "_Lasker_ sphere #1")
cal.sphere.z       <- c(RL  = 6,
                        LBC = 6,
                        LM  = 6) 
# Info about impedance analyzer
cal.imp.anal       <- c(RL  = "Agilent 4294A Precision Impedance Analyzer",
                        LBC = "Agilent 4294A Precision Impedance Analyzer",
                        LM  = "Agilent 4294A Precision Impedance Analyzer") 
# Other notes about calibration
cal.notes          <- c(RL  = "Lasker calibration sphere #1",
                        LBC = "Lasker calibration sphere #1",
                        LM  = "Lasker calibration sphere #1")

# Physical conditions during calibration
cal.temp           <-   c(RL = 20.16,
                          LM = 12.3)  # enter water temperature at sphere depth
cal.sal            <-   c(RL = 34.11,
                          LM = 26.9)  # enter salinity at sphere depth
cal.c              <- c(RL = 1520.8)   # enter sound speed (m/s)
cal.min.z          <-    c(RL = 6)     # enter minimum water depth below transducers
cal.max.z          <-   c(RL = 10)     # enter maximum water depth below transducers

# Enter ambient noise estimates (dB re 1 W) for each vessel
# Lowest to highest frequency
cal.noise <- list(RL  = NA,
                  LM  = NA,
                  LBC = NA)

# RMS error values from Echoview processing
cal.rms <- list(RL  = c(0.1018, 0.1057, 0.1384, 0.1128, 0.1945, 0.4646),
                LM  = c(0.1380, 0.1359, 0.1667, 0.3739),
                LBC = c(0.0967, 0.0875, 0.1295, 0.2589))

# Axis options for calibration plots
cal.scales    <- "free"  # fixed or free

# Vessel echosounder info  ------------------------------------------------
echo.freqs      <- c(SH  = "18, 38, 70, 120, and 200",
                     RL  = "18, 38, 70, 120, 200, and 333",
                     LBC = "38, 70, 120, and 200",
                     LM  = "38, 70, 120, and 200",
                     SD  = "38 and 200") # list of echosounder frequencies for Shimada
echo.freqs.dash <- c(SH  = "18-, 38-, 70-, 120-, and 200-",
                     RL  = "18-, 38-, 70-, 120-, 200-, and 333-",
                     LBC = "38-, 70-, 120-, and 200-",
                     LM  = "38-, 70-, 120-, and 200-",
                     SD  = "38- and 200-") # list of echosounder frequencies for Shimada
echo.models     <- c(SH  = "ES18-11, ES38B, ES70-7C, ES120-7C, and ES200-7C",
                     RL  = "ES18, ES38, ES70-7C, ES120-7C, ES200-7C, and ES333-7C",
                     LBC = "ES38-12, ES70-7C, ES120-7C and ES200-7C",
                     LM  = "ES38-7, ES70-7C, ES120-7C and ES200-7C",
                     SD  = "ES38-18|200-18C") # list of echosounder models for Shimada

# nominal centerboard positions
cb.retracted    <- 5
cb.intermediate <- 7
cb.extended     <- 9

# Set min and max values for temperature, salinity, etc.
# Temperature limits
min.T <- 0
max.T <- 25

# Salinity limits
min.S <- 30
max.S <- 35

# Set limits for latitude and longitude ----------------------------------------
min.lat  <-   25
max.lat  <-   52
min.long <- -132
max.long <- -112

# Files to manually exclude
exclude.uctd <- c(NA)
exclude.ctd  <- c(NA)

# SCS values and structure
# vessel echosounder info
tx.start.button    <- "Start Transect"
tx.end.button      <- "Break Transect"
ctd.button         <- "CTD IN"
uctd.button        <- "UCTD Deployed"
trawl.start.button <- "Begin Fishing (EQ)"
trawl.end.button   <- "Haul Back"
bongo.button       <- "Bongo IN"
pairovet.button    <- "Pairovet IN"
cb.flush.button    <- "Centerboard Flush"
cb.int.button      <- "Centerboard Interm"
cb.ext.button      <- "Centerboard Extended"
gps.lat.hdr        <- "SAMOS-Lat-VALUE"
gps.lon.hdr        <- "SAMOS-Lon-VALUE"
gps.lat.moa        <- "GP170-Lat"
gps.lon.moa        <- "GP170-Lon"
sst.hdr            <- "SAMOS-TSGT-VALUE"
sog.hdr            <- "SAMOS-SOG-VALUE"
wind.dir.hdr       <- "SAMOS-TrueWind-Dir-VALUE"
wind.speed.hdr     <- "SAMOS-TrueWind-Spd-VALUE"
order.occ.hdr      <- "Order Occ"
notes.hdr          <- "Notes"
