# Processing controls ----------------------------------------------------
## Settings in this section control various behaviors and tasks used in the main data processing scripts
### Biomass estimation
process.seine     <- F # Process purse seine data, if present
process.nearshore <- F # Process near backscatter data; typically TRUE
estimate.ns       <- F # Estimate biomass in the nearshore strata; T if nearshore surveyed
process.offshore  <- F # Process offshore backscatter data
estimate.os       <- F # Estimate biomass in the offshore strata; T if offshore surveyed
combine.regions   <- F # Combine nearshore/offshore plots with those from the core region

# Survey planning ---------------------------------------------------------
## This section controls and configures settings used by makeTransects and checkTransects for generating and checking survey transects
### Transect spacing (nautical miles)
tx.spacing.fsv  <- 10 # For Lasker 
tx.spacing.sd   <- tx.spacing.fsv/2 # For Saildrone
tx.spacing.ns   <- tx.spacing.fsv/2 # For nearshore sampling

# Mainland buffer distance for FSV and Saildrone transects
sd.buffer  <- 6 # nmi
fsv.buffer <- 80

# Minimum transect length
min.tx.length <- 0 # nmi

# UCTD spacing (nautical miles)
uctd.spacing   <- 15

### Transect removal and renumbering
rm.n.transects     <- 37 # Number of transects to remove from the start (if near Mexico)
rm.i.transects     <- NA # Remove specific transects from plan; else NA (for 2007RL: c(paste(90:117, "Nearshore")))
renumber.transects <- FALSE # Renumber transects to start at zero if transect are removed

# Locations to remove from planning (e.g., north, central, south, and mexico)
rm.location <- NA # c("mexico")

# Randomize
do.random <- FALSE
save.csv  <- TRUE
show.maps <- TRUE

## Used by processTransects.R -----------
# GPX file location
gpx.dir          <- "//swc-storage4-s.nmfs.local/AST4/SURVEYS/20230703_LASKER_SummerCPS/PLANNING/Rose Point/GPX"
gpx.file         <- "rosepoint_waypoints_20230531.gpx" #"rosepoint_waypoints.gpx"

# Define transit and survey speed (kn) for estimating progress
survey.speed     <- 9.5
transit.speed    <- 10
survey.direction <- "Northward" # Southward or Northward; to compute day lengths

# Beginning transit length (d)
transit.distance <- 320 # From SD to Punta Eugenia in 2023
transit.duration <- ceiling(transit.distance/transit.speed/24)

# Leg waste (d) due to transit, late departures, and early arrivals
leg.waste <- c(3, 2, 6)

# Remove transects to adjust survey progress
transects.rm <- NA # Numbered transects to remove

# Compute leg durations and breaks ----------------------------------------
# Define leg ends
leg.ends <- c(ymd("2023-07-03"), ymd("2023-07-30"),
              ymd("2023-08-05"), ymd("2023-08-31"),
              ymd("2023-09-05"), ymd("2023-09-30"))

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
survey.name.long       <- "Summer 2023 California Current Ecosystem Survey (CCES)"
survey.vessel.long     <- "Reuben Lasker" # Full vessel name: e.g., Bell M. Shimada
survey.vessel          <- "Lasker"        # Short vessel name; e.g., Shimada
survey.vessel.primary  <- "RL"            # Primary vessel abbreviation 
survey.name            <- "2307RL"        # SWFSC/AST survey name
survey.start           <- "17 July"       # Survey start date
survey.end             <- "3 November"    # Survey end date
survey.year            <- "2023"          # Survey year, for report
survey.season          <- "Summer"        # Survey season, for report
survey.das             <- 81              # Days at sea allocated
survey.landmark.n      <- "Cape Scott, BC" # Landmark - N extent of survey
survey.landmark.s      <- "Punta Baja, Baja CA" # Landmark - S extent of survey
survey.twilight        <- "none"          # Sunset type for computing day/night (none, nautical, civil, astronomical)
survey.twilight.offset <- 30              # Twilight offset; minutes before sunrise/after sunset
survey.twilight.remove <- FALSE           # Remove twilight period (T/F)
daynight.filter        <- c("Day","Night")# A character string including "Day", "Night", or both

# Inport dates for classifying data by cruise leg (if desired) -----------------
# Use start dates of each leg + end date of last leg
leg.breaks <- as.numeric(lubridate::ymd(c("2023-07-15", "2023-08-01", 
                                          "2023-09-01", "2023-10-01",
                                          "2023-11-04")))

# Define ERDDAP data variables for primary NOAA vessel
erddap.url           <- "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip"
erddap.vessel        <- "WTEGnrt"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.survey.start  <- "2023-07-17" # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2023-10-01" # End of survey for ERDDAP vessel data query
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed,windDirection,windSpeed,flag")
erddap.classes       <- c("character", "numeric", "numeric", "numeric","numeric","numeric","numeric","character")
erddap.headers       <- c("time", "lat","long","SST","SOG","wind_dir","wind_speed","flag")
erddap.flags         <- c('"ZZZZ.Z.Z..Z.*"')
survey.lat           <- c(27,51)
survey.long          <- c(-130,-113)

# Inport dates for classifying data by cruise leg (if desired) -----------------
# Use start dates of each leg + end date of last leg
leg.breaks.sh <- as.numeric(lubridate::ymd(c("2023-10-08", "2023-10-19",
                                             "2023-11-04")))

# Define ERDDAP data variables for Shimada
erddap.vessel.sh        <- "WTEDnrt"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.survey.start.sh  <- "2023-10-09" # Start of survey for ERDDAP vessel data query
erddap.survey.end.sh    <- "2023-11-04" # End of survey for ERDDAP vessel data query
erddap.flags.sh         <- c('"ZZZZ.Z.Z..Z.*"')

# Survey plan info --------------------------------------------------------
wpt.filename         <- "waypoints_2307RL.csv"
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
survey.start.sd  <- NA_character_ # Start of Saildrone survey
survey.end.sd    <- NA_character_ # End of Saildrone survey

# Set date range
erddap.url.sd <- "https://data.pmel.noaa.gov/pmel/erddap/tabledap/all_swfsc_2023"
erddap.survey.start.sd <- "2023-07-08T00%3A00%3A00Z"
erddap.survey.end.sd   <- "2023-10-15T23%3A59%3A00Z"
# Configure columns and classes
erddap.vars.sd       <- c("trajectory,latitude,longitude,SOG,time")
erddap.headers.sd    <- c("saildrone", "lat", "long", "SOG", "time")
erddap.classes.sd    <- c(rep("numeric", length(erddap.headers.sd) - 1),"character")

# Define date range for each Saildrone to remove overlapping transits
sd.date.range    <- data.frame(saildrone  = c(1048, 1060,1096),
                               start.date = ymd(c("2023-07-08", "2023-07-08", "2023-07-08")),
                               end.date   = ymd(c("2023-10-15", "2023-10-15", "2023-10-15")))

# Adjust time in Saildrone gps.csv files, if problems with Mission Planner (e.g., 1907RL)
sd.time.offset   <- 0 # Hours to add/subtract from GPS data (typically 0)

# Filter variables for TRAWL and CUFES data on SQL Server ----------------------
cruise.name <- 202307 # May be a numeric or numeric vector (e.g., c(201704,201706,...))
cruise.ship <- "RL"   # May be a character or character vector (e.g., c("RL",",...))

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
map.label.size <- c(central = 1, mexico = 2, north = 2, south = 2)

# Leaflet tile options; set both to T if caching
useCachedTile  <- F # Use cached tiles
useCrossOrigin <- F # Use cross origin
leaflet.checkTransects.simple <- TRUE # Use a simple Leaflet for checkTransects

# Trawl proportion plots
scale.pies <- FALSE   # Scale pie charts (TRUE/FALSE)
pie.scale  <- 0.0125 # 0.01-0.02 works well for coast-wide survey (i.e., summer), larger values (~0.03) for spring

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
stratum.ns    <- 3

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

# NASC
# For legend objects
nasc.breaks        <- c(0, 1, 200, 500, 2000, 5000, 20000, 50000, 20000000)
nasc.labels        <- c("0","1-200", "200-500", "500-2000", "2000-5000", 
                        "5000-20,000", "20,000-50,000", ">50,000")
nasc.scale         <- 0.55 # Scale percentage (smaller for larger scale)
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
nasc.vessels           <- c("RL", "LBC", "LM") #c("RL","LBC","LM","SD") 
nasc.vessels.offshore  <- NA # c("SD")
nasc.vessels.nearshore <- c("LBC","LM")
nasc.vessels.krill     <- c("RL")

# Define columns to use for a fixed integration depth (if cps.nasc is not present)
# Options include 0-100 (by 5), 100, 150, 250, and 350 m.
# Defined by the atm::extract_csv() function.
nasc.depth.cps   <- "NASC.250"
nasc.depth.krill <- "NASC.350"

# Purse seine data info
# Survey vessels that collected purse seine data
seine.vessels          <- c("LBC","LM")
# Use seine data to apportion backscatter
use.seine.data         <- FALSE

# Combine data from all vessels?
# Should data from different vessels be combined, e.g., for Lasker and Saildrone
# in the same strata?
merge.vessels <- c(Core = FALSE,
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
sounder.type           <- c(RL  = "EK80") 

# Location of survey data on AST1, AST2, etc. (a vector of file paths)
# Root directory where survey data are stored; other paths relative to this
if (Sys.info()['nodename'] %in% c("SWC-FRD-AST1-D","SWC-KSTIERH1-L")) {
  survey.dir           <- c(RL  = "C:/SURVEY/2307RL",
                            LBC = "//swc-storage4-s/AST4/SURVEYS/20230708_CARNAGE_SummerCPS",
                            LM  = "//swc-storage4-s/AST4/SURVEYS/20230703_LISA-MARIE_SummerCPS",
                            SD  = "//swc-storage4-s/AST4/SURVEYS/20230703_SAILDRONE_SummerCPS")   
} else {
  survey.dir           <- c(RL  = "//swc-storage4-s/AST4/SURVEYS/20230703_LASKER_SummerCPS",
                            LBC = "//swc-storage4-s/AST4/SURVEYS/20230708_CARNAGE_SummerCPS",
                            LM  = "//swc-storage4-s/AST4/SURVEYS/20230703_LISA-MARIE_SummerCPS",
                            SD  = "//swc-storage4-s/AST4/SURVEYS/20230703_SAILDRONE_SummerCPS")   
}

# Backscatter data (within survey.dir, typically)
nasc.dir               <- c(RL  = "PROCESSED/EV/CSV/LASKER",
                            LM  = "PROCESSED/EV/CSV",
                            LBC = "PROCESSED/EV/CSV",
                            SD  = "PROCESSED/EV/CSV") 

# Regex pattern for identifying CPS CSV files
nasc.pattern.cps       <- c(RL  = "Final 38 kHz CPS_nasc_cps.csv",
                            LM  = "Final 38 kHz CPS_nasc_cps.csv",
                            LBC = "Final 38 kHz CPS_nasc_cps.csv",
                            SD  = "Final 38 kHz CPS.csv")
# Regex pattern for identifying krill CSV files
nasc.pattern.krill     <- c(RL  = "KRILL-Juan Krill Final 120.csv",
                            LM  = "Krill-Juan Krill Final 120.csv",
                            LBC = "Krill-Juan Krill Final 120.csv",
                            SD  = "Krill-Juan Krill Final 120.csv")
# Regex pattern for identifying nearshore transects
nasc.pattern.nearshore <- c(RL  = "\\d{3}N",
                            LM  = "\\d{3}N",
                            LBC = "\\d{3}N",
                            SD  = "\\d{3}N")
# Regex pattern for identifying offshore transects
nasc.pattern.offshore  <- c(RL  = "\\d{3}O",
                            LM  = "\\d{3}O",
                            LBC = "\\d{3}O",
                            SD  = "\\d{3}O")
# Regex pattern for identifying offshore transects
nasc.pattern.inshore   <- c(RL  = "\\d{3}I",
                            LM  = "\\d{3}I",
                            LBC = "\\d{3}I",
                            SD  = "\\d{3}I")
# Regex pattern for identifying transits
nasc.pattern.transit   <- c(RL  = "\\d{3}T",
                            LM  = "\\d{3}T",
                            LBC = "\\d{3}T",
                            SD  = "\\d{3}T")
# Recursively search NASC directories
nasc.recurse           <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = TRUE)
# Max NASC value for removing outliers
nasc.max               <- NA

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
                            NS  = FALSE) # in the nearshore strata

# File containing CPS nasc from CTD app
data.cps.nasc          <- c(RL  = here("Data/Backscatter/nasc_cps_RL_2307RL.csv")) # in the nearshore strata 

# regex for matching character pattern
tx.char.pattern        <- c(RL  = "[^0-9]",
                            LM  = "[^0-9]",
                            LBC = "[^0-9]") 

# If T, strips numbers from transect names (i.e., would combine 105-1 and 105-2 to 105)
strip.tx.nums          <- c(RL  = TRUE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = TRUE) 

# If T, strips characters from transect numbers (i.e., would combine 105A and 105B to 105)
strip.tx.chars         <- c(RL  = TRUE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE) 

# If T, removes transects with names including "transit"
rm.transit             <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE)  

# If T, removes transects with names including "offshore"
rm.offshore            <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE) 

# If T, removes transects with names including "inshore"
rm.inshore             <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE)

# If T, removes transects with names including "nearshore"
rm.nearshore           <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE)

# If T, extracts nearshore intervals from vessels that sample close to shore
extract.nearshore      <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE)

# If T, subtracts NASC.5 from cps.nasc
rm.surface             <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE,
                            SD  = FALSE) 

# regex for matching number pattern
tx.num.pattern         <- c(RL  = "-\\d{1}",
                            LM  = "-\\d{1}",
                            LBC = "-\\d{1}",
                            SD  = "-\\d{1}")

# Use transect names for transect numbers
use.tx.number          <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE,
                            SD  = TRUE)

# Transects to manually exclude e.g., data.frame(vessel = "RL", transect = c("085","085-2"))
# Transects 018-031 in 2107RL occurred in Mexico, and were removed from this analysis, but
# but will ultimately be included in a joint analysis
tx.rm                  <- list(RL  = NA,
                               LM  = NA,
                               LBC = NA,
                               SD  = NA)

# Minimum acoustic transect length (nmi)
min.tx.length          <- c(RL  = 15,
                            LM  = 1,
                            LBC = 1,
                            SD  = 1)

# Enforce nearest trawl cluster distance limits?
limit.cluster.dist     <- c(OS  = FALSE,
                            NS  = FALSE) 

# Define source of species proportions and length frequency data (either clf or hlf)
# Uses either haul or cluster data for a given region (NS or OS)
cluster.source <- c(OS = "cluster",
                    NS = "haul")

# Manually exclude hauls from the analysis
# List hauls (e.g., c(1, 2...n)), else NA
haul.rm <- NA

# Maximum distance to trawl clusters
cum.biomass.limit      <- 0.90 # Distance used to compute max.cluster.distance

# If limit.cluster.dist == TRUE, set proportions to zero at distances greater than max.cluster.dist
max.cluster.dist       <- 30

# Define transect spacing bins and values (nmi) used to characterize transect spacing
tx.spacing.bins <- c(0,  6, 15, 35, 70, 100)
tx.spacing.dist <- c(5, 10, 20, 40, 80)

tx.spacing.ns   <-  5 # Nearshore transect spacing, in nmi; set NA if calculating programatically
tx.spacing.os   <- 40 # Nearshore transect spacing, in nmi; set NA if calculating programatically

# SCS data
scs.source             <- "XLSX" # "CSV", "ELG", or "XLSX"
scs.pattern            <- "MOA*.*xlsx" # regex for MOA files

# SCS data info for extracting NAV data
scs.nav.path           <- "C:/SURVEY/2307RL/DATA/SCS" # Local
scs.nav.dir            <- "SAMOS"
scs.nav.pattern        <- "SAMOS-OBS.*.elg"
scs.nav.recurse        <- TRUE

# CUFES data
cufes.source           <- "SQLite" # "SQL" or "SQLite"
cufes.dir.sqlite       <- file.path(survey.dir[survey.vessel.primary], "DATA/BIOLOGICAL/CUFES")
cufes.db.sqlite        <- "cufes202307RL.sqlite" # CUFES SQLite database
cufes.date.format      <- "mdy" # mdy (1907RL and later) or ymd (earlier surveys)
cufes.vessels          <- c("RL")

# Trawl data
trawl.source           <- "Access" # "SQL" or "Access"
trawl.dsn              <- "TRAWL"  # DSN for Trawl database on SQL server
trawl.dir.access       <- file.path(survey.dir, "DATA/BIOLOGICAL/HAUL")
trawl.db.access        <- "TrawlDataEntry2307RL.accdb"
trawl.performance      <- c("Aborted", "Poor") # Character vector; trawl performance to exclude
trawl.haul.exclude     <- NA # Numeric vector; haul numbers to exclude (e.g., for incomplete catch, etc.; NA if include all)

# CTD data
ctd.dir                <- file.path(survey.dir[survey.vessel.primary],"DATA/CTD")
ctd.hdr.pattern        <- "RL2203*.*hdr"
ctd.cast.pattern       <- ".*_processed.asc"
ctd.depth              <- 350

# UCTD data   
uctd.dir               <- file.path(survey.dir[survey.vessel.primary],"DATA/UCTD/PROCESSED")
uctd.hdr.pattern       <- ".*UCTD\\d{3}-\\d{1}.asc"
uctd.cast.pattern      <- ".*_processed.asc"

# TDR data
tdr.dir.kite           <- here("Data/TDR/Kite")
tdr.dir.foot           <- here("Data/TDR/Footrope")
tdr.pattern            <- "2307RL*.*rsk"
tdr.recurse            <- TRUE # Recursively search TDR directory
tdr.tz                 <- "America/Los_Angeles" # Time zone setting for TDRs
# Time offset, in hours (usually -1, diff between PDT and PST in summer)
tdr.offset             <- rep(0, 100) 
# tdr.offset             <- c(rep(-1, 72), # Time settings were updated after haul 73
#                             rep( 0, 20)) # Time settings were updated after haul 73
tdr.offset             <- setNames(tdr.offset, 1:length(tdr.offset)) # Add names from haul numbers
tdr.nav.source         <- "ERDDAP"
tdr.trawl.source       <- "Access"

# Biomass estimation settings ------------------------------------------
# Length bins and labels for calculating length frequencies 
length.min <- 1 # Minimum length bin for length frequencies
# (max. anchovy = 20 cm, sardine & herrings = 30 cm, Pac. mack = 40, and jack mack. = 60)
length.max <- data.frame("species" = c("Clupea pallasii","Engraulis mordax",
                                       "Sardinops sagax", "Scomber japonicus",
                                       "Trachurus symmetricus","Etrumeus acuminatus"),
                         "sl" = c(30,20,30,40,60,30))

# Species to generate point estimates
point.est.spp          <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")
# Species to generate point estimates
bootstrap.est.spp      <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")

# Number of bootstrap samples
boot.num <- 5 # 1000 during final

# Generate biomass length frequencies
do.lf    <- TRUE

# Define regions to present in main Results
estimate.regions   <- c("Core") # c("Core", "Nearshore")

# Define rules for selecting and pruning sampling strata -----------------------
# Defines breaks between strata
max.diff <- 3
# Defines minimum number of transects in a stratum
nTx.min <- 2

# Stratum pruning settings
nIndiv.min    <- 1
nClusters.min <- 1

# Use manually defined strata?
stratify.manually    <- TRUE
stratify.manually.os <- FALSE
stratify.manually.ns <- FALSE

# Manually define sampling strata for each species
# Create a new data frame with each species, stratum, and vector containing transects
strata.manual <- bind_rows(
  data.frame(
    scientificName = "Clupea pallasii", 
    stratum = 1,
    transect = 32:37),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 1,
    transect = 1:31),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 2,
    transect = 32:37),
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
    transect = 32:37),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 1,
    transect = 2:5),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 2,
    transect = 34:37),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 1,
    transect = 1:31),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 2,
    transect = 34:37))

# Stock boundaries --------------------------------------------------------
stock.break.anch <- 40.50  # Latitude of Cape Mendocino
# stock.break.sar  <- 37.674 # Latitude of San Francisco, based on differences in length dist.
stock.break.sar  <- 34.3 # Latitude of Pt. Conception (or change based on SST)

# Transects used to define stock boundaries (primary or other)

# Used in estimateOffshore, where stock break using offshore transect ends is ambiguous
stock.break.source <- "primary" 

# Data collection settings ------------------------------------------------
# ER60 file info
raw.prefix    <- "2307RL_EK80"
raw.size      <-  50   # file size in megabytes (MB)
raw.log.range <- 350  # depth of ER60 logging (m)

# Echoview settings
er60.version  <- "v2.4.3" # ER60 version
ek80.version  <- "v1.12.2" # EK80 version
ev.version    <- "v12.1" # Echoview version
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
cal.vessels        <- c("RL","LBC","LM")
cal.dir            <- "//swc-storage4-s/AST4/SURVEYS/SURVEYS/20230703_LASKER_SummerCPS/DATA/EK80/CALIBRATION/RESULTS"
cal.datetime       <- "26 June"     # Date/time of calibration
cal.plot.date      <- "2023-06-23" # Date of the calibration, used to plot cal time series
cal.window         <- 50           # Number of days around calibration date to look for results
cal.group          <- "SWFSC"      # Group conducting the calibration
cal.personnel      <- "J. Renfree, D. Murfin, and S. Sessions" # Calibration participants
cal.loc            <- "10th Avenue Marine Terminal, San Diego Bay" # Location name
cal.lat.dd         <-   32.6956    # Cal location latitude in decimal degrees (for mapping, e.g. with ggmap) 37.7865°N @ Pier 30-32
cal.lon.dd         <- -117.15278   # Cal location longitude in decimal degrees (for mapping, e.g. with ggmap) -122.3844°W @ Pier 30-32
cal.lat            <- dd2decmin(cal.lat.dd)
cal.lon            <- dd2decmin(cal.lon.dd)
cal.sphere         <- "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1; _Lasker_ sphere #1)" # Cal sphere info
cal.sphere.name    <- "_Lasker_ sphere #1"
cal.sphere.z       <- 6 # Nominal depth of calibration sphere below the transducer
cal.imp.anal       <- "Agilent 4294A Precision Impedance Analyzer" # Info about impedance analyzer
# Other notes about calibration
cal.notes          <- "Lasker calibration sphere #1"

# Physical conditions during calibration
cal.temp           <-   23.42   # enter water temperature at sphere depth
cal.sal            <-   35.0   # enter salinity at sphere depth
cal.c              <- 1530.6   # enter sound speed (m/s)
cal.min.z          <-    5     # enter minimum water depth below transducers
cal.max.z          <-   10     # enter maximum water depth below transducers
 
# Enter ambient noise estimates (dB re 1 W) for each vessel
# Lowest to highest frequency
cal.noise          <- list(RL  = NA,
                           LM  = NA,
                           LBC = NA)

# Axis options for calibration plots
cal.scales    <- "free"  # fixed or free

# Vessel echosounder info  ------------------------------------------------
if (survey.vessel.primary == "SH") {
  echo.freqs      <- "18, 38, 70, 120, and 200" # list of echosounder frequencies for Shimada
  echo.freqs.dash <- "18-, 38-, 70-, 120-, and 200-" # list of echosounder frequencies for Shimada
  echo.models     <- "ES18-11, ES38B, ES70-7C, ES120-7C, and ES200-7C" # list of echosounder models for Shimada
} else if (survey.vessel.primary == "RL") {
  echo.freqs      <- "18, 38, 70, 120, 200, and 333" # list of echosounder frequencies for Lasker
  echo.freqs.dash <- "18-, 38-, 70-, 120-, 200-, and 333-" # list of echosounder frequencies for Lasker
  echo.models     <- "ES18-11, ES38B, ES70-7C, ES120-7C, ES200-7C, and ES333-7C" # list of echosounder models for Shimada
}

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
min.lat  <-   31
max.lat  <-   52
min.long <- -132
max.long <- -117

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
cb.flush.button    <- "CB Flush"
cb.int.button      <- "CB Interm"
cb.ext.button      <- "CB Extended"
gps.lat.hdr        <- "SAMOS-Lat-VALUE"
gps.lon.hdr        <- "SAMOS-Lon-VALUE"
sst.hdr            <- "SAMOS-TSGT-VALUE"
sog.hdr            <- "SAMOS-SOG-VALUE"
wind.dir.hdr       <- "SAMOS-TrueWind-Dir-VALUE"
wind.speed.hdr     <- "SAMOS-TrueWind-Spd-VALUE"
order.occ.hdr      <- "Order Occ"
notes.hdr          <- "Notes"
