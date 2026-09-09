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
tx.spacing.fsv  <- 12.5 # For Lasker 
tx.spacing.sd   <- 15 # For Saildrone
tx.break.ns     <- 52 # Northernmost transect sampled by the southern F/V, 64 in 2024, near Carmel
tx.spacing.ns   <- 7  # c("S" = 7, "N" = 7, "CI" = 2.5) # or NA
tx.spacing.os   <- 40 # Nearshore transect spacing, in nmi; set NA if calculating programatically

# Mainland buffer distance for FSV and Saildrone transects
sd.buffer  <- 7 # nmi
fsv.buffer <- 80 #limits offshore portion of lines (SCB)

# Minimum transect length
min.tx.length <- 0 # nmi

# eDNA, CTD, and UCTD station preferences
ctd.tx.range   <- seq(15, 100) # Range of transects to include eDNA stations
edna.spacing   <- 10 # Surface eDNA sample spacing (nmi)
edna.tx.range  <- seq(1, 15) # Range of transects to include eDNA stations
uctd.spacing   <- 15 # UCTD spacing (nmi)
uctd.tx.range  <- seq(1, 100) # Range of transects to include eDNA stations

### Transect removal and renumbering
rm.n.transects     <- 56 # Number of transects to remove from the start (if near Mexico); if none, use zero; 71@10 nmi spacing
rm.n.transects.ns  <- 138 # Number of transects to remove from the start (if near Mexico); if none, use zero
rm.n.transects.sd  <- 47 # Number of transects to remove from the start (if near Mexico); if none, use zero
rm.i.transects     <- NA # Remove specific transects from plan; else NA (for 2007RL: c(paste(90:117, "Nearshore")))
# Renumber transects to start at zero if transect are removed
renumber.transects <- c("Compulsory" = TRUE, 
                        "Nearshore" = FALSE) 

# Locations to remove from planning (e.g., north, central, or south)
rm.location <- c("north") # c("south")

# Randomize
do.random <- TRUE
save.csv  <- TRUE
show.maps <- TRUE

## Used by processTransects.R -----------
### GPX file location
gpx.dir          <- here("Data/Nav")
gpx.file         <- "2606RL-12.5-nmi-spacing_final.gpx" # "2606RL-hybrid-spacing.gpx" "2606RL-12.5-nmi-spacing.gpx"

# Define transit and survey speed (kn) for estimating progress
survey.speed     <- 9 # FSV
survey.speed.ns  <- 7 # Nearshore vessels
transit.speed    <- 9
survey.direction <- "Northward" # Southward or Northward; to compute day lengths

# Beginning transit length (d)
transit.distance <- 0 # beginning in San Diego 2026
transit.duration <- ceiling(transit.distance / transit.speed / 24)

# Leg waste (d) due to transit, late departures, and early arrivals
leg.waste <- c(1, 2, 2, 2, 2)
wx.days   <- c(2, 2, 2, 2, 2)

# Time required for daytime trawling and CTD casts
day.trawl.duration <- 0 # duration of daytime trawls (h)
day.trawl.waste    <- c(0.5, 2.3, 2.3, 2.3, 2.3)*day.trawl.duration
day.ctd.waste      <- c(0, 0, 0, 0, 0)

# Remove transects to adjust survey progress
transects.rm <- NA # Numbered transects to remove

# Compute leg durations and breaks ----------------------------------------
# Define leg ends
leg.ends <- c(ymd("2026-06-17"), ymd("2026-07-02"),
              ymd("2026-07-06"), ymd("2026-07-21"),
              ymd("2026-07-26"), ymd("2026-08-10"),
              ymd("2026-08-14"), ymd("2026-08-29"),
              ymd("2026-09-03"), ymd("2026-09-22"))

# Compute days per leg
leg.days <- (leg.ends[seq(2, length(leg.ends), 2)] - leg.ends[seq(1,length(leg.ends) - 1, 2)]) + 1

# Calculate total days at sea (DAS)
total.das <- sum(leg.days)

# Leg durations used to split transects
n.survey.legs <- 4 # Number of legs available for survey planning (not counting research)
leg.length <- c(leg.days - leg.waste - wx.days)

# Leg breaks
leg.breaks.gpx <- c(0, cumsum(as.numeric(leg.length)))

# Region vector used to break transects for waypoint files
region.vec <- c(0, 32.5353, 34.7, 41.99, 48.490, 55)

## Used by formatCoastalExplorerNotebook.R ------
### Coastal (.nob)X file location
nob.dir          <- here("Data/Nav")
nob.file         <- "2606RL-12.5-nmi-spacing_20260421.nob"
nob.file.final   <- "2606RL-12.5-nmi-spacing_final.nob"

### Waypoint preferences
rangeCircleRadius <- c(ctd = "1 NM", uctd = "1 NM", eDNA = "1 NM")
rangeCircleCount  <- c(ctd = "1", uctd = "1", eDNA = "1")
rangeCircleFill   <- c(ctd = "true", uctd = "true", eDNA = "true")
rangeCircleColor  <- c(ctd = "rgb(0, 0, 1)", uctd = "rgb(1, 0.4901961, 0)", eDNA = "rgb(0, 1, 0)")
waypointIcon      <- c(ctd = "Blue Box", uctd = "White Box", eDNA = "Green Box")

## Used by estimateAcousticKm.Rmd ------
# Get nearshore vessels
nav.vessels.ns <- c("LM","LBC")

# Survey information ------------------------------------------------------
# Full survey name; only used in report title
survey.name.long       <- "Summer 2026 Integrated West Coast Pelagics Survey (IWCPS)"
survey.vessel.long     <- "Reuben Lasker" # Full vessel name: e.g., Bell M. Shimada
survey.vessel          <- "Lasker"       # Short vessel name; e.g., Shimada
survey.vessel.primary  <- "RL"            # Primary vessel abbreviation 
survey.name            <- "2606RL"        # SWFSC/AST survey name
survey.start           <- "17 June"       # Survey start date
survey.end             <- "30 September"  # Survey end date
survey.year            <- "2026"          # Survey year, for report
survey.season          <- "Summer"        # Survey season, for report
survey.das             <- 90             # Days at sea allocated
survey.landmark.n      <- "Cape Flattery, WA" # Landmark - N extent of survey
survey.landmark.s      <- "San Diego, CA" # Landmark - S extent of survey
survey.twilight        <- "none"          # Sunset type for computing day/night (none, nautical, civil, astronomical)
survey.twilight.offset <- 30              # Twilight offset; minutes before sunrise/after sunset
survey.twilight.remove <- FALSE           # Remove twilight period (T/F)
daynight.filter        <- c("Day","Night")# A character string including "Day", "Night", or both
safe.depth.fsv         <- 30              # Shallowest depth (m) sampled by the FSV

# Inport dates for classifying data by cruise leg (if desired) -----------------
# Use start dates of each leg + end date of last leg
leg.breaks <- as.numeric(lubridate::ymd(c("2026-06-16", "2026-07-04", 
                                          "2026-07-23", "2026-08-12", 
                                          "2026-09-02", "2026-09-25")))

# Anticipated progress throughout the transect plan
# Leg 1:1-14, Leg 2:15-43, Leg 3:44-68, Leg 4: 69-86
tx.breaks <- c(0, 22, 42, 64, 88)

# Define nav source depending on location of computer
## Options are: SCS (usually on the ship) or ERDDAP (usually on shore; 24h update rate)
if (Sys.info()['nodename'] %in% c("SWC-KSTIERHOF-L")) { #("SWC-FRD-AST1-D",
  nav.source    <- "RosePoint" # SCS, ERDDAP, or RosePoint
  nav.path.nob  <- "C:/SURVEY/2606RL/DATA/NAV/2606RL-12.5-nmi-spacing_final.nob" 
  nav.source.ns <- "GPX"
} else {
  nav.source    <- "RosePoint" # SCS, ERDDAP, or RosePoint
  nav.path.nob  <- "C:/SURVEY/2606RL/DATA/NAV/2606RL-12.5-nmi-spacing_final.nob" 
  nav.source.ns <- "GPX"
}

# Define hakeML directory depending on location of computer
if (Sys.info()['nodename'] %in% c("SWC-KSTIERHOF-L")) { #("SWC-FRD-AST1-D",
  hakeML.dir    <- "C:/SURVEY/2606RL/DATA/EK80" 
} else {
  hakeML.dir    <- "//10.15.115.10/data_import/Cruise Data/RL2604 IWCPS/transect_info"
}

# Define ERDDAP data variables for primary NOAA vessel
erddap.url           <- "http://coastwatch.pfeg.noaa.gov/erddap/tabledap/fsuNoaaShip"
erddap.vessel        <- "WTEGnrt"    # Lasker == WTEG; Shimada == WTED; add "nrt" if survey in progress
erddap.survey.start  <- "2026-06-10" # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2026-09-24" # End of survey for ERDDAP vessel data query
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed,windDirection,windSpeed,flag")
erddap.classes       <- c("character", "numeric", "numeric", "numeric","numeric","numeric","numeric","character")
erddap.headers       <- c("time", "lat","long","SST","SOG","wind_dir","wind_speed","flag")
erddap.flags         <- c('"ZZZZ.Z.Z..Z.*"')
survey.lat           <- c(27,51)
survey.long          <- c(-130,-113)

# Survey plan info --------------------------------------------------------
wpt.filename  <- "waypoints_2606RL.csv"
wpt.types     <- c(Adaptive = "Adaptive", Carranza = "Carranza",
                   Compulsory = "Compulsory", Franklin = "Franklin",
                   Nearshore = "Nearshore", Offshore = "Offshore", 
                   Saildrone = "Saildrone")
wpt.regions   <- c("Central CA", "S. CA Bight", "WA/OR") # "Vancouver Is."          
wpt.colors    <- c(Adaptive = "#FF0000", Carranza = "green",
                   Compulsory = "#000000", Franklin = "blue",
                   Nearshore = "#FF33F5", Offshore = "#FFA500", 
                   Saildrone = "#00FFFF") 
wpt.linetypes <- c(Adaptive = "dashed", Carranza = "solid",
                   Compulsory = "solid", Franklin = "solid", 
                   Nearshore = "solid", Offshore = "dashed", 
                   Saildrone = "dashed")

# Filter variables for TRAWL and CUFES data on SQL Server ----------------------
cruise.name <- 202606 # May be a numeric or numeric vector (e.g., c(201704,201706,...))
cruise.ship <- c("RL") # May be a character or character vector (e.g., c("RL",",...))

# Growth model parameters ------------------------------------------------------
model.season  <- "summer" # spring or summer; for selecting growth model parameters
model.type    <- "glm"    # lm, nlm, or glm; for selecting growth model

# Mapping preferences -----------------------------------------------------
# Turn off S2 processing in sf
sf::sf_use_s2(FALSE)
mapviewOptions(basemaps = c("Esri.OceanBasemap","Esri.WorldGrayCanvas","Esri.WorldImagery"))

# Coordinate reference systems for geographic and projected data
crs.geog <- 4326 # WGS84
crs.proj <- 3310 # California Albers Equal Area

# Default map height
map.height <- 10
# Map height for specific regions; used in makeTransects, checkTransects
map.height.region <- c(central = 12, mexico = 7, north = 7, south = 10)
map.label.size <- c(central = 1.5, mexico = 4, north = 2, south = 4)

# Leaflet tile options; set both to T if caching
useCachedTile  <- F # Use cached tiles
useCrossOrigin <- T # Use cross origin
leaflet.checkTransects.simple <- TRUE # Use a simple Leaflet for checkTransects

# Trawl proportion plots
scale.pies <- TRUE   # Scale pie charts (TRUE/FALSE)
pie.scale  <- 0.0125 # 0.01-0.02 works well for coast-wide survey (i.e., summer), larger values (~0.03) for spring

# Lookup table for renaming columns
pie.spp <- c("Jacksmelt"  = "Atherinopsis californiensis", "PacHerring" = "Clupea pallasii",
             "Anchovy"    = "Engraulis mordax", "Sardine"    = "Sardinops sagax", 
             "JapSardine" = "Sardinops melanosticta", "PacMack" = "Scomber japonicus", 
             "JackMack"   = "Trachurus symmetricus", "Smelt" = "Allosmerus elongatus",
             "RndHerring" = "Etrumeus acuminatus", "AllCPS" = "AllCPS")

# Map landmarks
label.list <- c("Monterey","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg","Coos Bay",
                "Morro Bay","Long Beach","Cape Scott","San Diego",
                "Ensenada","Punta Eugenia","El Rosario","Cabo San Lucas",
                "Punta Abreojos","San Carlos", "Santa Barbara", "Point Arena")

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
jacksmelt.color    <- '#FFFF00' 
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'
rnd.herring.color  <- '#F0B81D'
jap.sardine.color  <- '#F0A6A6'
smelt.color        <- '#A020F0'
hake.color         <- '#563D2D'
other.color        <- 'gray'

# Set gear type colors
seine.color <- "white"
trawl.color <- "black"

# Define species to be analysed -------------------
## CPS species
cps.spp            <- c("Clupea pallasii","Engraulis mordax","Sardinops melanosticta",
                        "Sardinops sagax", "Scomber japonicus","Trachurus symmetricus", 
                        "Etrumeus acuminatus","Allosmerus elongatus")

## Estimate biomass of Japanese sardine?
estimate.j.sardine <- FALSE

## trouts & salmon, Pacific salmon unid, pink, chum, coho, sockeye, Chinook, steelhead, cutthroat trout
salmon.spp <- c(161931, 161974, 161975, 161976, 161977, 161979, 161980, 161989, 161983) 

## Define length types per species
spp.sl <- c("Sardinops sagax", "Sardinops melanosticta", "Engraulis mordax")
spp.fl <- c("Scomber japonicus", "Trachurus symmetricus", "Clupea pallasii", 
            "Etrumeus acuminatus", "Merluccius productus","Allosmerus elongatus")
spp.ml <- c("Doryteuthis opalescens")

# CUFES -------------------------------------------------------
cufes.start        <- NA # Start of survey for CUFES filtering
cufes.end          <- NA # End of survey for CUFES filtering
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

# Trawl -----------------------------------------------------------------------
# For legend objects
trawl.breaks       <- c(0, 1, 10, 25, 50, 500, 1000, 10000) 
trawl.labels       <- c("<1", "1-10", "10-25", "25-50", "50-500", "500-1000", ">1000") 
trawl.sizes        <- c(1, 2, 3, 4, 5, 6, 7) 

# For pie charts; subsetted using pie.spp, which is defined from the catch data
# Species columns
pie.cols <- c("Engraulis mordax" = "Anchovy", "Trachurus symmetricus" = "JackMack", 
              "Atherinopsis californiensis" = "Jacksmelt", "Sardinops melanosticta" = "JapSardine",
              "Clupea pallasii" = "PacHerring", "Scomber japonicus" = "PacMack", 
              "Etrumeus acuminatus" = "RndHerring","Sardinops sagax" = "Sardine",
              "Allosmerus elongatus" = "Smelt")

pie.cols.prop <- c("Engraulis mordax" = "prop.anch", "Trachurus symmetricus" = "prop.jack", 
                   "Atherinopsis californiensis" = "prop.jsmelt", "Clupea pallasii" = "prop.her", 
                   "Scomber japonicus" = "prop.mack", "Etrumeus acuminatus" = "prop.rher", 
                   "Sardinops sagax" = "prop.sar", "Sardinops melanosticta" = "prop.sar.jap",
                   "Allosmerus elongatus" = "prop.smelt")

# Species labels
pie.labs <- c("Engraulis mordax" = "Anchovy", "Trachurus symmetricus" = "J. Mackerel", 
              "Atherinopsis californiensis" = "Jacksmelt", "Sardinops melanosticta" = "J. Sardine",
              "Clupea pallasii" = "P. herring", "Scomber japonicus" = "P. mackerel", 
              "Etrumeus acuminatus" = "R. herring", "Sardinops sagax" = "Sardine",
              "Allosmerus elongatus" = "Smelt")

# Species colors
pie.colors <- c("Engraulis mordax" = anchovy.color, "Trachurus symmetricus" = jack.mack.color, 
                "Atherinopsis californiensis" = jacksmelt.color, "Sardinops melanosticta" = jap.sardine.color,
                "Clupea pallasii" = pac.herring.color, "Scomber japonicus" = pac.mack.color, 
                "Etrumeus acuminatus" = rnd.herring.color, "Sardinops sagax" = sardine.color,
                "Allosmerus elongatus" = smelt.color)

# NASC ------------------------------------------------------------------
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

# Raster settings ------------------------------------------------------------------
# Sardine habitat
sar.hab.breaks       <- c(0, 0.18, 0.29, 1) 
sar.hab.colors       <- c("black", "#595959", "#CCCCCC")
sar.hab.labels       <- c("0", "0.18", "0.29", "1") 

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
nasc.vessels           <- c("RL") #c("RL","LBC","LM","SD") 
nasc.vessels.offshore  <- NA # c("SD")
nasc.vessels.nearshore <- c("LBC", "LM")
nasc.vessels.krill     <- c("RL")

# Define columns to use for a fixed integration depth (if cps.nasc is not present)
# Options include 0-100 (by 5), 100, 150, 250, and 350 m.
# Defined by the atm::extract_csv() function.
nasc.depth.deep  <- "NASC.20" # "NASC.70"
nasc.depth.cps   <- "NASC.250"
nasc.depth.krill <- "NASC.350"

# Combine data from all vessels?
# Should data from different vessels be combined, e.g., for Lasker and Saildrone
# in the same strata? Or, in 2023, Lasker and Shimada when additional sea days were provided
merge.vessels <- c(Core = FALSE, # To combine RL and SH in 2407RL; perhaps SD too
                   OS   = FALSE,
                   NS   = FALSE)

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
if (Sys.info()['nodename'] %in% c("SWC-FRD-AST1-D")) {
  survey.dir           <- c(RL  = "C:/SURVEY/2606RL",
                            LBC = "G:/Shared drives/NMFS SWFSC FRD AST/Surveys/2026 Summer IWCPS (2606LBC)",
                            LM  = "C:/SURVEY/2606LM")
} else {
  survey.dir           <- c(RL  = "C:/SURVEY/2606RL",
                            LBC = "G:/Shared drives/NMFS SWFSC FRD AST/Surveys/2026 Summer IWCPS (2606LBC)",
                            LM  = "C:/SURVEY/2606LM")
}

# Backscatter data (within survey.dir, typically)
nasc.dir               <- c(RL  = "PROCESSED/EV/CSV",
                            LM  = "PROCESSED/EV/CSV",
                            LBC = "PROCESSED/EV/CSV") 

# Regexp pattern for identifying CPS CSV files
nasc.pattern.cps       <- c(RL  = "_CPS-Final 38 kHz CPS_nasc_cps.csv",
                            LM  = "Final 38 kHz CPS_nasc_cps.csv",
                            LBC = "Final 38 kHz CPS.csv")

# Regex pattern for identifying krill CSV files
nasc.pattern.krill     <- c(RL  = "Poly Krill Final 120.csv",
                            LM  = "Juan Krill Final 120.csv",
                            LBC = "Juan Krill Final 120.csv")

# Regex pattern for identifying nearshore transects
nasc.pattern.nearshore <- c(RL  = "\\d{3}N",
                            LM  = "\\d{3}N",
                            LBC = "\\d{3}N")

# Regex pattern for identifying offshore transects
nasc.pattern.offshore  <- c(RL  = "\\d{3}O",
                            LM  = "\\d{3}O",
                            LBC = "\\d{3}O")

# Regex pattern for identifying inshore transits between transects
nasc.pattern.inshore   <- c(RL  = "\\d{3}I",
                            LM  = "\\d{3}I",
                            LBC = "\\d{3}I")

# Regex pattern for identifying transits
nasc.pattern.transit   <- c(RL  = "\\d{3}T",
                            LM  = "\\d{3}T",
                            LBC = "\\d{3}T")
# Recursively search NASC directories
nasc.recurse           <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE)

# Max NASC value for removing outliers
nasc.max               <- NA
Sv.max                 <- NULL # Max Sv value (dB); Set to -14 after testing is completed

# Purse seine data info -------------------------------------------------------
# Use seine data to apportion nearshore backscatter
# If seine catches were believed to be representative, TRUE
# Else, FALSE (e.g., if sets were non-random or otherwise believed to be biased)
use.seine.data  <- TRUE
seine.source    <- "Access"
seine.dir       <- "DATA/BIOLOGICAL/SEINE"
seine.db.name   <- "SeineDataEntry2606RL.accdb"
seine.xlsx.name <- "Nearshore_LBC_2606RL.xlsx"
seine.tz        <- "America/Los_Angeles"
seine.types     <- c("survey", "research", NA)
seine.gpx.name  <- "lbc_nav.gpx" # "nav_nearshore.gpx"

# Survey vessels that collected purse seine data
seine.vessels          <- c("LBC" ,"LM")
seine.vessels.long     <- c("LBC" = "Long Beach Carnage","LM"  = "Lisa Marie")

# Deep backscatter correction
# Correct deep backscatter?
adj.deep.nasc <- FALSE
# Remove deep backscatter, if a correction is not applied? Set to FALSE if adj.deep.nasc = TRUE
rm.deep.nasc <- TRUE
# Vessels for which to remove deep backscatter that may be anchovy
deep.nasc.vessels <- c("LBC", "LM")

# Which net data should be used to apportion nearshore backscatter?
# "Trawl" and/or "Seine"
catch.source.ns <- c("Trawl", "Purse seine")

# Define path to seine data directories for each vessel
seine.data.paths <- c("LBC"= file.path(survey.dir["LBC"], "DATA/SEINE/lbc_data_2606RL.xlsx"),
                      "LM" = file.path(survey.dir["LM"],  "DATA/SEINE/lm_data_2606RL.xlsx"))

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
                            NS  = FALSE) # in the nearshore strata

# File containing CPS nasc from CTD app
data.cps.nasc          <- c(RL  = here("Data/Backscatter/nasc_cps_RL_2606RL.csv")) # in the nearshore strata 

# regex for matching character pattern
tx.char.pattern        <- c(RL  = "[^0-9]",
                            LM  = "[^0-9]",
                            LBC = "[^0-9]") 

# If T, strips numbers from transect names (i.e., would combine 105-1 and 105-2 to 105)
strip.tx.nums          <- c(RL  = TRUE,
                            LM  = FALSE,
                            LBC = FALSE) 

# If T, strips characters from transect numbers (i.e., would combine 105A and 105B to 105)
strip.tx.chars         <- c(RL  = TRUE,
                            LM  = FALSE,
                            LBC = FALSE) 

# If T, removes transects with names including "transit"
rm.transit             <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE)  

# If T, removes transects with names including "offshore"
rm.offshore            <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE) 

# If T, removes transects with names including "inshore"
rm.inshore             <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE)

# If T, removes transects with names including "nearshore"
rm.nearshore           <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE)

# If T, extracts nearshore intervals from vessels that sample close to shore
extract.nearshore      <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE)

# If T, subtracts NASC.5 from cps.nasc
rm.surface             <- c(RL  = FALSE,
                            LM  = FALSE,
                            LBC = FALSE) 

# regex for matching number pattern
tx.num.pattern         <- c(RL  = "-\\d{1}",
                            LM  = "-\\d{1}",
                            LBC = "-\\d{1}")

# Use transect names for transect numbers
use.tx.number          <- c(RL  = TRUE,
                            LM  = TRUE,
                            LBC = TRUE)

# Transects to manually exclude e.g., data.frame(vessel = "RL", transect = c("085","085-2"))
# Transects 018-031 in 2107RL occurred in Mexico, and were removed from this analysis, but
# but will ultimately be included in a joint analysis
tx.rm                  <- list(RL  = NA,
                               LM  = NA,
                               LBC = NA)

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

# Maximum distance to trawl clusters
cum.biomass.limit <- 0.90 # Distance used to compute max.cluster.distance

# If limit.cluster.dist == TRUE, set proportions to zero at distances greater than max.cluster.dist
max.cluster.dist <- 30

# Define transect spacing bins and values (nmi) used to characterize transect spacing
tx.spacing.bins <- c(0,  6, 25, 35, 70, 100)
tx.spacing.dist <- c(5, 12.5, 30, 40, 80)

# tx.spacing.bins <- c(0,  6, 15, 35, 70, 100)
# tx.spacing.dist <- c(5, 12.5, 20, 40, 80)

# Define transect buffering preferences for stratum polygon creation
na.buffer.dist <- 4 # Distance (nmi) to buffer N. American land mask for core strata masking
ci.buffer.dist <- 2.5 # Distance (nmi) to buffer Channel Island land mask for nearshore strata masking
ci.clip.dist   <- 0.1 # Distance (nmi) to buffer Channel Island land mask for nearshore strata clipping

## Core area
tx.ext.pct  <- 0.35   # Extension percentage constant
tx.ext.dir  <- "east" # east (toward shore), west (away from shore), or both
tx.buff.pct <- 1.05  # Scaling factor for transect buffering, 1.025 in 2025

## Nearshore area
tx.ext.pct.ns    <- 2.25      # Extension percentage constant
tx.ext.pct.ns.ci <- 0.25   # Extension percentage scaling for Channel Island transects
tx.ext.dir.ns    <- "both" # east (toward shore), west (away from shore), or both
tx.buff.pct.ns   <- 1.1   # Scaling factor for transect buffering

# SCS data
scs.source             <- "ELG" # "CSV", "ELG", or "XLSX"
scs.pattern            <- "MOA*.*xlsx" # regex for MOA files

# SCS data info for extracting NAV data
scs.nav.script         <- "get_nav_scs5.r"
scs.nav.path           <- "C:/SURVEY/2606RL/DATA/SCS" # Local
scs.nav.dir            <- "GPS - Science GP170"
scs.nav.pattern        <- "RAW.log"
scs.gga.pattern        <- "GPGGA.RAW.log"
scs.vtg.pattern        <- "GPVTG.RAW.log"
scs.nav.recurse        <- TRUE
scs.nav.seconds        <- c(1, 30) # Seconds to retain in high-res nav data, e.g., c(1, 30) will retain two points per minute

# CUFES data
cufes.source           <- "SQLite" # "SQL" or "SQLite"
cufes.dir.sqlite       <- file.path(survey.dir[survey.vessel.primary], "DATA/BIOLOGICAL/CUFES")
cufes.db.sqlite        <- "cufes202506SH.sqlite" # CUFES SQLite database
cufes.date.format      <- "mdy" # mdy (1907RL and later) or ymd (earlier surveys)
cufes.vessels          <- c("RL")

# Trawl data
trawl.source           <- "SQL"  # "SQL" or "Access" or "CLAMS-Oracle" or "CLAMS-SQLite" or "CLAMS-Postgres"
clams.db.settings      <- "CLAMS_db_info-2606RL.R"
trawl.dsn              <- "TRAWL"  # DSN for Trawl database on SQL server
trawl.db.name          <- "TrawlDataEntry2606RL.db"
trawl.db.ext           <- ".db"
# Time zone for events in trawl database ("America/Los_Angeles" in past; "UTC" for CLAMS)
trawl.db.tz            <- "UTC" 
trawl.performance      <- c("Aborted") # Character vector; trawl performance to exclude
trawl.haul.rm          <- NA # c(24) # Numeric vector; haul numbers to exclude (e.g., for incomplete catch, etc.; NA if include all)

# Location of trawl database
if (Sys.info()['nodename'] %in% c("SWC-FRD-AST1-D","SWC-KSTIERHOF-L")) {
  trawl.dir <- "DATA/BIOLOGICAL/HAUL"
} else if (Sys.info()['nodename'] %in% c("RL4433188-CHL1")) {
  trawl.dir <- ""
} else {
  trawl.dir <- "DATA/BIOLOGICAL/HAUL"
}

# CTD data
ctd.dir                <- file.path(survey.dir[survey.vessel.primary],"DATA/CTD/PROCESSED")
ctd.hdr.dir            <- file.path(survey.dir[survey.vessel.primary],"DATA/CTD")
ctd.hdr.pattern        <- "*.*hdr"
ctd.cast.pattern       <- ".*_processed.asc"
ctd.cast.depth         <- 350

# UCTD data   
uctd.dir               <- file.path(survey.dir[survey.vessel.primary],"DATA/MVP")
uctd.type              <- "MVP" # "Valeport" or "Oceansciences" or "MVP"
uctd.hdr.pattern       <- "rl2604.*.m1"
uctd.cast.pattern      <- "rl2604.*.m1"
uctd.cast.depth        <- 250
# Column names for cast files
uctd.col.names         <- list(vp2 = c("date","time","Z","P","T","C","S","Sv","Dens","ChlA","Ticks"),
                               m1  = c("P","Z","Sv","T","C","S","Dens","LOPC","ANLG0","ANLG1"))
# Number of lines to skip when reading txt file
uctd.skip              <- c(vp2 = 79, 
                            m1  = 62)
# Vector of file names for bad casts
uctd.cast.rm <- c("rl2604_20260619_0010.m1",
                  "rl2604_20260623_0041.m1",
                  "rl2604_20260625_0055.m1",
                  "rl2604_20260630_0082.m1",
                  "rl2604_20260701_0091.m1",
                  "rl2604_20260805_0380.m1",
                  "rl2604_20260824_0432.m1",
                  "rl2604_20260824_0434.m1",
                  "rl2604_20260824_0435.m1")
# Amount of seconds to remove from UCTD casts (e.g., MVP at surface)
uctd.surface.rm        <- 20 

# RBR TDR data
tdr.dir.kite           <- here("Data/TDR/Kite")
tdr.dir.foot           <- here("Data/TDR/Footrope")
tdr.pattern            <- "2606RL\\d{3}\\w{1,2}.rsk$"
tdr.pattern.cruise     <- "^\\d{4}\\w{2}"
tdr.recurse            <- TRUE # Recursively search TDR directory
# Time zone setting for TDRs
tdr.tz.kite            <- c(rep("UTC", 200)) 
tdr.tz.kite <- setNames(tdr.tz.kite, 1:length(tdr.tz.kite))
tdr.tz.foot            <- c(rep("UTC", 200)) 
tdr.tz.foot <- setNames(tdr.tz.foot, 1:length(tdr.tz.foot))
# Time offset, in hours (e.g., -1, diff between PDT and PST in summer)
## Kite
### Define offsets
tdr.offset.k <- c(rep(0, 200)) 
### Add names from haul numbers
tdr.offset.k <- setNames(tdr.offset.k, 1:length(tdr.offset.k))
## Footrope
### Define offsets
tdr.offset.f <- c(rep(0, 200)) 
### Add names from haul numbers
tdr.offset.f <- setNames(tdr.offset.f, 1:length(tdr.offset.f)) 
# Data info
tdr.nav.source         <- "ERDDAP"
tdr.trawl.source       <- "Access"
tdr.cruise             <- c("202606") # Cruise name(s) for TDR files

# Seabird TDR data
tdr.offset.asc <- 0

# TV80 data
tv80.tz     <- "UTC"
tv80.offset <- 0 # Offset, in hours
# TV80 column names
tv80.cols   <- stringr::str_split("UnixTimeSeconds;DateTime;VES_Heading;VES_Latitude;VES_Longitude;VES_Heading;VES_Course_True;VES_Speed;VES_Speed;VES_Course_True;TWL_Depth;TWL_Depth;VES_Temperature;DOR_Battery_Std;DOR_Pitch_Std;DOR_Roll_Std;DOR_Depth_Std;DOR_Pitch_Prt;DOR_Depth_Prt;DOR_Battery_Std;DOR_Temperature_Std;DOR_Roll_Prt;DOR_Depth_Dif_c;DOR_Battery_Prt;DOR_Spread",
                                ";")[[1]]
# Number of rows to skip when reading TV80 data files
tv80.skip   <- 1 

# Biomass estimation settings ------------------------------------------
# Length bins and labels for calculating length frequencies 
length.min <- 1 # Minimum length bin for length frequencies
# (max. anchovy = 20 cm, sardine & herrings = 30 cm, Pac. mack = 40, and jack mack. = 60)
length.max <- data.frame("species" = c("Clupea pallasii", "Engraulis mordax",
                                       "Sardinops sagax", "Scomber japonicus",
                                       "Trachurus symmetricus", "Etrumeus acuminatus",
                                       "Allosmerus elongatus"),
                         "sl" = c(30, 20, 30, 50, 60, 30, 30))

# Species to generate point estimates
point.est.spp          <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")
# Species to generate point estimates
bootstrap.est.spp      <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")

# Number of bootstrap samples
boot.num <- 50 # 1000 during final

# Generate biomass length frequencies
do.lf    <- TRUE

# Define regions to present in main Results
estimate.regions   <- c("Core") # "Nearshore"

# Define rules for selecting and pruning sampling strata -----------------------
# Defines breaks between strata
max.diff <- 3
# Defines minimum number of transects in a stratum
nTx.min <- 2

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

} else {
  strata.manual <- bind_rows( 
    # If not using Saildrone
    data.frame(
      scientificName = "Clupea pallasii",
      stratum = 1,
      transect = 30:39),
    data.frame(
      scientificName = "Clupea pallasii",
      stratum = 2,
      transect = 40:53),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 1,
      transect = 1:26),
    data.frame(
      scientificName = "Engraulis mordax",
      stratum = 2,
      transect = 43:49),
    data.frame(
      scientificName = "Etrumeus acuminatus",
      stratum = 1,
      transect = 1:4),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 1,
      transect = 1:4),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 2,
      transect = 11:15),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 3,
      transect = 16:20),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 4,
      transect = 22:24),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 5,
      transect = 25:29),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 6,
      transect = 32:36),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 7,
      transect = 40:45),
    data.frame(
      scientificName = "Sardinops sagax",
      stratum = 8,
      transect = 50:53),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 1,
      transect = 1:4),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 2,
      transect = 16:19),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 3,
      transect = 23:25),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 4,
      transect = 41:46),
    data.frame(
      scientificName = "Scomber japonicus",
      stratum = 5,
      transect = 50:53),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 1,
      transect = 1:10),
    data.frame(
      scientificName = "Trachurus symmetricus",
      stratum = 2,
      transect = 15:53))
}

# Stock boundaries --------------------------------------------------------
stock.break.anch <- c("Cape Mendocino" = 40.96)  # Latitude of Cape Mendocino; 40.80 is actual Cape Mendocino
stock.break.sar  <- c("Pt. Conception" = 34.55) # Latitude of ~Pt. Conception, base off 2023 habitat map

# Transects used to define stock boundaries (primary or other)
# Used in estimateOffshore, where stock break using offshore transect ends is ambiguous
stock.break.source <- "primary" 

# Data collection settings ------------------------------------------------
# ER60 file info
raw.prefix    <- "2606RL_EK80"
raw.size      <-   1L  # file size in gigabytes (GB)
raw.log.range <- 350  # depth of ER60 logging (m)

# Echoview settings
er60.version  <- "v2.4.3" # ER60 version
ek80.version  <- "v21.15.1" # EK80 version
ev.version    <- "v14.1" # Echoview version
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
compulsory.spacing      <- 20   # minimum transect spacing (nmi) for compulsory acoustic transects
adaptive.spacing        <- 10   # minimum transect spacing (nmi) for adaptive acoustic transects
adaptive.cluster.size   <-  5   # minimum number of consecutive transects to define a cluster
cufes.threshold.anchovy <-  1   # egg density, eggs per minute
cufes.threshold.sardine <-  0.3 # egg density, eggs per minute

# # Calibration information ------------------------------------------------
cal.vessels        <- c("RL","LBC","LM") # NULL
cal.vessels.fm     <- NULL # c("RL") 
cal.dir            <- c(RL  = here("Data/Calibration/ECS/2606RL"),
                        LM  = here("Data/Calibration/ECS/2606RL"),
                        LBC = here("Data/Calibration/ECS/2606RL"))
# Location of Lasker (or primary vessel) calibration single-target detections (for polar plots)
single.targets.dir <- c(RL =  "//swc-storage4-s/AST4/SURVEYS/20250603_SHIMADA_IWCPS/DATA/EK80/CALIBRATION/EV_PROCESSING/CSV/singleTargets",
                        LM =  "//swc-storage4-s/AST4/SURVEYS/20250725_LISA-MARIE_SummerCPS/DATA/EK80/CALIBRATION/POST-SURVEY/EV/singleTargets",
                        LBC = "//swc-storage4-s/AST4/SURVEYS/20250617_CARNAGE_SummerCPS/DATA/EK80/CALIBRATION/EV/CSV")
# sphere.TS <- list(RL  = list("18" = -42.41, "38" = -42.40, "70" = -41.64, "120" = -39.80, "200" = -38.82, "333" = -36.78),
#                   LM  = list("38" = -42.36, "70" = -41.40, "120" = -39.72, "200" = -41.45),
#                   LBC = list("38" = -42.41, "70" = -41.62, "120" = -39.74, "200" = -38.84))
# Named vector of EK80 FM-mode calibration directories
cal.dir.fm         <- c(RL  = "//swc-storage4-s/AST4/SURVEYS/20250603_SHIMADA_IWCPS/DATA/EK80/CALIBRATION/RESULTS/Final-FM") 

cal.datetime       <- c(RL  = "15 June",
                        LBC = "9 April",
                        LM  = "23 July")    # Date/time of calibration
cal.plot.date      <- c(RL = "2026-06-15") # Date of the calibration, used to plot cal time series
cal.window         <- c(RL = 75)           # Number of days around calibration date to look for results
cal.group          <- c(RL = "SWFSC")      # Group conducting the calibration
cal.personnel      <- c(RL = "D. Murfin, J. Renfree, and S. Sessions") # Calibration participants
cal.loc            <- c(RL  = "10th Avenue Marine Terminal, San Diego Bay",
                        LBC = "SWFSC Technology Development Tank",
                        LM  = "Monterey Bay, near Santa Cruz") # Location name
cal.lat.dd         <- c(RL  = 32.6956, # Cal location latitude in decimal degrees
                        LBC = 32.8701, # SWFSC Tech Tank
                        LM  = 36.9592) # At anchor in Monterey Bay, near Santa Cruz    
cal.lon.dd         <- c(RL  = -117.15278, # Cal location longitude in decimal degrees
                        LBC = -117.2505, # SWFSC Tech Tank
                        LM  = -122.0120) # At anchor in Monterey Bay, near Santa Cruz
cal.lat            <- dd2decmin(cal.lat.dd)
cal.lon            <- dd2decmin(cal.lon.dd)
cal.sphere         <- c(RL  = "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1)",
                        LBC = "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1)",
                        LM  = "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material (WC38.1)") # Cal sphere info
cal.sphere.fm      <- c(RL = "25-mm WC sphere (WC25)") # Cal sphere info for additional FM calibrations
cal.sphere.name    <- c(RL  = "SWFSC Sphere #10",
                        LBC = "SWFSC Sphere #10",
                        LM  = "SWFSC Sphere #10")
cal.sphere.z       <- c(SH  = 6,
                        LBC = 6,
                        LM  = 6) # Nominal depth of calibration sphere below the transducer
cal.imp.anal       <- c(RL = "Agilent 4294A Precision Impedance Analyzer") # Info about impedance analyzer
# Other notes about calibration
cal.notes          <- c(RL = "Lasker calibration sphere #TBD")

# Physical conditions during calibration
## DEPRECATED; use values in calibration ECS files
# sphere.TS now extracted from ECS files (2025-present)
# cal.temp  <- c(RL  = 20.16,
#                LM  = NA,
#                LBC = NA) # enter water temperature at sphere depth
# cal.sal   <- c(RL  = 34.11,
#                LM  = NA,
#                LBC = NA) # enter salinity at sphere depth
# cal.c     <- c(RL  = 1520.8,
#                LM  = NA,
#                LBC = NA) # enter sound speed (m/s)
# cal.min.z <- c(RL  =  6,
#                LM  = NA,
#                LBC = NA) # enter minimum water depth below transducers
# cal.max.z <- c(RL  = 10,
#                LM  = NA,
#                LBC = NA) # enter maximum water depth below transducers
# cal.rms <- list(RL  = rep(NA_real_, 5), # c(0.1018, 0.1057, 0.1384, 0.1128, 0.1945, 0.4646),
#                 LM  = rep(NA_real_, 4), # c(0.1380, 0.1359, 0.1667, 0.3739),
#                 LBC = rep(NA_real_, 4)) # c(0.0967, 0.0875, 0.1295, 0.2589))
# # Enter ambient noise estimates (dB re 1 W) for each vessel
# # Lowest to highest frequency
# cal.noise          <- list(RL  = NA,
#                            LM  = NA,
#                            LBC = NA)

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
                     RL  = "ES18-11, ES38B, ES70-7C, ES120-7C, ES200-7C, and ES333-7C",
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
min.S <- 0
max.S <- 35

# Set limits for latitude and longitude ----------------------------------------
min.lat  <-   31
max.lat  <-   52
min.long <- -132
max.long <- -117

# Files to manually exclude
exclude.uctd <- c("rl2604_20260805_0380.m1")
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
gps.lat.hdr        <- "SciGPS-Lat"
gps.lon.hdr        <- "SciGPS-Lon"
gps.lat.moa        <- "SciGPS-Lat"
gps.lon.moa        <- "SciGPS-Lon"
sst.hdr            <- "TSG45-InternalTemp-C"
sog.hdr            <- "SciGPS-SOG"
cog.hdr            <- "SciGPS-COG"
wind.dir.hdr       <- "TrueWindDirection-StbdMast-DIRECTION"
wind.speed.hdr     <- "TrueWindSpeed-StbdMast-kts-SPEED"
order.occ.hdr      <- "Order Occ"
notes.hdr          <- "Notes"
