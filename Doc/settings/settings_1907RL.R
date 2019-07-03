# Survey information ------------------------------------------------------
# Full survey name; only used in report title
survey.name.long       <- "Summer 2019 California Current Ecosystem Survey"
survey.vessel.long     <- "Reuben Lasker" # Full vessel name: e.g., Bell M. Shimada
survey.vessel          <- "Lasker"        # Short vessel name; e.g., Shimada
survey.vessel.primary  <- "RL"            # Primary vessel abbreviation 
survey.name            <- "1907RL"        # SWFSC/AST survey name
survey.start           <- "13 June"       # Survey start date
survey.end             <- "9 September"   # Survey end date
survey.year            <- "2019"          # Survey year, for report
survey.season          <- "Summer"        # Survey season, for report
survey.das             <- 77              # Days at sea allocated
survey.landmark.n      <- "Cape Scott, British Columbia" # Landmark - N extent of survey
survey.landmark.s      <- "San Diego, CA" # Landmark - S extent of survey
survey.twilight        <- "none"          # Sunset type for computing day/night (none, nautical, civil, astronomical)
survey.twilight.offset <- 30              # Twilight offset; minutes before sunrise/after sunset
survey.twilight.remove <- FALSE           # Remove twilight period (T/F)
daynight.filter        <- c("Day","Night")# A character string including "Day", "Night", or both

# Inport dates for classifying data by cruise leg (if desired) -----------------
leg.breaks <- as.numeric(lubridate::ymd(c("2019-06-12", "2019-07-06", 
                                          "2019-07-29", "2019-08-20",
                                          "2019-09-10")))

# Define ERDDAP data variables
erddap.vessel        <- "WTEGnrt"    # Lasker == WTEG; Shimada == WTED; add "nrt" if during survey
erddap.survey.start  <- "2019-06-12" # Start of survey for ERDDAP vessel data query
erddap.survey.end    <- "2019-09-10" # End of survey for ERDDAP vessel data query
erddap.vars          <- c("time,latitude,longitude,seaTemperature,platformSpeed")
erddap.classes       <- c("factor", "numeric", "numeric", "numeric","numeric")
erddap.headers       <- c("time", "lat", "long", "SST", "SOG")
survey.lat           <- c(32,51)
survey.long          <- c(-130,-117)

# Survey plan info --------------------------------------------------------
wpt.filename         <- "waypoints_1907RL.csv"
wpt.types            <- c("Compulsory","Adaptive","Nearshore","Offshore")
wpt.colors           <- c("#FF0000", "#0000FF", "#EDEA37", "#FFA500") 

# Saildrone info -----------------------------------------------
# Select Saildrone numbers
sd.numbers <- c("1024")

# Set Saildrone filter method
sd.buffer.type   <- c("saildrone")
sd.buffer.dist   <- 1.5 # buffer distance (nmi) around planned transects to classify SD intervals
sd.filter.method <- "manual" # Options are c("buffer","manual")
sd.nasc.name     <- "cps_nasc_SD.csv"

# Define Saildrone sampling dates
survey.start.sd  <- "2019-06-13" # Start of Saildrone survey
survey.end.sd    <- "2019-09-09" # End of Saildrone survey

# Set date range
erddap.survey.start.sd <- "2019-06-13T00%3A00%3A00Z"
erddap.survey.end.sd   <- "2019-09-09T19%3A59%3A00Z"

# Configure columns and classes
erddap.vars.sd       <- c("trajectory,Clatitude,Clongitude")
erddap.headers.sd    <- c("saildrone", "lat", "long", "time")
# erddap.headers.sd    <- c("saildrone","lat","long","COG","HDG","time")
erddap.classes.sd    <- c(rep("numeric", length(erddap.headers.sd) - 1),"factor")
erddap.classes.sd    <- c(rep("numeric", length(erddap.headers.sd) - 1),"factor")

# Filter variables for TRAWL and CUFES data on SQL Server ----------------------
cruise.name <- 201907 # May be a numeric or numeric vector (e.g., c(201704,201706,...))
cruise.ship <- "RL"   # May be a character or character vector (e.g., c("RL","SH",...))

# Growth model parameters ------------------------------------------------------
model.season  <- "summer" # spring or summer; for selecting growth model parameters
model.type    <- "glm"    # lm, nlm, or glm; for selecting growth model

# Mapping preferences -----------------------------------------------------
mapviewOptions(basemaps = c("Esri.OceanBasemap","Esri.WorldImagery","CartoDB.Positron"))

# Coordinate reference systems for geographic and projected data
crs.geog <- 4326 # WGS84
crs.proj <- 3310 # Califoria Albers Equal Area

# Configure mapview options
mapviewOptions(basemaps = c("Esri.OceanBasemap","Esri.WorldImagery","CartoDB.Positron"))

# Leaflet tile options; set both to T if caching
useCachedTile  <- F # Use cached tiles
useCrossOrigin <- F # USe cross origin

# Trawl proportion plots
scale.pies <- FALSE   # Scale pie charts (TRUE/FALSE)
pie.scale  <- 0.0125 # 0.01-0.02 works well for coast-wide survey (i.e., summer), larger values (~0.03) for spring

# Map landmarks
label.list <- c("Monterey Bay","San Francisco","Cape Flattery","Crescent City",
                "Newport","Point Conception","Cape Mendocino","Columbia River",
                "Cape Blanco","Bodega Bay","Westport","Fort Bragg",
                "Morro Bay","Long Beach","Cape Scott","San Diego")

# Species, stock and strata for nearshore biomass plots -------------------
spp.common.ns <- "Northern Anchovy"
spp.ns        <- "Engraulis mordax"
stock.ns      <- "Northern"
strata.ns     <- 4

# Figure preferences ------------------------------------------------------
# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'

# Load cmocean colormaps
load(here("Code/cmocean.Rdata"))
ocean.pal <- colors

# Define species to be analysed
cps.spp            <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                        "Scomber japonicus","Trachurus symmetricus")
# CUFES
# For legend objects
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
nasc.breaks        <- c(0, 200, 500, 2000, 5000, 20000, 50000, 20000000)
nasc.labels        <- c("0-200", "200-500", "500-2000", "2000-5000", 
                        "5000-20,000", "20,000-50,000", ">50,000")
nasc.scale         <- 0.7 # Scale percentage (smaller for larger scale)
nasc.sizes         <- c(0.25, 2, 3, 4, 5, 6, 7)*nasc.scale
nasc.colors        <- c("#000000", "#1E90FF", "#FFFF00", "#FF8C00", 
                        "#FF0000", "#FFC0CB", "#FFFFFF")

# Catch map
# For legend objects
catch.breaks       <- c(0,10,100,500,1000)
catch.labels       <- c("0-10","10-100", "100-500", "500-1000")
catch.pie.sizes    <- c(1,2,3,4,5,6)

annotation.size <-  2.5    # Font size for annotations; try 4 for spring surveys, 2.5 for summer surveys

# Data sources ------------------------------------------------------------
# Backscatter data info
nasc.vessels           <- c("RL", "LM", "SD") # Survey vesslels that collected acoustic data (a vector of two letter vessel abbreviations)
nasc.interval          <-  100    # Interval length (m); from Echoview
nasc.summ.interval     <- 2000/nasc.interval # number of intervals over which to summarize NASC
# Location of survey data on AST1, AST2, etc. (a vector of file paths)
sounder.type           <- c(RL = "EK60",
                            LM = "EK60",
                            SD = "EK80") # Echosounder type; e.g., EK60, EK80, other
# Root directory where survey data are stored; other paths relative to this
if (Sys.info()['nodename'] == "SWC-KSTIERHOF-D") {
  survey.dir           <- c(RL = "C:/SURVEY/1907RL",
                            LM = "C:/SURVEY/1907RL",
                            SD = "C:/SURVEY/1907RL")   
} else {
  survey.dir           <- c(RL = "C:/SURVEY/1907RL",
                            LM = "C:/SURVEY/1907RL",
                            SD = "C:/SURVEY/1907RL")
}
nasc.dir               <- c(RL = "PROCESSED/EV/CSV/LASKER",
                            LM = "PROCESSED/EV/CSV/LISA_MARIE",
                            SD = "PROCESSED/EV/CSV/SAILDRONE") # Backscatter data (within survey.dir, typically; a vector of file paths)
nasc.pattern.cps       <- c(RL = "*Final 38 kHz CPS.csv",
                            LM = "*Final 38 kHz CPS.csv",
                            SD = "*CPS-Final CPS.csv")
nasc.pattern.krill     <- c(RL = "*Juan Krill Final 120.csv",
                            LM = "*Juan Krill Final 120.csv",
                            SD = "*Juan Krill Final 120.csv")
source.cps.nasc        <- c(RL = F,
                            LM = F,
                            SD = F) # If T, read cps.nasc from file; else use NASC.50 
data.cps.nasc          <- c(RL = here("Data/CPS_NASC/cps_nasc_1907RL.csv"),
                            LM = NA,
                            SD = NA) # File containing CPS nasc from CTD app
tx.char.pattern        <- c(RL = "[^0-9]",
                            LM = "[^0-9]",
                            SD = "[^0-9]") # regex for matching character pattern
strip.tx.nums          <- c(RL = F,
                            LM = F,
                            SD = F) # If T, strips numbers from transect names (i.e., would combine 105-1 and 105-2 to 105)
strip.tx.chars         <- c(RL = F,
                            LM = F,
                            SD = F) # If T, strips characters from transect numbers (i.e., would combine 105A and 105B to 105)
rm.transit             <- c(RL = T,
                            LM = F,
                            SD = F) # If T, removes transects with names including "transit"
rm.offshore            <- c(RL = T,
                            LM = F,
                            SD = F) # If T, removes transects with names including "offshore"
rm.nearshore           <- c(RL = T,
                            LM = F,
                            SD = F) # If T, removes transects with names including "nearshore"
rm.surface             <- c(RL = F,
                            LM = F,
                            SD = F) # If T, subtracts NASC.5 from cps.nasc
tx.num.pattern         <- c(RL = "-\\d{1}",
                            LM = "-\\d{1}",
                            SD = "-\\d{1}") # regex for matching number pattern
use.tx.number          <- c(RL = F,
                            LM = F,
                            SD = F) # Use transect names for transect numbers
tx.rm                  <- c(RL = NA,
                            SD = NA) # Transects to manually exclude e.g., data.frame(vessel = "RL", transect = c("085","085-2"))
min.tx.length          <- c(RL = 3,
                            LM = 1,
                            SD = 1)  # Minimum acoustic transect length (nmi)
# CUFES database
cufes.source           <- "SQLite" # "SQL" or "SQLite"
cufes.dir.sqlite       <- file.path(survey.dir[survey.vessel.primary], "DATA/BIOLOGICAL/CUFES")
cufes.db.sqlite        <- "cufes201907RL.sqlite" # CUFES SQLite database
cufes.date.format      <- "mdy" # mdy (1907RL only) or ymd (most other surveys)
# Trawl database
trawl.source           <- "Access" # "SQL" or "Access"
trawl.dsn              <- "TRAWL" # System DSN for Trawl database on SQL server
trawl.dir.access       <- file.path(survey.dir,"DATA/BIOLOGICAL/HAUL")
trawl.db.access        <- "TrawlDataEntry1907RL.accdb"
# CTD data
ctd.dir                <- unique(file.path(survey.dir,"DATA/CTD"))
ctd.hdr.pattern        <- "1907\\d{3}.hdr"
ctd.cast.pattern       <- ".*_processed.asc"
ctd.depth              <- 350
# UCTD data   
uctd.dir               <- unique(file.path(survey.dir,"DATA/UCTD"))
uctd.hdr.pattern       <- ".*UCTD\\d{3}.*asc"
uctd.cast.pattern      <- ".*_processed.asc"

# Biomass estimation settings ------------------------------------------
# Species to generate point estimates
point.est.spp          <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")
# Species to generate point estimates
bootstrap.est.spp      <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax",
                            "Scomber japonicus","Trachurus symmetricus")

# Number of bootstrap samples
boot.num <- 10 # 1000 during final

# Generate biomass length frequencies
do.lf    <- TRUE

# Estimate biomass in nearshore and offshore strata
estimate.nearshore <- FALSE
estimate.offshore  <- FALSE

# Stratum pruning settings
nIndiv.min    <- 10
nClusters.min <- 2

# Use manually defined strata?
stratify.manually <- FALSE

# Manually define sampling strata for each species
# Create a new data frame with each species, stratum, and vector containing transects
strata.manual <- bind_rows(
  data.frame(
    scientificName = "Clupea pallasii", 
    stratum = 1,
    transect = 60:64),
  data.frame(
    scientificName = "Clupea pallasii", 
    stratum = 2,
    transect = 66:99),
  data.frame(
    scientificName = "Clupea pallasii", 
    stratum = 3,
    transect = 100:107),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 1,
    transect = 1:16),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 2,
    transect = 17:19),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 3,
    transect = 20:39),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 4,
    transect = 67:90),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 5,
    transect = 94:96),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 6,
    transect = 98:101),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 7,
    transect = 111:149),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 1,
    transect = 2:16),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 2,
    transect = 23:34),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 3,
    transect = 50:85),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 4,
    transect = 94:96),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 5,
    transect = 98:101),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 6,
    transect = 125:127),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 7,
    transect = 136:139),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 1,
    transect = 2:16),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 2,
    transect = 20:28),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 3,
    transect = 50:72),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 4,
    transect = 74:86),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 5,
    transect = 94:96),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 6,
    transect = 97:101),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 1,
    transect = 1:16),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 2,
    transect = 20:33),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 3,
    transect = 38:40),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 4,
    transect = 41:46),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 5,
    transect = 47:85),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 6,
    transect = 87:96),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 7,
    transect = 97:99),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 8,
    transect = 124:129),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 9,
    transect = 136:139)
)

# Offshore strata
strata.manual.os <- bind_rows(
  data.frame(
    scientificName = "Clupea pallasii", 
    stratum = 1,
    transect = 1:8),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 1,
    transect = 1:3),
  data.frame(
    scientificName = "Engraulis mordax", 
    stratum = 2,
    transect = 4:8),
  data.frame(
    scientificName = "Sardinops sagax", 
    stratum = 1,
    transect = 1:8),
  data.frame(
    scientificName = "Scomber japonicus", 
    stratum = 1,
    transect = 1:8),
  data.frame(
    scientificName = "Trachurus symmetricus", 
    stratum = 1,
    transect = 1:8),
)

# Stock boundaries --------------------------------------------------------
stock.break.anch <- 40.430520 # Latitude of Cape Mendocino
stock.break.sar  <- 34.7 # Latitude of Pt. Conception (or change based on SST)

# Data collection settings ------------------------------------------------
# ER60 file info
raw.prefix    <- "1907RL_EK60"
raw.size      <-  50   # file size in megabytes (MB)
raw.log.range <- 350  # depth of ER60 logging (m)

# Echoview settings
er60.version  <- "V2.4.3" # ER60 version
ek80.version  <- "V1.10.3" # EK80 version
ev.version    <- "V9.0.318.34509" # Echoview version
int.start        <-   5  # Integration start line depth (m)
int.stop         <- 650  # Integration start line depth (m)
cps.depth        <-  70  # Integration depth for CPS (m)
krill.depth      <- 350  # Integration depth for krill (m)
hake.depth       <- 750  # integration depth for hake (m)
speed.filter     <-   5  # Speed filter threshold (kn)
vmr.krill        <- -45  # VMR value for krill formula operator (dB)
vmr.cps          <- -45  # VMR value for CPS (with swim bladders) formula operator (dB)
bin.depth        <-   5  # Integration bin depth (m)
bin.length       <- 100  # Integration bin width (m)
adz.range        <-   3  # Range (m) of acoustic dead zone
nasc.freq        <-  38  # Echosounder frequency used to estimate CPS biomass

# Adaptive sampling information ------------------------------------------
compulsory.spacing      <- 20  # minimum transect spacing (nmi) for compulsory acoustic transects
adaptive.spacing        <- 10  # minimum transect spacing (nmi) for adaptive acoustic transects
adaptive.cluster.size   <- 5   # minimum number of consecutive transects to define a cluster
cufes.threshold.anchovy <- 1   # egg density, eggs per minute
cufes.threshold.sardine <- 0.3 # egg density, eggs per minute

# # Calibration information ------------------------------------------------
cal.vessels        <- "RL"
cal.dir            <- "//swc-storage3-s.nmfs.local/AST3/SURVEYS/20190613_LASKER_SummerCPS/DATA/EK60/CALIBRATION/RESULTS"
cal.datetime       <- "30 April to 4 May 2019" # Date/time of calibration
cal.plot.date      <- "2019-05-01" # Date of the calibration, used to plot cal time series
cal.window         <- 5            # Number of days around calibration date to look for results
cal.group          <- "SWFSC"      # Group conducting the calibration
cal.personnel      <- "J. Renfree, T. Sessions, D. Murfin, and D. Palance" # Calibration participants
cal.loc            <- "10th Avenue Marine Terminal, San Diego Bay" # Location name
cal.lat.dd         <-   32.6956    # Cal location latitude in decimal degrees (for mapping, e.g. with ggmap) 37.7865°N @ Pier 30-32
cal.lon.dd         <- -117.15278   # Cal location longitude in decimal degrees (for mapping, e.g. with ggmap) -122.3844°W @ Pier 30-32
cal.lat            <- dd2decmin(cal.lat.dd)
cal.lon            <- dd2decmin(cal.lon.dd)
cal.sphere         <- "38.1-mm diameter sphere made from tungsten carbide (WC) with 6% cobalt binder material" # Cal sphere info
cal.sphere.name    <- "_Lasker_ sphere #1"
cal.sphere.z       <- 6 # Nominal depth of calibration sphere below the transducer
cal.imp.anal       <- "Agilent 4294A Precision Impedance Analyzer" # Info about impedance analyzer
# Other notes about calibration
cal.notes          <- "Calibration while alongside at the 10th Avenue pier, just north of the Coronado Bridge. Lasker calibration sphere #1"

# Physical conditions during calibration
cal.temp           <-   18.7   # enter water temperature
cal.sal            <-   33.8   # enter salinity
cal.c              <- 1516.8   # enter sound speed (m/s)
cal.min.z          <-    5     # enter minimum water depth below transducers
cal.max.z          <-    8     # enter maximum water depth below transducers
 
# Enter ambient noise estimates (dB re 1 W) for each vessel
# Lowest to highest frequency
cal.noise          <- list(RL = c(-128,-142,-148,-155,-140,-138))

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
