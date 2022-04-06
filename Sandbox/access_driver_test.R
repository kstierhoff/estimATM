# This script is for testing the installation of the 
# Microsoft Access Database Engine 2016 Redistributable available here:
# https://www.microsoft.com/en-us/download/details.aspx?id=54920

# Load libraries
library(odbc)
library(here)

# Test connection
trawl.con <- odbc::dbConnect(odbc::odbc(), Driver = "Microsoft Access Driver (*.mdb, *.accdb)", 
                             DBQ = file.path(here("Data/Trawl"), "TrawlDataEntry2107RL.accdb"))

# View connection
trawl.con

# Close connection
odbc::dbDisconnect(trawl.con)
