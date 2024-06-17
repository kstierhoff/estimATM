# Set-up survey directories
library(fs)

survey.name <- "2407RL"

fs::dir_create("C:/SURVEY",survey.name,"DATA/BIOLOGICAL",c("CUFES","HAUL","XRAY"))
fs::dir_create("C:/SURVEY",survey.name,"DATA",c("CTD","SCS","TDR","UCTD"))
fs::dir_create("C:/SURVEY",survey.name,"PROCESSED",c("CSV","ECS","Exported_Images","Files_to_Export","GPS"))

               
