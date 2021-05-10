# Check duplicate barcodes in the trawl database
# B. Schwartzkopf - 2021-05-03

# checking for duplicated barcodes
id <- all[c(3,18,19)] #individual ID is column 19
id <- na.omit(id) # need to take out NA's
id$check <- duplicated(id$individual_ID)
unique(id$check) # quick way to see if there are any trues
# need code to print rows where id$check == TRUE
