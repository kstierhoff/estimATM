list.files("C:/SURVEY/2107RL/PROCESSED/EV/CSV/LISAMARIE")

i = "LM"

# List all CSV files
nasc.files <- dir_ls(file.path("C:/SURVEY/2107RL/PROCESSED/EV/CSV/LISAMARIE"),
                     regexp = nasc.pattern.cps[i],
                     ignore.case = TRUE)

ii = 118

tmp <- atm::extract_csv(nasc.files[ii]) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

tmp.path <- tmp %>% 
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

mapview(tmp.path) + mapview(tmp)

nasc.files[ii]
