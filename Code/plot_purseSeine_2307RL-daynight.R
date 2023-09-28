# Load basemap
load(here("Data/Map/basemap.Rdata"))

# Import waypoints
# Read transect waypoints
wpts <- read_csv(here("Data/Nav", wpt.filename))

# Convert planned transects to sf; CRS = crs.geog
wpts.sf <- wpts %>% 
  filter(Type %in% wpt.types) %>% 
  st_as_sf(coords = c("Longitude","Latitude"), crs = crs.geog) %>% 
  mutate(
    label = paste("Transect", Transect),
    popup = paste('<b>Transect:</b>', Transect, Type)
  )

transects.sf <- wpts.sf %>% 
  group_by(Type, Transect, Region) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  ungroup() %>% 
  mutate(
    distance = round(as.numeric(st_length(.))/1852,1),
    label    = paste("Transect", Transect),
    popup    = paste('<b>Transect:</b>', Transect, Type, '<br/>',
                     'Distance:', distance, 'nmi<br/>')
  )

# Use nav data to resize map to survey progress
map.bounds <- transects.sf %>%
  st_transform(crs = crs.proj) %>%
  st_bbox()  

# Determine map aspect ratio and set height and width
map.aspect <- (map.bounds$xmax - map.bounds$xmin)/(map.bounds$ymax - map.bounds$ymin)
map.height <- 10
map.width  <- map.height*map.aspect

# Calculate pie radius based on latitude range
pie.radius.ns  <- as.numeric(abs(map.bounds$ymin - map.bounds$ymax)*pie.scale)

# Define day/night

# Summarize NASC by date and lat/long
set.sun <- lm.sets %>% 
  # mutate(date = date(time)) %>% 
  group_by(date) %>% 
  summarise(lat = mean(lat),
            long = mean(long)) %>% 
  as.data.frame()

# Get sunrise/sunset for each survey day
set.daynight <- data.frame()

for (i in 1:nrow(set.sun)) {
  tmp <- day_night(date = date(set.sun$date[i]), 
                   geocode = data.frame(lat = set.sun$lat[i], lon = set.sun$long[i]),
                   twilight = survey.twilight)
  set.daynight <- bind_rows(set.daynight, tmp)
}

# Format the results
set.daynight <- set.daynight %>% 
  mutate(sunrise = ymd_hms(paste(day, hms::as_hms(sunrise*3600))) - minutes(survey.twilight.offset),
         sunset  = sunrise + daylength*3600 + minutes(survey.twilight.offset),
         sunrise = as.character(sunrise), # convert to character to work with gather()
         sunset  = as.character(sunset)) %>% 
  select(day, sunrise, sunset) %>% 
  gather(period, time, -day) %>% 
  mutate(time    = ymd_hms(time, tz = "UTC"), # convert back to POSIX
         sun_key = as.character(time)) %>% 
  arrange(time) %>% 
  mutate(id = seq(1, nrow(.))) %>% 
  mutate(day_night = case_when(
    period == "sunrise" ~ "Day",
    period == "sunset"  ~ "Night"))

lm.sets <- lm.sets %>% 
  mutate(id = cut(as.numeric(datetime), as.numeric(set.daynight$time),
                  include.lowest = T, labels = F)) %>% 
  left_join(select(set.daynight, id, period)) %>%
  mutate(day_night = case_when(
    period == "sunrise" ~ "Day",
    period == "sunset"  ~ "Night")) 

set.pie <- set.pie %>% 
  left_join(select(lm.sets, key.set, day_night))

# Filter for empty trawls
set.zero    <- filter(set.pie, AllCPS == 0)

# Replace zeros with minimally small value for scatterpie plotting
set.pos <- filter(set.pie, AllCPS > 0) %>% 
  replace(. == 0, 0.0000001) %>% 
  arrange(desc(X))

set.pies.lm <- base.map + 
  # Plot purse seine pies
  scatterpie::geom_scatterpie(data = filter(set.pos, str_detect(key.set, "LM")), 
                              aes(X, Y, group = key.set, r = pie.radius.ns),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "RndHerring", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "R. herring", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, rnd.herring.color, sardine.color)) +
  geom_point(data = filter(set.zero, vessel.name == "LM"), aes(X, Y)) +
  # # ggtitle("Lisa Marie") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

ggsave(set.pies.lm, filename = here("Figs/fig_seine_proportion_set_wt_LisaMarie.png"),
       height = 10, width = map.width)

set.pies.lm.day <- base.map + 
  # Plot purse seine pies
  scatterpie::geom_scatterpie(data = filter(set.pos, str_detect(key.set, "LM"), day_night == "Day"), 
                              aes(X, Y, group = key.set, r = pie.radius.ns),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "RndHerring", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "R. herring", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, rnd.herring.color, sardine.color)) +
  geom_point(data = filter(set.zero, vessel.name == "LM", day_night == "Day"), 
             aes(X, Y), shape = 21, colour = "white", fill = "black") +
  ggtitle("Lisa Marie - Day") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

set.pies.lm.night <- base.map + 
  # Plot purse seine pies
  scatterpie::geom_scatterpie(data = filter(set.pos, str_detect(key.set, "LM"), day_night == "Night"), 
                              aes(X, Y, group = key.set, r = pie.radius.ns),
                              cols = c("Anchovy", "JackMack", "Jacksmelt",
                                       "PacHerring", "PacMack", "RndHerring", "Sardine"),
                              color = 'black', alpha = 0.8) +
  # Configure trawl scale
  scale_fill_manual(name = 'Species',
                    labels = c("Anchovy", "J. Mackerel", "Jacksmelt",
                               "P. herring", "P. mackerel", "R. herring", "Sardine"),
                    values = c(anchovy.color, jack.mack.color, jacksmelt.color,
                               pac.herring.color, pac.mack.color, rnd.herring.color, sardine.color)) +
  geom_point(data = filter(set.zero, vessel.name == "LM", day_night == "Night"), 
             aes(X, Y), shape = 21, colour = "white", fill = "black") +
  ggtitle("Lisa Marie - Night") +
  coord_sf(crs = crs.proj, # CA Albers Equal Area Projection
           xlim = c(map.bounds["xmin"], map.bounds["xmax"]),
           ylim = c(map.bounds["ymin"], map.bounds["ymax"]))

set.pies.lm.daynight <- patchwork::wrap_plots(set.pies.lm.day, set.pies.lm.night, nrow = 1)

ggsave(set.pies.lm.daynight, filename = here("Figs/fig_seine_proportion_set_wt_LisaMarie-daynight.png"),
       height = 10, width = map.width*2)
