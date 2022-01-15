if (date(erddap.survey.end) > date(now())) {
  hab.days     <- floor(((date(now()) - days(2)) - ymd(erddap.survey.start))/3)
  hab.date.end <- date(now()) - days(2)
} else {
  # Calculate the number of days in the survey
  hab.days     <- floor((ymd(erddap.survey.end) - ymd(erddap.survey.start))/3)
  hab.date.end <- date(erddap.survey.end)
}

# Format survey start date for downloading sardine potential habitat maps
hab.date.start <- format(ymd(erddap.survey.start), "%Y-%m-%dT12:00:00Z")
hab.date.mid1  <- format(ymd(erddap.survey.start) + hab.days, "%Y-%m-%dT12:00:00Z")
hab.date.mid2  <- format(ymd(erddap.survey.start) + hab.days*2, "%Y-%m-%dT12:00:00Z")

# Combine habitat model dates
hab.date.all <- "2021-04-01"

load(here("Data/Nav/nav_data.Rdata"))

if (survey.year >= 2021) {
  # Starting in 2021, sardine habitat data are available for downloaded from ERDDAP
  # The daily MODIS data are available for 2006-present, so in theory this code should
  # work for all of the ATM surveys of CPS, but the code below will recreate the maps
  # as presented in the survey and biomass report Tech Memos from those surveys.
  
  hab.date.all <- format(ymd(hab.date.all), "%Y-%m-%dT12:00:00Z")
  
  if(exists("hab.data.all")) rm(hab.data.all)
  
  for (ddd in hab.date.all) {
    # Generate URLs from dates
    habURL <- URLencode(
      paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/sardine_habitat_modis_Lon0360.csv0?potential_habitat%5B(",
             ddd,
             ")%5D%5B(27.0):1:(51.0)%5D%5B(230.0):1:(247.0)%5D,potential_habitat_mask%5B(",
             ddd,
             ")%5D%5B(27.0):1:(51.0)%5D%5B(230.0):1:(247.0)%5D,chlorophyll%5B(",
             ddd, 
             ")%5D%5B(27.0):1:(51.0)%5D%5B(230.0):1:(247.0)%5D,sst%5B(",
             ddd,
             ")%5D%5B(27.0):1:(51.0)%5D%5B(230.0):1:(247.0)%5D"))
    
    hab.temp <- read_csv(habURL, 
                         col_names = c("datetime","lat","long","habitat","habitat_mask","chla","sst")) %>% 
      # filter(!is.nan(habitat_mask)) %>%  
      mutate(long = long-360,
             date = as.factor(date(datetime))) %>% 
      project_df(to = 3310)
    
    if(exists("hab.data.all")) {
      hab.data.all <- bind_rows(hab.data.all, hab.temp)
    } else {
      hab.data.all <- hab.temp
    }
  }
  
  # Save habitat data
  dir_create(here("Data/Habitat"))
  saveRDS(hab.data.all, file = here("Data/Habitat/habitat_data_all.rds"))
  
  # Get map bounds for setting map extent
  map.bounds <- nav.sf %>% 
    sf::st_bbox()
  
  # map.bounds <- hab.data %>% 
  #   st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
  #   st_bbox()
  
  # Get map features --------------------------
  # Get state data
  states     <- ne_states(country = 'United States of America', returnclass = 'sf')
  hab.states <- filter(states, name %in% c("California","Oregon","Washington",
                                           "Nevada","Idaho"))
  # Get countries
  countries <- ne_countries(scale = "large", returnclass = "sf") %>%
    filter(subregion %in% c("Northern America","Central America"))
  
  # Read bathy contours shapefile 
  bathy <- sf::st_read(here("Data/GIS/bathy_contours.shp")) %>% 
    sf::st_transform(4326) %>% 
    rename(Depth = Contour)
  
  # Create map
  hab.map.all <- ggplot() + 
    facet_wrap(~date, ncol = 1) +
    geom_raster(data = filter(hab.data.all, !is.nan(habitat_mask)), 
                aes(long, lat, fill = factor(habitat_mask))) +
    geom_sf(data = countries, fill = "black", color = "gray30") +
    geom_sf(data = states, fill = "black", colour = "gray30") +
    scale_fill_manual(name = bquote(atop('Potential', 'habitat')),
                      # breaks = c(1, 10, 20, 100),
                      values = c("blue","yellow","orange","red"), 
                      labels = c("Unsuitable","Bad","Good","Optimal"),
                      guide = guide_legend(reverse = TRUE)) +
    # Plot bathymetry contours
    # geom_sf(data = bathy, colour = "gray90") +
    # Format axes and titles
    xlab("Longitude") + ylab("Latitude") + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_sf(crs = 4326, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
    
    theme_bw() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          legend.position="bottom")
  
  chl.map.all <- ggplot() + 
    facet_wrap(~date, ncol = 1) +
    geom_raster(data = filter(hab.data.all, !is.nan(chla), chla <= 30), 
                aes(long, lat, fill = chla)) +
    geom_sf(data = countries, fill = "black", color = "gray30") +
    geom_sf(data = states, fill = "black", colour = "gray30") +
    cmocean::scale_fill_cmocean(name = "algae",
                                labels = c(0,10, 20, 30),
                                bquote(atop('Chorophyll a',~'(mg'~'m'^-3*')'))) +
    # Plot bathymetry contours
    # geom_sf(data = bathy, colour = "gray90") +
    # Format axes and titles
    xlab("Longitude") + ylab("Latitude") + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_sf(crs = 4326, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
    
    theme_bw() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          legend.position="bottom")
  
  sst.map.all <- ggplot() + 
    facet_wrap(~date, ncol = 1) +
    geom_raster(data = filter(hab.data.all, !is.nan(sst)), 
                aes(long, lat, fill = sst)) +
    geom_sf(data = countries, fill = "black", color = "gray30") +
    geom_sf(data = states, fill = "black", colour = "gray30") +
    cmocean::scale_fill_cmocean(name = "thermal",
                                bquote(atop('SST',~'(C'^'o'~')'))) +
    # scale_fill_manual(name = "Potential habitat",
    #                   # breaks = c(1, 10, 20, 100),
    #                   values = c("blue","yellow","orange","red"), 
    #                   labels = c("Unsuitable","Bad","Good","Optimal"),
    #                   guide = guide_legend(reverse = TRUE)) +
    # Plot bathymetry contours
    # geom_sf(data = bathy, colour = "gray90") +
    # Format axes and titles
    xlab("Longitude") + ylab("Latitude") + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_sf(crs = 4326, # CA Albers Equal Area Projection
             xlim = unname(c(map.bounds["xmin"], map.bounds["xmax"])),
             ylim = unname(c(map.bounds["ymin"], map.bounds["ymax"]))) +
    
    theme_bw() +
    theme(panel.spacing = unit(1, "lines"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          legend.position="bottom")
  
  # Save map
  ggsave(hab.map.all, 
         filename = here("Figs/fig_habitat_map_all.png"),
         width = 6, height = 6)
  
  ggsave(chl.map.all, 
         filename = here("Figs/fig_chla_map_all.png"),
         width = 6, height = 6)
  
  ggsave(sst.map.all, 
         filename = here("Figs/fig_sst_map_all.png"),
         width = 6, height = 6)
  
  # Combine all habitat variables
  anch.hab.grid <- plot_grid(hab.map.all, chl.map.all, sst.map.all,
                             labels = c("a)", "b)","c)"),
                             nrow = 1,
                             align = "hv")
  
  # Save combo map
  ggsave(anch.hab.grid, filename = here("Figs/fig_habitat_map_anchovy.png"),
         width = 13, height = 6)
  
} 
