# Combine plots from core and nearshore survey areas
for (ss in cps.spp) {
  # List core figures
  biomass.path.core <- fs::dir_ls(here("Figs"), regexp = paste0("fig_biomass_dens_", ss))
  # List nearshore figures
  biomass.path.ns   <- fs::dir_ls(here("Figs"), regexp = paste0("fig_biomass_dens_ns_", ss))
  # Extract stocks from filenames
  biomass.stocks <- str_split(biomass.path.core, "_") %>% 
    unlist() %>% 
    str_subset(ss) %>% 
    str_replace(paste0(ss, "-"), "") %>% 
    str_replace(".png", "")
  
  for (sss in biomass.stocks) {
    # Get aspect ratio for map image
    image.aspect <- magick::image_read(str_subset(biomass.path.core, sss)) %>% 
      image_info() %>% 
      mutate(aspect = width/height) %>% 
      pull()
    
    # Create cowplot objects for map grid
    biomass.fig.core <- ggdraw() + draw_image(str_subset(biomass.path.core, sss)) 
    biomass.fig.ns   <- ggdraw() + draw_image(str_subset(biomass.path.ns, sss))
    
    # Create final figure
    biomass.fig.all <- plot_grid(biomass.fig.core, biomass.fig.ns, 
                                 nrow = 1, labels = c("a)","b)"))  
    
    # Save final figure
    ggsave(biomass.fig.all, 
           filename = here("Figs", paste0("fig_biomass_dens_combo_", ss, "-", sss, ".png")),
           height = map.height, width = map.height*image.aspect*2) 
  }
}

