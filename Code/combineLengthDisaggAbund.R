# Combine length-disaggregated abundance/biomass across regions ---------------
# Fixed or free scales?
L.disagg.scales <- "fixed" # or "free"

abund.summ.all <- abund.summ %>% 
  mutate(Region = "Core") %>% 
  bind_rows(abund.summ.ns)

# Create plots of length-disaggregated abundance and biomass
# Create list for storing plots
L.disagg.plots.all <- list()

if (L.disagg.scales == "fixed") {
  # Plot length-disaggrated abundance and biomass by length class for each species
  for (i in unique(abund.summ.all$Species)) {
    for (j in unique(abund.summ.all$Stock[abund.summ.all$Species == i])) {
      # Get y-axis limits for abundance and biomass plots
      y.max.abund   <- max(abund.summ.all$abundance[abund.summ.all$Species == i & abund.summ.all$Stock == j]) * 1.1
      y.max.biomass <- max(abund.summ.all$biomass[abund.summ.all$Species == i & abund.summ.all$Stock == j]) * 1.1
      
      if (!is.nan(y.max.abund)) {
        # Create x-axis breaks from TL vector
        max.x         <- max(abund.summ.all$TL[abund.summ.all$Species == i & abund.summ.all$Stock == j])
        x.breaks      <- seq(0, max.x, max.x/10)
        
        # Plot length-disaggregated abundance for each species
        L.abund.all <- ggplot() + 
          geom_bar(data = filter(abund.summ.all, Species == i, Stock == j),
                   aes(TL, abundance),
                   stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Core"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Nearshore"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray70',colour = 'gray20') +
          facet_wrap(~Region, nrow = 1) +
          scale_x_continuous("Length (cm)", breaks = x.breaks) + 
          scale_y_continuous('Abundance (n)', limits = c(0, y.max.abund),
                             expand = c(0,0), labels = fancy_sci) +
          theme_bw() +
          theme(strip.background.x = element_blank(),
                strip.text.x = element_text(face = "bold"))
        
        # Plot length-disaggregated biomass for each species
        L.biomass.all <- ggplot() + 
          geom_bar(data = filter(abund.summ.all, Species == i, Stock == j),
                   aes(TL, biomass),
                   stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Core"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Nearshore"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray70',colour = 'gray20') +
          facet_wrap(~Region, nrow = 1) +
          scale_x_continuous("Length (cm)", breaks = x.breaks) +
          scale_y_continuous('Biomass (t)', limits = c(0, y.max.biomass),
                             expand = c(0,0), labels = fancy_sci) +
          # facet_wrap(~Stock, nrow = 1) +
          theme_bw() +
          theme(strip.background.x = element_blank(),
                strip.text.x = element_text(face = "bold"))
        
        # Arrange all plots
        L.disagg.all <- plot_grid(L.abund.all, L.biomass.all, ncol = 1, align = 'h')
        
        # Save plot
        ggsave(L.disagg.all, 
               filename = paste0(here("Figs/fig_L_disagg_combo_"), i, "-", j, ".png"), 
               height = 8, width = 8)
        
        # Add plot to list
        L.disagg.plots.all[[i]][[j]] <- L.disagg.all
      }
    }
  }
  
} else {
  
  # Plot length-disaggrated abundance and biomass by length class for each species
  for (i in unique(abund.summ.all$Species)) {
    for (j in unique(abund.summ.all$Stock[abund.summ.all$Species == i])) {
      # Get y-axis limits for abundance and biomass plots
      y.max.abund   <- max(abund.summ.all$abundance[abund.summ.all$Species == i & abund.summ.all$Stock == j]) * 1.1
      y.max.biomass <- max(abund.summ.all$biomass[abund.summ.all$Species == i & abund.summ.all$Stock == j]) * 1.1
      
      if (!is.nan(y.max.abund)) {
        # Create x-axis breaks from TL vector
        max.x         <- max(abund.summ.all$TL[abund.summ.all$Species == i & abund.summ.all$Stock == j])
        x.breaks      <- seq(0, max.x, max.x/10)
        
        # Plot length-disaggregated abundance for each species
        L.abund.all <- ggplot() + 
          geom_bar(data = filter(abund.summ.all, Species == i, Stock == j),
                   aes(TL, abundance),
                   stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Core"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Nearshore"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray70',colour = 'gray20') +
          facet_wrap(~Region, nrow = 1, scales = "free_y") +
          scale_x_continuous("Length (cm)", breaks = x.breaks) + 
          scale_y_continuous('Abundance (n)', labels = fancy_sci) +
          theme_bw() +
          theme(strip.background.x = element_blank(),
                strip.text.x = element_text(face = "bold"))
        
        # Plot length-disaggregated biomass for each species
        L.biomass.all <- ggplot() + 
          geom_bar(data = filter(abund.summ.all, Species == i, Stock == j),
                   aes(TL, biomass),
                   stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Core"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray50',colour = 'gray20') +
          # geom_bar(data = filter(abund.summ.all, Species == i, Stock == j, Region == "Nearshore"),
          #          aes(TL, abundance),
          #          stat = 'identity',fill = 'gray70',colour = 'gray20') +
          facet_wrap(~Region, nrow = 1, scales = "free_y") +
          scale_x_continuous("Length (cm)", breaks = x.breaks) +
          scale_y_continuous('Biomass (t)', labels = fancy_sci) +
          theme_bw() +
          theme(strip.background.x = element_blank(),
                strip.text.x = element_text(face = "bold"))
        
        # Arrange all plots
        L.disagg.all <- plot_grid(L.abund.all, L.biomass.all, ncol = 1, align = 'h')
        
        # Save plot
        ggsave(L.disagg.all, 
               filename = paste0(here("Figs/fig_L_disagg_combo_"), i, "-", j, ".png"), 
               height = 8, width = 8)
        
        # Add plot to list
        L.disagg.plots.all[[i]][[j]] <- L.disagg.all
      }
    }
  }  
}

# Create blank plots for missing species
for (i in cps.spp) {
  if (is.null(L.disagg.plots.all[[i]])) {
    df <- data.frame()
    L.disagg.temp.all <- ggplot(df) + geom_point() + 
      xlim(0,10) + ylim(0,10) + 
      annotate('text',5,5,label = 'No Data', size = 6, fontface = 'bold') +
      theme_bw() + ggtitle(i)  
    ggsave(L.disagg.temp.all, 
           filename = paste0(here("Figs/fig_L_disagg_os_"), i, ".png"))
  }
}
