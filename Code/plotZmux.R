# Process impedance results -----------------------------------------------
# List ZMUX files
zmux.files <- dir(here("Data/Zmux"), pattern = ".csv", full.names = T)

# Create data frame for results
zmux <- data.frame()

# Process CSV files
for (i in zmux.files) {
  # Extract tranducer model from file name
  txdr <- unlist(str_split(i,"/"))
  txdr <- txdr[length(txdr)] %>% str_replace('.csv','')
  
  # Read file
  df <-  read_csv(i) 
  
  # Extract R
  df.R <- df %>%
    rename(f = Frequency) %>%
    select(f,grep('^R',names(df))) %>%
    gather(q,R,-f) %>% 
    mutate(
      txdr = txdr,
      Quadrant = str_sub(q,start = nchar(q)),
      f = f/1e3)
  
  # Extract X
  df.X <- df %>%
    rename(f = Frequency) %>%
    select(f,grep('^X',names(df))) %>% 
    gather(q,X,-f) %>% 
    mutate(
      txdr = txdr,
      Quadrant = str_sub(q,start = nchar(q)),
      f = f/1e3)
  
  # Combine R and X
  zmux.temp <- df.R %>% bind_cols(select(df.X,X)) %>% 
    select(txdr,f,Quadrant,R,X)
  
  zmux <- bind_rows(zmux,zmux.temp)
}
# Write data to CSV
write.csv(zmux, file = here("Output/zmux_raw.csv"), 
          quote = F, row.names = F)

# Derive other parameters
zmux <- zmux %>% 
  mutate(Z = complex(real = R,imaginary = X),
         Y = 1/Z,
         G = Re(Y),
         B = Im(Y),
         theta = atan(X/R)*180/pi) # Convert to degrees

# Save impedance data
save(zmux, file = here("Output/zmux_data.Rdata"))

# Create impedance plots --------------------------------------------------

# Create list to store plots
zmux.plots <- list()

# Get transducer models
txdrs <- unique(zmux$txdr)

# Create plot for each transducer
for (i in txdrs) {
  # Filter data
  p <- filter(zmux,txdr == i)
  # Plot impedance vs. frequency
  Z.plot <- ggplot(p,aes(f,abs(Z),colour = Quadrant)) + geom_path() + 
    scale_x_continuous('Frequency (kHz)') +
    scale_y_continuous(expression(italic("|Z|")~(Omega))) +
    theme_bw() + theme(legend.position = 'none')
  
  # Add legend to the 18 kHz plot only
  if (length(grep('18',i)) == 1) {
    Z.plot <- Z.plot +
      theme(legend.position =  c(1,0),
            legend.justification = c(1,0),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(face = 'bold'),
            legend.title = element_blank())
  }
  
  # Plot phase vs. frequency
  theta.plot <- ggplot(p,aes(f,theta,colour = Quadrant)) + geom_path() + 
    scale_x_continuous('Frequency (kHz)') +
    scale_y_continuous(expression(italic(theta)~(deg.))) +
    theme_bw() + theme(legend.position = 'none')
  
  # Plot conductance vs. frequency
  G.plot <- ggplot(p,aes(f,G*1e3,colour = Quadrant)) + geom_path() + 
    scale_x_continuous('Frequency (kHz)') +
    scale_y_continuous(expression(italic(G)~(mS))) +
    theme_bw() + theme(legend.position = 'none')
  
  # Plot susceptance versus conductance (admittance circle)
  B.plot <- ggplot(p,aes(G*1e3,B*1e3,colour = Quadrant)) + geom_path() + 
    scale_x_continuous(expression(italic(G)~(mS))) +
    scale_y_continuous(expression(italic(B)~(mS))) +
    theme_bw() + theme(legend.position = 'none')
  
  # Arrange all plots
  title <- ggdraw() + draw_label(i, fontface = 'bold')
  
  txdr.plot <- plot_grid(Z.plot, theta.plot, G.plot, B.plot,
                         nrow = 2, align = 'hv', labels = c("a)","b)","c)","d)"))
  
  txdr.plot <- plot_grid(title, txdr.plot, ncol = 1, rel_heights = c(0.1, 1))
  
  ggsave(paste0(here("Figs/fig_impedance_plot_"), i, ".png"), 
         height = 8, width = 8)
  
  # Add plot to list
  zmux.plots[[i]] <- txdr.plot
}

# Combine all plots (takes time!!)
txdr.plot.final <- plot_grid(zmux.plots[[grep('18',txdrs)]], 
                             zmux.plots[[grep('38',txdrs)]], 
                             zmux.plots[[grep('70', txdrs)]], 
                             zmux.plots[[grep('120',txdrs)]], 
                             zmux.plots[[grep('200',txdrs)]], 
                             zmux.plots[[grep('333',txdrs)]],
          ncol = 2)

# Save final plot
ggsave(txdr.plot.final, 
       filename = here("Figs/fig_impedance_plot_all.png"), 
       height = 17, width = 13)

save(zmux.plots, file = here("Output/impedance_plots.Rdata"))
save(txdr.plot.final, file = here("Output/impedance_plots_all.Rdata"))
