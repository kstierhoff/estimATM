# Script for processing and plotting EK80 calibration results while in Frequency Modulation (FM) mode

# Requires only .xml results from EK80
# Parameters are set in the corresponding survey settings file (e.g., Doc/settings/settings_2407RL.R)
# Results for each vessel saved to .Rdata

# Cycle through each vessel
for (i in cal.vessels.fm) {
  # List calibration results files
  cal.files.fm <- dir_ls(cal.dir.fm[i],
                         pattern = "*.xml") 
  
  # Initialize data frames for storing results
  cal.res.fm   <- data.frame()
  
  for (j in cal.files.fm) {
    # Extract FM calibration results and transducer info
    cal.fm.extract <- extract_cal_fm(j, vessel.name = i, survey.name = survey.name)
    
    # Combine results and transducer info
    cal.res.tmp <- cal.fm.extract$cal.res %>% 
      mutate(txdr = cal.fm.extract$cal.info$txdr.type,
             txdr.freq = as.numeric(
               str_extract(cal.fm.extract$cal.info$txdr.type, 
                           pattern = "\\d{1,3}\\b")))
    
    # Combine results with other frequencies
    cal.res.fm <- bind_rows(cal.res.fm, cal.res.tmp)
  }
  
  # Format FM calibration results for plotting
  cal.fm <- cal.res.fm %>% 
    pivot_longer(cols = c(-txdr, -txdr.freq, -freq)) %>% 
    mutate(txdr = fct_reorder(txdr, txdr.freq)) # Reorder transducers by frequency
  
  # Store in data frame specific to the current vessel
  assign(paste0("cal.fm.", i), cal.fm)
  
  # save output to .Rdata and CSV
  save(cal.fm,
       file = here(paste0("Output/cal_output_table_FM_", i, ".Rdata")))
  
  write_csv(cal.fm,
            file = here(paste0("Output/cal_output_table_FM_", i, ".csv")))
  
  # Plot results ------------------------------------------------------------
  # Gain
  cal.gain <- ggplot(filter(cal.fm, name == "gain"), aes(freq, value)) + 
    geom_point() + facet_wrap(~txdr, scales = "free_x", nrow = 1) +
    labs(y = expression(italic(G)[0]*' '*(dB))) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          strip.text.x = element_text(face = "bold"),
          strip.background = element_blank())
  
  # Beam angles
  cal.ba <- ggplot() + 
    geom_point(data = filter(cal.fm, name == "ba.alon"), aes(freq, value), colour = "cyan") +
    geom_point(data = filter(cal.fm, name == "ba.athw"), aes(freq, value), colour = "magenta") +
    facet_wrap(~txdr, scales = "free_x", nrow = 1) +
    labs(y = expression(alpha["-3dB"]*", "*beta["-3dB"]*''*' '*(degree))) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          strip.text.x = element_blank(),
          strip.background = element_blank())
  
  # Offset angles
  cal.oa <- ggplot() + 
    geom_point(data = filter(cal.fm, name == "oa.alon"), aes(freq, value), colour = "cyan") +
    geom_point(data = filter(cal.fm, name == "oa.athw"), aes(freq, value), colour = "magenta") +
    facet_wrap(~txdr, scales = "free_x", nrow = 1) +
    theme_bw() + 
    theme(strip.text.x = element_blank(),
          strip.background = element_blank()) +
    labs(x = "\nFrequency (kHz)",
         y = expression(alpha[0]*", "*beta[0]*''*' '*(degree)))
  
  # Combine plots
  cal.fm.all <- plot_grid(cal.gain, cal.ba, cal.oa, ncol = 1, align = "v")
  
  # Save plot
  ggsave(cal.fm.all, filename = here("Figs", 
                                     paste0("fig_cal_FM_AllFreqs_", i, ".png")),
         height = 6, width = 10)
}

# Save all FM calibration results to .Rdata
save(list = ls(pattern = "cal.fm."), file = here("Output/cal_results_FM.Rdata"))
