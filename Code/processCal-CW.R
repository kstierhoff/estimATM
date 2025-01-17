# Script for processing and plotting EK80 calibration results while in Continuous Wave (CW) mode

# Requires both .xml results from EK80, and an .ecs file from Echoview
# Parameters are set in the corresponding survey settings file (e.g., Doc/settings/settings_2407RL.R)
# Results for each vessel saved to .Rdata

# Cycle through each vessel
for (i in cal.vessels) {
  
  # Get list of calibration files for that vessel
  cal.files <- sort(list.files(cal.dir[i], pattern = ".xml", 
                               full.names = TRUE))
  
  # Initialize data frames for storing results
  cal.res   <- data.frame()
  cal.pings <- data.frame()
  
  # Cycle through each calibration file (frequency)
  for (j in cal.files) {
    
    # Extract calibration info for that frequency
    cal <- extract_cal(j)
    
    # Append to data frames
    cal.res <- bind_rows(cal.res,   cal$cal.res)
    cal.pings <- bind_rows(cal.pings, cal$cal.pings)
  }
  
  # Now read calibration results from ECS file
  cal.ECS <- list.files(cal.dir[i], pattern = ".ecs$", 
                        full.names = TRUE) %>%
    atm::extract_cal_ecs() %>% 
    arrange(Frequency)
  
  # create data frame of important echosounder parameters
  cal.params <- cal.res %>% 
    arrange(txdr_freq) %>% 
    select(txdr_freq,  # Transducer frequency
           txdr_type,  # Transducer model
           txdr_sn,    # Transducer serial number
           gpt_power,  # Transmit power
           gpt_pd)     # Pulse duration
  
  # Specify names for echosounder parameters 
  names(cal.params) <- c("Frequency",
                         "Model",
                         "Serial Number",
                         "Transmit Power ($p_\\mathrm{et}$)",
                         "Pulse Duration ($\\tau$)")
  
  
  # create data frame of beam model results
  bm.res <- select(cal.ECS,
                   Temperature,
                   Salinity,
                   SoundSpeed,
                   Frequency, 
                   Gain, 
                   Sa_correction, 
                   Beamwidth_alongship, 
                   Beamwidth_athwartship,
                   OffsetAngle_alongship, 
                   OffsetAngle_athwartship,
                   TwoWayBeamAngle) %>% 
    mutate(RMS = cal.rms[[i]])
  
  # Specify number of significant digits for certain parameters
  bm.res$Temperature             <- formatC(bm.res$Temperature, format="f", digits=1)
  bm.res$Salinity                <- formatC(bm.res$Salinity, format="f", digits=1)
  bm.res$SoundSpeed              <- formatC(bm.res$SoundSpeed, format="f", digits=1)
  bm.res$Frequency               <- formatC(bm.res$Frequency, format="d", digits=0)
  bm.res$Gain                    <- formatC(bm.res$Gain, format="f", digits=2)
  bm.res$Sa_correction           <- formatC(bm.res$Sa_correction, format="f", digits=2)
  bm.res$Beamwidth_alongship     <- formatC(bm.res$Beamwidth_alongship, format="f", digits=2)
  bm.res$Beamwidth_athwartship   <- formatC(bm.res$Beamwidth_athwartship, format="f", digits=2)
  bm.res$OffsetAngle_alongship   <- formatC(bm.res$OffsetAngle_alongship, format="f", digits=2)
  bm.res$OffsetAngle_athwartship <- formatC(bm.res$OffsetAngle_athwartship, format="f", digits=2)
  bm.res$TwoWayBeamAngle         <- formatC(bm.res$TwoWayBeamAngle, format="f", digits=2)
  bm.res$RMS                     <- formatC(bm.res$RMS, format="f", digits=2)
  
  # Specify names for beam model results
  names(bm.res) <- c("Temperature",
                     "Salinity",
                     "Sound speed",
                     "Frequency",
                     "On-axis Gain ($G_0$)",
                     "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)",
                     "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",
                     "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",
                     "Angle Offset Along. ($\\alpha_{0}$)",
                     "Angle Offset Athw. ($\\beta_{0}$)",
                     "Equivalent Two-way Beam Angle ($\\Psi$)",
                     "RMS")
  
  # Sort data frames by frequency
  param.output <- cal.params %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  bm.output <- bm.res %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  # Create column defining parameter units
  param.units <- data.frame(Units = c(" ",                 # Model number
                                      " ",                 # Serial number
                                      "W",                 # Transmit power
                                      "ms"))               # Pulse duration
  
  # Create column defining beam model units
  bm.units <- data.frame(Units = c("C",                    # Temperature
                                   "ppt",                  # Salinity
                                   "m s$^{-1}$",               # Sound speed
                                   "dB re 1",              # Transducer gain
                                   "dB re 1",              # Sa correction factor
                                   "deg",                  # Alongship beamwidth
                                   "deg",                  # Athwartship beamwidth
                                   "deg",                  # Alongship offset angle
                                   "deg",                  # Athwartship offset angle
                                   "dB re 1 sr",           # Equivalent two-way beam angle
                                   "db"))                  # RMS
  
  # Add units column to parameter data frame
  param.output <- bind_cols(param.output, param.units) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  # Add units column to beam model results data frame
  bm.output <- bind_cols(bm.output, bm.units) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  # combine the parameters and results data frames
  all.output <- bind_rows(param.output, bm.output) %>% 
    rename(" " = "Frequency ($f$, kHz)")
  
  # Store in data frame specific to the current vessel
  assign(paste0("all.output.", i), all.output)
  
  # save output to .Rdata and CSV
  save(all.output,
       file = here(paste0("Output/cal_output_table_", i, ".Rdata")))
  
  write_csv(all.output,
            file = here(paste0("Output/cal_output_table_", i, ".csv")))
  
  # If saving figures, then plot and save calibration polar plots
  if (save.figs) {
    
    # Get list of single-target csv files
    single.target.files <- sort(list.files(single.targets.dir[i], pattern = ".csv", 
                                           full.names = TRUE))
    
    # Create data frame that will contain TS info for all frequencies
    cal.pings <- data.frame()
    
    # Loop through and read each file
    for (j in single.target.files) {
      
      # Parse out frequency from filename
      freq <- as.numeric(gsub(".*?(\\d+)kHz.*", "\\1", basename(j)))
      
      # Read single-target CSV file
      singleTargets <- read_csv(j) %>%                           # Read entire file
        select(TS_comp, Angle_minor_axis, Angle_major_axis) %>%  # Retain only compensated TS and angles
        mutate(txdr_freq = freq) %>%                             # Add frequency column, for parsing data later on
        mutate(target_ts = sphere.TS[[i]][[as.character(freq)]])
      
      # Add to data frame
      cal.pings <- bind_rows(cal.pings, singleTargets)
    }
    
    # Set axis limits based on range of ping angles
    cal.lim.tmp <- round(max(max(cal.pings$Angle_minor_axis), max(cal.pings$Angle_major_axis))) 
    
    # If range is odd, add 1 to make axis ticks look nice
    if (cal.lim.tmp %% 2) {
      cal.axis.lims <- c(-(cal.lim.tmp + 1), cal.lim.tmp + 1)
    } else {
      cal.axis.lims <- c(-cal.lim.tmp, cal.lim.tmp)
    }
    
    # Add a column for the relative TS, along with a scaled version that is
    # limited to -1 and 1 dB
    cal.pings <- mutate(cal.pings, 
        relTS_c        = TS_comp - target_ts,
        relTS_c_scaled = case_when(
          relTS_c >= 1 ~ 1,
          relTS_c <= -1 ~-1,
          between(relTS_c,-1,1) ~ relTS_c))
    
    # If scales are fixed, then set fixed x- and y-axis limits using cal.axis.lims
    if (cal.scales == "fixed") {
      
      # Plot relative beam-compensated target strength data #####
      tsc.scatter <- ggplot(cal.pings, aes(Angle_major_axis, Angle_minor_axis)) +
        geom_point(aes(fill = relTS_c_scaled), shape = 21) + 
        # geom_point(data = filter(cal.pings, outlier == 1), aes(athw, along),
        #            shape = "+", size = 4) +
        facet_wrap(~txdr_freq, scales = cal.scales) + 
        scale_fill_distiller(name = expression(italic(TS)[rel]),
                             type = "div", palette = "RdBu", limits = c(-1,1)) +
        scale_x_continuous('\nAthwartship Beam Angle (deg)', limits = cal.axis.lims,
                           breaks = seq(min(cal.axis.lims), max(cal.axis.lims), 2)) +
        scale_y_continuous('Alongship Beam Angle (deg)\n',limits = cal.axis.lims,
                           breaks = seq(min(cal.axis.lims), max(cal.axis.lims), 2)) +
        theme_bw() + 
        theme(panel.spacing = unit(1, "lines"),
              strip.background = element_rect(fill = "white"),
              strip.text.x = element_text(face = "bold")) +
        coord_equal()
      
    # Otherwise let the x- and y-axis limits be free and not limited to integer values
    } else {
      
      # Plot relative beam-compensated target strength data #####
      tsc.scatter <- ggplot(cal.pings, aes(Angle_major_axis, Angle_minor_axis)) +
        geom_point(aes(fill = relTS_c_scaled), shape = 21, colour = "gray70") + 
        # geom_point(data = filter(cal.pings, outlier == 1), aes(athw, along),
        #            shape = "+", size = 4) +
        facet_wrap(~txdr_freq, scales = cal.scales) + 
        scale_fill_distiller(name = expression(italic(TS)[rel]),
                             type = "div", palette = "RdBu", limits = c(-1,1)) +
        scale_x_continuous('\nAthwartship Beam Angle (deg)') +
        scale_y_continuous('Alongship Beam Angle (deg)\n') +
        theme_bw() + 
        theme(panel.spacing = unit(1, "lines"),
              strip.background = element_rect(fill = "white"),
              strip.text.x = element_text(face = "bold"))
    }
    
    # Define figure widths based on vessel so that plots are relatively square
    fig.width <- switch(i,
                        "RL"  = 10,
                        "LM"  =  7,
                        "LBC" =  7,
                        "SH"  = 10)
    
    # Save TS_c plot 
    ggsave(here(paste0("Figs/fig_cal_TSrel_scatter_", i, ".png")), tsc.scatter,
           width = fig.width, height = 6)
  }
}

# Save all calibration results to .Rdata
save(list = ls(pattern = "all.output.*"), file = here("Output/cal_results_CW.Rdata"))
