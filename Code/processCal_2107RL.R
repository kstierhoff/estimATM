# Process calibration results -----------------------------------------------------
## Process Long Beach Carnage
if ("LBC" %in% cal.vessels) {
  
  # Create output directory ------------------------------------------------------
  dir_create(here("Output/Calibration/LBC"))
  
  # Calibration directory -----------------------------------------------
  dir.lbc <- here("Data/Calibration/LBC")
  
  # Get calibration files
  cal.files.lbc <- sort(c(list.files(dir.lbc,pattern = ".txt", 
                                     full.names = TRUE),
                          list.files(dir.lbc,pattern = ".xml", 
                                     full.names = TRUE)))
  
  # set survey info
  survey.vessel.temp   <- "LBC"
  survey.name.temp     <- survey.name
  cal.group.temp       <- cal.group
  
  # Create data frames for storing results
  cal.res.lbc   <- data.frame()
  cal.info.lbc  <- data.frame()
  cal.pings.lbc <- data.frame()
  
  # process all calibration results in CALIBRATION directory
  for (i in cal.files.lbc) {
    # Extract calibration results, information, and pings
    cal <- extract_cal(i, survey.vessel.temp, survey.name.temp, cal.group.temp)
    
    # combine all results
    cal.res.lbc   <- bind_rows(cal.res.lbc,   cal$cal.res)
    cal.info.lbc  <- bind_rows(cal.info.lbc,  cal$cal.info)
    cal.pings.lbc <- bind_rows(cal.pings.lbc, cal$cal.pings)
  }
  
  # Format results
  cal.res.lbc   <- arrange(cal.res.lbc, mdy(cal_date), txdr_freq)
  cal.info.lbc  <- arrange(cal.info.lbc, mdy_hms(date_time))
  cal.pings.lbc <- arrange(cal.pings.lbc, date_time)
  
  # write all results to file
  save(cal.res.lbc, cal.info.lbc, cal.pings.lbc, cal.files.lbc,
       file = here("Data/Calibration/LBC/cal_results_lbc.Rdata"))
  
  # Get results from current survey ---------------------------------------------
  
  # create data frame for calibration parameters
  cal.params.lbc <- cal.res.lbc %>% 
    select(txdr_freq,txdr_type,txdr_sn,gpt_power,gpt_pd,
           txdr_gain,txdr_sa_corr,gpt_rcr_bw,gpt_si,
           txdr_2way_ba,env_alpha,txdr_alon_ang_sens,
           txdr_athw_ang_sens,txdr_alon_ba,txdr_athw_ba,
           txdr_alon_oa,txdr_athw_oa,target_ts) 
  
  # add noise estimates
  # if noise isn't measured, enter -999 in the User Inputs, else use noise estimates
  if (is.na(cal.noise["LBC"])) { 
    cal.params.lbc$noise <- rep("N/A", length(cal.files.lbc))
  } else {
    cal.params.lbc$noise <- unlist(cal.noise[cal.vessels])
  }
  
  # create data frame for beam model results
  bm.res.lbc <- cal.res.lbc %>% 
    select(txdr_freq,bm_txdr_gain,bm_sa_corr,dev_bm_rms,
           bm_alon_ba,bm_athw_ba,bm_alon_oa,bm_athw_oa) 
  
  # create a data frame for echosounder settings
  echo.settings.lbc <- cal.res.lbc %>% 
    select(txdr_freq,gpt_pd,gpt_si,gpt_rcr_bw,gpt_power,txdr_z,env_alpha) 
  
  # create names for cal parameters and beam model results
  names(cal.params.lbc) <- c("Frequency","Model","Serial Number","Transmit Power ($p_\\mathrm{et}$)",
                             "Pulse Duration ($\\tau$)","On-axis Gain ($G_0$)",
                             "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)",
                             "Bandwidth ($W_\\mathrm{f}$)","Sample Interval",
                             "Eq. Two-way Beam Angle ($\\mathrm{\\Psi}$)",
                             "Absorption Coefficient ($\\alpha_\\mathrm{f}$)",
                             "Angle Sensitivity Along. ($\\mathrm{\\Lambda}_{\\alpha}$)",
                             "Angle Sensitivity Athw. ($\\mathrm{\\Lambda}_{\\beta}$)",
                             "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",
                             "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",
                             "Angle Offset Along. ($\\alpha_{0}$)","Angle Offset Athw. ($\\beta_{0}$)",
                             "Theoretical TS ($TS_\\mathrm{theory}$)",
                             "Ambient Noise")
  
  names(bm.res.lbc) <- c("Frequency","On-axis Gain ($G_0$)",
                         "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)","RMS",
                         "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",
                         "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",
                         "Angle Offset Along. ($\\alpha_{0}$)",
                         "Angle Offset Athw. ($\\beta_{0}$)")
  
  names(echo.settings.lbc) <- c("Frequency","Pulse Duration ($\\mu s$)","Sample Interval (m)",
                                "Bandwidth (Hz)","Transmit Power (W)",
                                "Transducer Depth (m)","Absorption Coefficient (dB km$\\^{-1}$)")
  
  # cast output by frequency and transducer model number
  param.output.lbc <- cal.params.lbc %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  bm.output.lbc <- bm.res.lbc %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  # add a column with parameter units
  param.units.lbc <- data.frame(Units = c(" "," ","W","ms","dB re 1","dB re 1","Hz","m","dB re 1 sr","dB km$^{-1}$",
                                          "Elec.$^\\circ$/Geom.$^\\circ$","Elec.$^\\circ$/Geom.$^\\circ$",
                                          "deg","deg","deg","deg","dB re 1 m$^{2}$","dB re 1 W"))
  
  # add a column with beam model units
  bm.units.lbc <- data.frame(Units = c("dB re 1","dB re 1","dB","deg","deg","deg","deg"))
  
  # add units to output and arrange columns
  param.output.lbc <- bind_cols(param.output.lbc, param.units.lbc) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  bm.output.lbc <- bind_cols(bm.output.lbc, bm.units.lbc) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  # combine results data frames
  all.output.lbc <- rbind(param.output.lbc, bm.output.lbc) %>% 
    rename(" " = "Frequency ($f$, kHz)")
  
  # save output to .Rdata and CSV
  save(all.output.lbc,
       file = here("Output/cal_output_table_LBC.Rdata"))
  
  write_csv(all.output.lbc,
            file = here("Output/cal_output_table_LBC.csv"))
}

## Process Lisa Marie
if ("LM" %in% cal.vessels) {
  
  # Create output directory ------------------------------------------------------
  dir_create(here("Output/Calibration/LM"))
  
  # Calibration directory -----------------------------------------------
  dir.lm <- here("Data/Calibration/LM")
  
  # Get calibration files
  cal.files.lm <- sort(c(list.files(dir.lm,pattern = ".txt", 
                                    full.names = TRUE),
                         list.files(dir.lm,pattern = ".xml", 
                                    full.names = TRUE)))
  
  # set survey info
  survey.vessel.temp   <- "LM"
  survey.name.temp     <- survey.name
  cal.group.temp       <- cal.group
  
  # Create data frames for storing results
  cal.res.lm   <- data.frame()
  cal.info.lm  <- data.frame()
  cal.pings.lm <- data.frame()
  
  # process all calibration results in CALIBRATION directory
  for (i in cal.files.lm) {
    # Extract calibration results, information, and pings
    cal <- extract_cal(i, survey.vessel.temp, survey.name.temp, cal.group.temp)
    
    # combine all results
    cal.res.lm   <- bind_rows(cal.res.lm,   cal$cal.res)
    cal.info.lm  <- bind_rows(cal.info.lm,  cal$cal.info)
    cal.pings.lm <- bind_rows(cal.pings.lm, cal$cal.pings)
  }
  
  # Format results
  cal.res.lm   <- arrange(cal.res.lm, mdy(cal_date), txdr_freq)
  cal.info.lm  <- arrange(cal.info.lm, mdy_hms(date_time))
  cal.pings.lm <- arrange(cal.pings.lm, date_time)
  
  # write all results to file
  save(cal.res.lm, cal.info.lm, cal.pings.lm, cal.files.lm,
       file = here("Data/Calibration/LBC/cal_results_lm.Rdata"))
  
  # Get results from current survey ---------------------------------------------
  
  # create data frame for calibration parameters
  cal.params.lm <- cal.res.lm %>% 
    select(txdr_freq,txdr_type,txdr_sn,gpt_power,gpt_pd,
           txdr_gain,txdr_sa_corr,gpt_rcr_bw,gpt_si,
           txdr_2way_ba,env_alpha,txdr_alon_ang_sens,
           txdr_athw_ang_sens,txdr_alon_ba,txdr_athw_ba,
           txdr_alon_oa,txdr_athw_oa,target_ts) 
  
  # add noise estimates
  # if noise isn't measured, enter -999 in the User Inputs, else use noise estimates
  if (is.na(cal.noise["LM"])) { 
    cal.params.lm$noise <- rep("N/A", length(cal.files.lm))
  } else {
    cal.params.lm$noise <- unlist(cal.noise[cal.vessels])
  }
  
  # create data frame for beam model results
  bm.res.lm <- cal.res.lm %>% 
    select(txdr_freq,bm_txdr_gain,bm_sa_corr,dev_bm_rms,
           bm_alon_ba,bm_athw_ba,bm_alon_oa,bm_athw_oa) 
  
  # create a data frame for echosounder settings
  echo.settings.lm <- cal.res.lm %>% 
    select(txdr_freq,gpt_pd,gpt_si,gpt_rcr_bw,gpt_power,txdr_z,env_alpha) 
  
  # create names for cal parameters and beam model results
  names(cal.params.lm) <- c("Frequency","Model","Serial Number","Transmit Power ($p_\\mathrm{et}$)",
                            "Pulse Duration ($\\tau$)","On-axis Gain ($G_0$)",
                            "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)",
                            "Bandwidth ($W_\\mathrm{f}$)","Sample Interval",
                            "Eq. Two-way Beam Angle ($\\mathrm{\\Psi}$)",
                            "Absorption Coefficient ($\\alpha_\\mathrm{f}$)",
                            "Angle Sensitivity Along. ($\\mathrm{\\Lambda}_{\\alpha}$)",
                            "Angle Sensitivity Athw. ($\\mathrm{\\Lambda}_{\\beta}$)",
                            "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",
                            "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",
                            "Angle Offset Along. ($\\alpha_{0}$)","Angle Offset Athw. ($\\beta_{0}$)",
                            "Theoretical TS ($TS_\\mathrm{theory}$)",
                            "Ambient Noise")
  
  names(bm.res.lm) <- c("Frequency","On-axis Gain ($G_0$)",
                        "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)","RMS",
                        "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",
                        "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",
                        "Angle Offset Along. ($\\alpha_{0}$)",
                        "Angle Offset Athw. ($\\beta_{0}$)")
  
  names(echo.settings.lm) <- c("Frequency","Pulse Duration ($\\mu s$)","Sample Interval (m)",
                               "Bandwidth (Hz)","Transmit Power (W)",
                               "Transducer Depth (m)","Absorption Coefficient (dB km$\\^{-1}$)")
  
  # cast output by frequency and transducer model number
  param.output.lm <- cal.params.lm %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  bm.output.lm <- bm.res.lm %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  # add a column with parameter units
  param.units.lm <- data.frame(Units = c(" "," ","W","ms","dB re 1","dB re 1","Hz","m","dB re 1 sr","dB km$^{-1}$",
                                         "Elec.$^\\circ$/Geom.$^\\circ$","Elec.$^\\circ$/Geom.$^\\circ$",
                                         "deg","deg","deg","deg","dB re 1 m$^{2}$","dB re 1 W"))
  
  # add a column with beam model units
  bm.units.lm <- data.frame(Units = c("dB re 1","dB re 1","dB","deg","deg","deg","deg"))
  
  # add units to output and arrange columns
  param.output.lm <- bind_cols(param.output.lm, param.units.lm) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  bm.output.lm <- bind_cols(bm.output.lm, bm.units.lm) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  # combine results data frames
  all.output.lm <- rbind(param.output.lm, bm.output.lm) %>% 
    rename(" " = "Frequency ($f$, kHz)")
  
  # save output to .Rdata and CSV
  save(all.output.lm,
       file = here("Output/cal_output_table_LM.Rdata"))
  
  write_csv(all.output.lm,
            file = here("Output/cal_output_table_LM.csv"))
}

## Process Carranza
if ("JCF" %in% cal.vessels) {
  
  # Create output directory ------------------------------------------------------
  dir_create(here("Output/Calibration/JCF"))
  
  # Calibration directory -----------------------------------------------
  dir.jcf <- here("Data/Calibration/JCF")
  
  # Get calibration files
  cal.files.jcf <- sort(c(list.files(dir.jcf,pattern = ".txt", 
                                     full.names = TRUE),
                          list.files(dir.jcf,pattern = ".xml", 
                                     full.names = TRUE)))
  
  # set survey info
  survey.vessel.temp   <- "JCF"
  survey.name.temp     <- survey.name
  cal.group.temp       <- "INAPESCA"
  
  # Create data frames for storing results
  cal.res.jcf   <- data.frame()
  cal.info.jcf  <- data.frame()
  cal.pings.jcf <- data.frame()
  
  # process all calibration results in CALIBRATION directory
  for (i in cal.files.jcf) {
    # Extract calibration results, information, and pings
    cal <- extract_cal(i, survey.vessel.temp, survey.name.temp, cal.group.temp)
    
    # combine all results
    cal.res.jcf   <- bind_rows(cal.res.jcf,   cal$cal.res)
    cal.info.jcf  <- bind_rows(cal.info.jcf,  cal$cal.info)
    cal.pings.jcf <- bind_rows(cal.pings.jcf, cal$cal.pings)
  }
  
  # Format results
  cal.res.jcf   <- arrange(cal.res.jcf, mdy(cal_date), txdr_freq)
  cal.info.jcf  <- arrange(cal.info.jcf, mdy_hms(date_time))
  cal.pings.jcf <- arrange(cal.pings.jcf, date_time)
  
  # write all results to file
  save(cal.res.jcf, cal.info.jcf, cal.pings.jcf, cal.files.jcf,
       file = here("Data/Calibration/LBC/cal_results_jcf.Rdata"))
  
  # Get results from current survey ---------------------------------------------
  
  # create data frame for calibration parameters
  cal.params.jcf <- cal.res.jcf %>% 
    select(txdr_freq,txdr_type,txdr_sn,gpt_power,gpt_pd,
           txdr_gain,txdr_sa_corr,gpt_rcr_bw,gpt_si,
           txdr_2way_ba,env_alpha,txdr_alon_ang_sens,
           txdr_athw_ang_sens,txdr_alon_ba,txdr_athw_ba,
           txdr_alon_oa,txdr_athw_oa,target_ts) 
  
  # add noise estimates
  # if noise isn't measured, enter -999 in the User Inputs, else use noise estimates
  if (is.na(cal.noise["JCF"])) { 
    cal.params.jcf$noise <- rep("N/A", length(cal.files.jcf))
  } else {
    cal.params.jcf$noise <- unlist(cal.noise[cal.vessels])
  }
  
  # create data frame for beam model results
  bm.res.jcf <- cal.res.jcf %>% 
    select(txdr_freq,bm_txdr_gain,bm_sa_corr,dev_bm_rms,
           bm_alon_ba,bm_athw_ba,bm_alon_oa,bm_athw_oa) 
  
  # create a data frame for echosounder settings
  echo.settings.jcf <- cal.res.jcf %>% 
    select(txdr_freq,gpt_pd,gpt_si,gpt_rcr_bw,gpt_power,txdr_z,env_alpha) 
  
  # create names for cal parameters and beam model results
  names(cal.params.jcf) <- c("Frequency","Model","Serial Number","Transmit Power ($p_\\mathrm{et}$)",
                             "Pulse Duration ($\\tau$)","On-axis Gain ($G_0$)",
                             "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)",
                             "Bandwidth ($W_\\mathrm{f}$)","Sample Interval",
                             "Eq. Two-way Beam Angle ($\\mathrm{\\Psi}$)",
                             "Absorption Coefficient ($\\alpha_\\mathrm{f}$)",
                             "Angle Sensitivity Along. ($\\mathrm{\\Lambda}_{\\alpha}$)",
                             "Angle Sensitivity Athw. ($\\mathrm{\\Lambda}_{\\beta}$)",
                             "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",
                             "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",
                             "Angle Offset Along. ($\\alpha_{0}$)","Angle Offset Athw. ($\\beta_{0}$)",
                             "Theoretical TS ($TS_\\mathrm{theory}$)",
                             "Ambient Noise")
  
  names(bm.res.jcf) <- c("Frequency","On-axis Gain ($G_0$)",
                         "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)","RMS",
                         "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",
                         "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",
                         "Angle Offset Along. ($\\alpha_{0}$)",
                         "Angle Offset Athw. ($\\beta_{0}$)")
  
  names(echo.settings.jcf) <- c("Frequency","Pulse Duration ($\\mu s$)","Sample Interval (m)",
                                "Bandwidth (Hz)","Transmit Power (W)",
                                "Transducer Depth (m)","Absorption Coefficient (dB km$\\^{-1}$)")
  
  # cast output by frequency and transducer model number
  param.output.jcf <- cal.params.jcf %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  bm.output.jcf <- bm.res.jcf %>% 
    pivot_longer(-Frequency, names_to = "variable", values_to = "value", values_transform = as.character) %>% 
    pivot_wider(names_from = Frequency, values_from = value)
  
  # add a column with parameter units
  param.units.jcf <- data.frame(Units = c(" "," ","W","ms","dB re 1","dB re 1","Hz","m","dB re 1 sr","dB km$^{-1}$",
                                          "Elec.$^\\circ$/Geom.$^\\circ$","Elec.$^\\circ$/Geom.$^\\circ$",
                                          "deg","deg","deg","deg","dB re 1 m$^{2}$","dB re 1 W"))
  
  # add a column with beam model units
  bm.units.jcf <- data.frame(Units = c("dB re 1","dB re 1","dB","deg","deg","deg","deg"))
  
  # add units to output and arrange columns
  param.output.jcf <- bind_cols(param.output.jcf, param.units.jcf) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  bm.output.jcf <- bind_cols(bm.output.jcf, bm.units.jcf) %>% 
    select(variable, Units, everything()) %>% 
    rename("Frequency ($f$, kHz)" = variable)
  
  # combine results data frames
  all.output.jcf <- rbind(param.output.jcf, bm.output.jcf) %>% 
    rename(" " = "Frequency ($f$, kHz)")
  
  # save output to .Rdata and CSV
  save(all.output.jcf,
       file = here("Output/cal_output_table_JCF.Rdata"))
  
  write_csv(all.output.lbc,
            file = here("Output/cal_output_table_JCF.csv"))
}

## Process Saildrone
# Create a column with beam model units
bm.units.sd <- data.frame(Units = c("", "", "dB re 1 sr", "dB re 1 m$^{2}$", 
                                    "dB re 1","dB re 1","dB","deg","deg","deg","deg"))

# bm.units.sd <- data.frame(Units = c("kHz", "", "", "dB re 1 sr", "dB re 1 m$^{2}$", 
#                                     "dB re 1","dB re 1","dB","deg","deg","deg","deg"))

all.output.sd <- tibble::tribble(
  ~" ", ~"1036 (38)", ~"1036 (200)", ~"1055 (38)", ~"1055 (200)", ~"1059 (38)", ~"1059 (200)",
  # "Frequency", "38", "200", "38", "200", "38", "200",
  "Echosounder SN", "268641-07", "268641-08", "266972-07", "266972-08", "268632-07", "268632-08",
  "Transducer SN", "110", "110", "127", "127", "131", "131",
  "Eq. Two-way Beam Angle ($\\mathrm{\\Psi}$)", "-13.0", "-11.8", "-12.7", "-11.7", "-12.9",    "-12.0",
  "Theoretical TS ($TS_\\mathrm{theory}$)",    "-42.39",    "-38.83",    "-42.40",    "-38.85",    "-42.40",    "-38.85",
  "On-axis Gain ($G_0$)", "19.06", "18.52", "19.30", "18.92", "18.99", "18.96",
  "$S_\\mathrm{a}$ Correction ($S_\\mathrm{a}\\mathrm{corr}$)",  "0.01",  "0.04",  "-0.04", "0.11", "-0.02", "0.05",
  "RMS",  "0.12",  "0.33",  "0.21",  "0.52",  "0.21",  "0.47",
  "3-dB Beamwidth Along. ($\\alpha_\\mathrm{-3dB}$)",  "17.0",  "19.1",  "17.7",  "19.3",  "17.5", "20.0",
  "3-dB Beamwidth Athw. ($\\beta_\\mathrm{-3dB}$)",  "17.1",  "19.9",  "17.6",  "20.2",  "16.8", "18.3",
  "Angle Offset Along. ($\\alpha_{0}$)",   "0.3",   "0.0",   "0.2",   "0.4",  "0.1",  "0.6",
  "Angle Offset Athw. ($\\beta_{0}$)",   "-0.2",   "-0.1",   "0.1",  "-0.2",   "-0.5", "0.20"
) %>% 
  bind_cols(bm.units.sd) %>% 
  select(1, Units, everything())

# save output to .Rdata and CSV
save(all.output.sd,
     file = here("Output/cal_output_table_Saildrone.Rdata"))

write.csv(all.output.sd,
          file = here("Output/cal_output_table_Saildrone.csv"), 
          quote = F, row.names = F)
