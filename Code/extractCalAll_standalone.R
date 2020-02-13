# Install and load pacman (library management package)
if (!require("pacman")) install.packages("pacman")

# Install and load required packages from CRAN ---------------------------------
pacman::p_load(tidyverse,odbc,lubridate,cowplot,here)

# Install and load required packages from Github -------------------------------
# atm
pacman::p_load_gh("kstierhoff/atm")

vessel.list <- c("Reuben Lasker","Bell M. Shimada")

# Import calibration directories -----------------------------------------------
# Configure ODBC connection
cal.con  <- dbConnect(odbc(), 
                      Driver = "SQL Server", 
                      Server = "161.55.235.187", 
                      Database = "AST", 
                      Trusted_Connection = "True")
# Collect table
dirs.all <- tbl(cal.con,"tbl_CAL_DIRS") %>% collect()

# Close connection
dbDisconnect(cal.con)

# Select only directories from current survey vessel
dirs <- filter(dirs.all, process == 1, ship_name %in% vessel.list) %>% 
  arrange(path)

# Create data frames for storing results
cal.res.all   <- data.frame()
cal.info.all  <- data.frame()
cal.pings.all <- data.frame()

# Configure the progress bar
pb <- winProgressBar(title = "Calibration File Processing Progress", 
                     label = "0% done", min = 0, max = 100, initial = 0)

for (dd in 1:nrow(dirs)) {
  # get calibration file directories
  cal.files <- sort(c(list.files(as.character(dirs$path[dd]),pattern = ".txt", 
                                 full.names = TRUE),
                      list.files(as.character(dirs$path[dd]),pattern = ".xml", 
                                 full.names = TRUE)))
  
  # set survey info
  survey.vessel.temp   <- dirs$ship_name[dd]
  survey.name.temp     <- dirs$survey_name[dd]
  cal.group.temp       <- dirs$group_name[dd]
  
  # process all calibration results in CALIBRATION directory
  for (i in cal.files) {
    # Extract calibration results, information, and pings
    cal <- extract_cal(i, survey.vessel.temp, survey.name.temp, cal.group.temp)
    
    # combine all results
    cal.res.all   <- bind_rows(cal.res.all,   cal$cal.res)
    cal.info.all  <- bind_rows(cal.info.all,  cal$cal.info)
    cal.pings.all <- bind_rows(cal.pings.all, cal$cal.pings)
  }
  
  info <- sprintf("%d%% done", round((dd/nrow(dirs))*100))
  setWinProgressBar(pb, dd/nrow(dirs)*100, label = info)
}
close(pb)

# Format results
cal.res.all   <- arrange(cal.res.all, mdy(cal_date), txdr_freq)
cal.info.all  <- arrange(cal.info.all, mdy_hms(date_time))
cal.pings.all <- arrange(cal.pings.all, date_time)

# Plot transducer results ------------------------------------------------------
cal.res.all <- left_join(cal.res.all, select(cal.info.all, id, vessel_name)) 

cal.plot <- cal.res.all %>% 
  mutate(
    cal_date = mdy(cal_date),
    txdr_type = fct_reorder(txdr_type, txdr_freq)) 

# Summarise beam widths for plotting
bw_summ <- cal.plot %>% 
  group_by(txdr_type) %>% 
  summarise(
    w.mean = round(mean(bm_alon_ba)),
    w.min = w.mean - 2,
    w.max = w.mean + 2)

cal.plot <- left_join(cal.plot, bw_summ)

# write_csv(cal.plot, here::here("Output/cal_time_series_SH_RL_josiah.csv"))

gain <- ggplot(cal.plot, aes(cal_date, bm_txdr_gain, colour = vessel_name)) + 
  geom_line(linetype = 'dashed') + 
  geom_point() +
  geom_point(data = cal.plot, aes(cal_date,bm_txdr_gain), 
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type, nrow = 1) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(On-axis~gain, (italic(G)[0]*', dB re 1')))) +
  theme(strip.text.x = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white"))

# ggsave(gain, filename = here::here("Figs/fig_cal_time_series_gain.png"),
#        width = 15, height = 2)
# 
# sa_corr <- ggplot(cal.plot, aes(cal_date, bm_sa_corr, colour = vessel_name)) +  
#   geom_line(linetype = 'dashed') + 
#   geom_point() + 
#   geom_point(data = cal.plot, aes(cal_date,bm_sa_corr), 
#              shape = 21, size = 3, fill = 'white') +
#   facet_wrap(~txdr_type, nrow = 1) +
#   theme_bw() + 
#   xlab("Date") + 
#   ylab(expression(atop(italic(S)[a]~correction, (italic(S)[a]*corr*', dB re 1')))) +
#   theme(strip.text.x = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# bm_alon_ba <- ggplot(cal.plot, aes(cal_date, bm_alon_ba, colour = vessel_name)) + 
#   geom_line(linetype = 'dashed') + 
#   geom_point() + 
#   geom_point(data = cal.plot, aes(cal_date,bm_alon_ba), 
#              shape = 21, size = 3, fill = 'white') +
#   facet_wrap(~txdr_type, nrow = 1, scales = 'free_y', shrink = F) +
#   theme_bw() + 
#   xlab("Date") + 
#   ylab(expression(atop(Beamwidth~along., (alpha["-3dB"]*', deg.')))) +
#   theme(strip.text.x = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# bm_athw_ba <- ggplot(cal.plot, aes(cal_date, bm_athw_ba, colour = vessel_name)) + 
#   geom_line(linetype = 'dashed') + 
#   geom_point() + 
#   geom_point(data = cal.plot, aes(cal_date, bm_athw_ba),
#              shape = 21, size = 3, fill = 'white') +
#   facet_wrap(~txdr_type, nrow = 1) +
#   theme_bw() + 
#   xlab("Date") + 
#   ylab(expression(atop(Beamwidth~along., (beta["-3dB"]*', deg.')))) +
#   theme(strip.text.x = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# bm_alon_oa <- ggplot(cal.plot, aes(cal_date, bm_alon_oa, colour = vessel_name)) + 
#   geom_line(linetype = 'dashed') + 
#   geom_point() + 
#   geom_point(data = cal.plot, aes(cal_date, bm_alon_oa),
#              shape = 21, size = 3, fill = 'white') +
#   facet_wrap(~txdr_type, nrow = 1) +
#   theme_bw() + 
#   xlab("Date") + 
#   ylab(expression(atop(Angle~offset~athw., (alpha[0]*', deg.')))) +
#   theme(strip.text.x = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# bm_athw_oa <- ggplot(cal.plot,aes(cal_date, bm_athw_oa, colour = vessel_name)) + 
#   geom_line(linetype = 'dashed') + 
#   geom_point() + 
#   geom_point(data = cal.plot, aes(cal_date, bm_athw_oa), 
#              shape = 21, size = 3, fill = 'white') +
#   facet_wrap(~txdr_type, nrow = 1) +
#   theme_bw() + 
#   xlab("Date") +
#   ylab(expression(atop(Angle~offset~along., (beta[0]*', deg.')))) +
#   theme(strip.text.x = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# rms <- ggplot(cal.plot, aes(cal_date, dev_bm_rms, colour = vessel_name)) + 
#   # geom_hline(yintercept = 0.4,linetype = 'longdash',colour = 'blue',alpha = 0.5) +
#   geom_line(linetype = 'dashed') + geom_point() + 
#   geom_point(data = cal.plot, aes(cal_date,dev_bm_rms),
#              shape = 21, size = 3, fill = 'white') +
#   facet_wrap(~txdr_type, nrow = 1) + 
#   scale_y_continuous(limits = c(0, max(cal.plot$dev_bm_rms)*1.1), expand = c(0,0)) +
#   theme_bw() + xlab("Date") + ylab(expression(atop(RMS,(dB)))) +
#   theme(strip.text.x = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# plot.all <- plot_grid(gain, sa_corr, bm_alon_ba, bm_athw_ba, 
#                       bm_alon_oa, bm_athw_oa, rms,
#                       align = "v", ncol = 1, 
#                       labels = c("a)","b)","c)","d)","e)","f)","g)"))
# 
# ggsave(plot.all,
#        filename = here::here("Figs/fig_cal_time_series_josiah.png"),
#        height = 15, width = 11)
# 
# # Combine beamwidths and angle offsets into the same plot
# ba_all <- ggplot(cal.plot, aes(cal_date, bm_alon_ba)) + 
#   geom_line(colour = 'blue', linetype = 'dashed') + 
#   geom_point(colour = 'blue') +
#   geom_point(data  = cal.plot, aes(cal_date, bm_alon_ba),
#              shape = 21, size = 3, fill = 'white', colour = 'blue') +
#   geom_line(data   = cal.plot, aes(cal_date, bm_athw_ba), 
#             colour = 'red', linetype = 'dashed') +
#   geom_point(data  = cal.plot, aes(cal_date, bm_athw_ba), colour = 'red') +
#   geom_point(data  = cal.plot, aes(cal_date, bm_athw_ba), 
#              shape = 21, size = 3, fill = 'white', colour = 'red') +
#   geom_point(data  = cal.plot, aes(cal_date, w.max), alpha = 0) +
#   geom_point(data  = cal.plot, aes(cal_date, w.min), alpha = 0) +
#   facet_wrap(~txdr_type, nrow = 1, scales = 'free_y', shrink = F) +
#   theme_bw() + 
#   xlab("Date") + 
#   ylab(expression(atop(Beamwidth, (alpha["-3dB"]*", "*beta["-3dB"]*', deg.')))) +
#   theme(strip.text.x     = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# oa_all <- ggplot(cal.plot,aes(cal_date,bm_alon_oa)) +
#   geom_hline(yintercept = 0, colour = 'gray50') +
#   geom_line(colour = 'blue', linetype = 'dashed') + 
#   geom_point(colour = 'blue') +
#   geom_point(data  = cal.plot, aes(cal_date, bm_alon_oa), 
#              shape = 21, size = 3, fill = 'white', colour = 'blue') +
#   geom_line(data   = cal.plot, aes(cal_date, bm_athw_oa), 
#             colour = 'red', linetype = 'dashed') +
#   geom_point(data  = cal.plot, aes(cal_date,bm_athw_oa), colour = 'red') +
#   geom_point(data  = cal.plot, aes(cal_date, bm_athw_oa), 
#              shape = 21, size = 3, fill = 'white', colour = 'red') +
#   facet_wrap(~txdr_type,nrow = 1) + 
#   ylim(-0.3,0.3) +
#   theme_bw() + 
#   xlab("Date") + 
#   ylab(expression(atop(Angle~offset, (alpha[0]*', '*beta[0]*', deg.')))) +
#   theme(strip.text.x = element_text(face = "bold"),
#         strip.background = element_rect(fill = "white"))
# 
# plot.all.combo <- plot_grid(gain,sa_corr, ba_all, oa_all, rms,
#                             align = "v", ncol = 1, 
#                             labels = c("a)","b)","c)","d)","e)"))
# 
# ggsave(plot.all.combo, 
#        filename = here::here("Figs/fig_cal_time_series_combo_josiah.png"),
#        height = 15, width = 13)
# 
