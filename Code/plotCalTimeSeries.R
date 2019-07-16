# Plot transducer results #####
cal.res.all <- left_join(cal.res.all, select(cal.info.all, id, vessel_name)) 

cal.plot <- cal.res.all %>% 
  mutate(
    cal_date = mdy(cal_date),
    txdr_type = fct_reorder(txdr_type, txdr_freq)) 

cal.plot.used <- cal.plot %>% 
  filter(vessel_name %in% survey.vessel.long,
         between(cal_date,
                 ymd(cal.plot.date) - days(cal.window),
                 ymd(cal.plot.date) + days(cal.window)))

bw_summ <- cal.plot %>% 
  group_by(txdr_type) %>% 
  summarise(
    w.mean = round(mean(bm_alon_ba)),
    w.min = w.mean - 2,
    w.max = w.mean + 2)

cal.plot <- left_join(cal.plot, bw_summ)

gain <- ggplot(cal.plot, aes(cal_date, bm_txdr_gain)) + 
  geom_line(colour = 'gray50',linetype = 'dashed') + 
  geom_point() +
  geom_point(data = cal.plot.used, aes(cal_date,bm_txdr_gain), 
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type, nrow = 1) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(On-axis~gain, (italic(G)[0]*', dB re 1')))) +
  theme(strip.text.x = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white"))

sa_corr <- ggplot(cal.plot, aes(cal_date, bm_sa_corr)) +  
  geom_line(colour = 'gray50', linetype = 'dashed') + 
  geom_point() + 
  geom_point(data = cal.plot.used, aes(cal_date,bm_sa_corr), 
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type, nrow = 1) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(italic(S)[a]~correction, (italic(S)[a]*corr*', dB re 1')))) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

bm_alon_ba <- ggplot(cal.plot, aes(cal_date, bm_alon_ba)) + 
  geom_line(colour = 'gray50', linetype = 'dashed') + 
  geom_point() + 
  geom_point(data = cal.plot.used, aes(cal_date,bm_alon_ba), 
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type, nrow = 1, scales = 'free_y', shrink = F) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(Beamwidth~along., (alpha["-3dB"]*', deg.')))) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

bm_athw_ba <- ggplot(cal.plot, aes(cal_date, bm_athw_ba)) + 
  geom_line(colour = 'gray50', linetype = 'dashed') + 
  geom_point() + 
  geom_point(data = cal.plot.used, aes(cal_date, bm_athw_ba),
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type,nrow = 1) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(Beamwidth~along., (beta["-3dB"]*', deg.')))) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

bm_alon_oa <- ggplot(cal.plot, aes(cal_date, bm_alon_oa)) + 
  geom_line(colour = 'gray50',linetype = 'dashed') + 
  geom_point() + 
  geom_point(data = cal.plot.used, aes(cal_date, bm_alon_oa),
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type, nrow = 1) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(Angle~offset~athw., (alpha[0]*', deg.')))) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

bm_athw_oa <- ggplot(cal.plot,aes(cal_date, bm_athw_oa)) + 
  geom_line(colour = 'gray50', linetype = 'dashed') + 
  geom_point() + 
  geom_point(data = cal.plot.used, aes(cal_date, bm_athw_oa), 
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type, nrow = 1) +
  theme_bw() + 
  xlab("Date") +
  ylab(expression(atop(Angle~offset~along., (beta[0]*', deg.')))) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

rms <- ggplot(cal.plot, aes(cal_date, dev_bm_rms)) + 
  # geom_hline(yintercept = 0.4,linetype = 'longdash',colour = 'blue',alpha = 0.5) +
  geom_line(colour = 'gray50', linetype = 'dashed') + geom_point() + 
  geom_point(data = cal.plot.used, aes(cal_date,dev_bm_rms),
             shape = 21, size = 3, fill = 'white') +
  facet_wrap(~txdr_type, nrow = 1) + 
  scale_y_continuous(limits = c(0, max(cal.plot$dev_bm_rms)*1.1), expand = c(0,0)) +
  theme_bw() + xlab("Date") + ylab(expression(atop(RMS,(dB)))) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

plot.all <- plot_grid(gain, sa_corr, bm_alon_ba, bm_athw_ba, 
                      bm_alon_oa, bm_athw_oa, rms,
                      align = "v", ncol = 1, 
                      labels = c("a)","b)","c)","d)","e)","f)","g)"))

ggsave(plot.all,
       filename = here("Figs/fig_cal_time_series.png"),
       height = 15, width = 11)

# Combine beamwidths and angle offsets into the same plot
ba_all <- ggplot(cal.plot, aes(cal_date, bm_alon_ba)) + 
  geom_line(colour = 'blue', linetype = 'dashed') + 
  geom_point(colour = 'blue') +
  geom_point(data  = cal.plot.used, aes(cal_date, bm_alon_ba),
             shape = 21, size = 3, fill = 'white', colour = 'blue') +
  geom_line(data   = cal.plot, aes(cal_date, bm_athw_ba), 
            colour = 'red', linetype = 'dashed') +
  geom_point(data  = cal.plot, aes(cal_date, bm_athw_ba), colour = 'red') +
  geom_point(data  = cal.plot.used, aes(cal_date, bm_athw_ba), 
             shape = 21, size = 3, fill = 'white', colour = 'red') +
  geom_point(data  = cal.plot, aes(cal_date, w.max), alpha = 0) +
  geom_point(data  = cal.plot, aes(cal_date, w.min), alpha = 0) +
  facet_wrap(~txdr_type, nrow = 1, scales = 'free_y', shrink = F) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(Beamwidth, (alpha["-3dB"]*", "*beta["-3dB"]*', deg.')))) +
  theme(strip.text.x     = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

oa_all <- ggplot(cal.plot,aes(cal_date,bm_alon_oa)) +
  geom_hline(yintercept = 0, colour = 'gray50') +
  geom_line(colour = 'blue', linetype = 'dashed') + 
  geom_point(colour = 'blue') +
  geom_point(data  = cal.plot.used, aes(cal_date, bm_alon_oa), 
             shape = 21, size = 3, fill = 'white', colour = 'blue') +
  geom_line(data   = cal.plot, aes(cal_date, bm_athw_oa), 
            colour = 'red', linetype = 'dashed') +
  geom_point(data  = cal.plot, aes(cal_date,bm_athw_oa), colour = 'red') +
  geom_point(data  = cal.plot.used, aes(cal_date, bm_athw_oa), 
             shape = 21, size = 3, fill = 'white', colour = 'red') +
  facet_wrap(~txdr_type,nrow = 1) + 
  ylim(-0.3,0.3) +
  theme_bw() + 
  xlab("Date") + 
  ylab(expression(atop(Angle~offset, (alpha[0]*', '*beta[0]*', deg.')))) +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

plot.all.combo <- plot_grid(gain,sa_corr, ba_all, oa_all, rms,
                      align = "v", ncol = 1, 
                      labels = c("a)","b)","c)","d)","e)"))

ggsave(plot.all.combo, 
       filename = here("Figs/fig_cal_time_series_combo.png"),
       height = 15, width = 13)
