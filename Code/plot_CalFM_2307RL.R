cal.files.fm <- dir_ls(here("Data/Calibration/FM"),
                       pattern = "*.xml")

# Read calibration data from each frequency ---------------------------------------------
cal.fm.38 <- extract_cal_fm(here("Data/Calibration/FM/CalibrationDataFile-D20230627-T195615_38kHz_FM.xml"),
                            vessel.name = "RL",
                            survey.name = survey.name)$cal.res %>% 
  mutate(
    txdr = "ES38-7",
    txdr.freq = 38)

cal.fm.70 <- extract_cal_fm(here("Data/Calibration/FM/CalibrationDataFile-D20230627-T201001_70kHz_FM.xml"),
                             vessel.name = "RL",
                             survey.name = survey.name)$cal.res %>%  
  mutate(
    txdr = "ES70-7C",
    txdr.freq = 70)

cal.fm.120 <- extract_cal_fm(here("Data/Calibration/FM/CalibrationDataFile-D20230627-T202254_120kHz_FM.xml"),
                             vessel.name = "RL",
                             survey.name = survey.name)$cal.res %>% 
  mutate(
    txdr = "ES120-7C",
    txdr.freq = 120)

cal.fm.200 <- extract_cal_fm(here("Data/Calibration/FM/CalibrationDataFile-D20230627-T204110_200kHz_FM.xml"),
                             vessel.name = "RL",
                             survey.name = survey.name)$cal.res %>% 
  mutate(
    txdr = "ES200-7C",
    txdr.freq = 200)

cal.fm.333 <- extract_cal_fm(here("Data/Calibration/FM/CalibrationDataFile-D20230627-T194253_333kHz_FM.xml"),
                             vessel.name = "RL",
                             survey.name = survey.name)$cal.res %>% 
  mutate(
    txdr = "ES333-7C",
    txdr.freq = 333)


# Combine all results -----------------------------------------------------
cal.fm <- cal.fm.38 %>% 
  bind_rows(cal.fm.70) %>%
  bind_rows(cal.fm.120) %>%
  bind_rows(cal.fm.200) %>%
  bind_rows(cal.fm.333) %>% 
  pivot_longer(cols = c(-txdr, -txdr.freq, -freq)) %>% 
  mutate(txdr = fct_reorder(txdr, txdr.freq))


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
ggsave(cal.fm.all, filename = here("Figs/fig_cal_FM_AllFreqs.png"),
       height = 6, width = 10)
