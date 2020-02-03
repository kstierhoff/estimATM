library(tidyverse)
library(cowplot)

# Create a vector of column names
fm.names <- c("frequency","gain","Sa.corr","ba.alon","ba.athw","oa.alon", "oa.athw","rms")

# Read calibration data from each frequency ---------------------------------------------
cal.fm.70 <- read_csv(here::here("Data/Calibration/FM/70kHz.csv"),
                   col_names = fm.names) %>% 
  mutate(
    txdr = "ES70-7C",
    txdr.freq = 70,
    frequency = frequency/1000)

cal.fm.120 <- read_csv(here::here("Data/Calibration/FM/120kHz.csv"),
                      col_names = fm.names) %>% 
  mutate(
    txdr = "ES120-7C",
    txdr.freq = 120,
    frequency = frequency/1000)

cal.fm.200 <- read_csv(here::here("Data/Calibration/FM/200kHz.csv"),
                      col_names = fm.names) %>% 
  mutate(
    txdr = "ES200-7C",
    txdr.freq = 200,
    frequency = frequency/1000)

cal.fm.333 <- read_csv(here::here("Data/Calibration/FM/333kHz.csv"),
                      col_names = fm.names) %>% 
  mutate(
    txdr = "ES333-7C",
    txdr.freq = 333,
    frequency = frequency/1000)


# Combine all results -----------------------------------------------------
cal.fm <- cal.fm.70 %>% 
  bind_rows(cal.fm.120) %>%
  bind_rows(cal.fm.200) %>%
  bind_rows(cal.fm.333) %>% 
  pivot_longer(cols = c(-txdr, -txdr.freq, -frequency)) %>% 
  mutate(txdr = fct_reorder(txdr, txdr.freq))


# Plot results ------------------------------------------------------------
# Gain
cal.gain <- ggplot(filter(cal.fm, name == "gain"), aes(frequency, value)) + 
  geom_point() + facet_wrap(~txdr, scales = "free_x", nrow = 1) +
  labs(y = expression(italic(G)[0]*' '*(dB))) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(face = "bold"),
        strip.background = element_blank())

# Beam angles
cal.ba <- ggplot() + 
  geom_point(data = filter(cal.fm, name == "ba.alon"), aes(frequency, value), colour = "cyan") +
  geom_point(data = filter(cal.fm, name == "ba.athw"), aes(frequency, value), colour = "magenta") +
  facet_wrap(~txdr, scales = "free_x", nrow = 1) +
  labs(y = expression(alpha["-3dB"]*", "*beta["-3dB"]*''*' '*(degree))) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        strip.background = element_blank())

# Offset angles
cal.oa <- ggplot() + 
  geom_point(data = filter(cal.fm, name == "oa.alon"), aes(frequency, value), colour = "cyan") +
  geom_point(data = filter(cal.fm, name == "oa.athw"), aes(frequency, value), colour = "magenta") +
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
