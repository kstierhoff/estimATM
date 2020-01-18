library(tidyverse)
library(patchwork)

fm.names <- c("frequency","gain","Sa.corr","ba.alon","ba.athw","oa.alon", "oa.athw","rms")

# Calibration plots -------------------------------------------------------
# 70 kHz
cal.fm <- read_csv(here::here("Data/Calibration/FM/70kHz.csv"),
                  col_names = fm.names) %>% 
  mutate(frequency = frequency/1000)

p1 <- ggplot(cal.fm, aes(frequency, gain)) +
  geom_point(size = 1) +
  ylab(expression((italic(G)[0]*', dB re 1'))) +
  labs(title = " 70 kHz") +
  theme_bw() +
  theme(axis.title.x = element_blank())

p2 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, ba.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, ba.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Beamwidth, (alpha["-3dB"]*", "*beta["-3dB"]*', deg.')))) +
  theme_bw() +
  theme(axis.title.x = element_blank())

p3 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, oa.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, oa.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Angle~offset, (alpha[0]*', '*beta[0]*', deg.')))) + 
  xlab("Frequency (kHz)") +
  theme_bw()

p.70 <- plot_grid(p1, p2, p3, ncol = 1, align = "v")

ggsave(p.70, filename = here("Figs/fig_cal_FM_70kHz.png"))

# 120 kHz
cal.fm <- read_csv(here::here("Data/Calibration/FM/120kHz.csv"),
                   col_names = fm.names) %>% 
  mutate(frequency = frequency/1000)

p1 <- ggplot(cal.fm, aes(frequency, gain)) +
  geom_point(size = 1) +
  ylab(expression(atop(On-axis~gain, (italic(G)[0]*', dB re 1')))) +
  labs(title = "120 kHz") +
  theme_bw() +
  theme(axis.title.x = element_blank())

p2 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, ba.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, ba.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Beamwidth, (alpha["-3dB"]*", "*beta["-3dB"]*', deg.')))) +
  theme_bw() +
  theme(axis.title.x = element_blank())

p3 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, oa.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, oa.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Angle~offset, (alpha[0]*', '*beta[0]*', deg.')))) + 
  xlab("Frequency (kHz)") +
  theme_bw()

p.120 <- plot_grid(p1, p2, p3, ncol = 1, align = "v")

ggsave(p.120, filename = here("Figs/fig_cal_FM_120kHz.png"))

# 200 kHz
cal.fm <- read_csv(here::here("Data/Calibration/FM/200kHz.csv"),
                   col_names = fm.names) %>% 
  mutate(frequency = frequency/1000)

p1 <- ggplot(cal.fm, aes(frequency, gain)) +
  geom_point(size = 1) +
  ylab(expression(atop(On-axis~gain, (italic(G)[0]*', dB re 1')))) +
  labs(title = "200 kHz") +
  theme_bw() +
  theme(axis.title.x = element_blank())

p2 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, ba.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, ba.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Beamwidth, (alpha["-3dB"]*", "*beta["-3dB"]*', deg.')))) +
  theme_bw() +
  theme(axis.title.x = element_blank())

p3 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, oa.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, oa.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Angle~offset, (alpha[0]*', '*beta[0]*', deg.')))) + 
  xlab("Frequency (kHz)") +
  theme_bw()

p.200 <- plot_grid(p1, p2, p3, ncol = 1, align = "v")

ggsave(p.200, filename = here("Figs/fig_cal_FM_200kHz.png"))

# 333 kHz
cal.fm <- read_csv(here::here("Data/Calibration/FM/333kHz.csv"),
                   col_names = fm.names) %>% 
  mutate(frequency = frequency/1000)

p1 <- ggplot(cal.fm, aes(frequency, gain)) +
  geom_point(size = 1) +
  ylab(expression(atop(On-axis~gain, (italic(G)[0]*', dB re 1')))) +
  labs(title = "333 kHz") +
  theme_bw() +
  theme(axis.title.x = element_blank())

p2 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, ba.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, ba.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Beamwidth, (alpha["-3dB"]*", "*beta["-3dB"]*', deg.')))) +
  theme_bw() +
  theme(axis.title.x = element_blank())

p3 <- ggplot() +
  geom_point(data = cal.fm, aes(frequency, oa.alon),
             colour = "cyan", size = 1) +
  geom_point(data = cal.fm, aes(frequency, oa.athw),
             colour = "magenta", size = 1) +
  ylab(expression(atop(Angle~offset, (alpha[0]*', '*beta[0]*', deg.')))) +
  xlab("Frequency (kHz)") +
  theme_bw()

p.333 <- plot_grid(p1, p2, p3, ncol = 1, align = "v")

ggsave(p.333, filename = here("Figs/fig_cal_FM_333kHz.png"))

p.all <- plot_grid(p.70, p.120, p.200, p.333, nrow = 2)

# p.all <- plot_grid(p.70, p.120, p.200, p.333, nrow = 2,
#                    labels = c("70 kHz","120 kHz","200 kHz","333 kHz"))

ggsave(p.all, filename = here("Figs/fig_cal_FM_AllFreqs.png"),
       height = 10, width = 9)

# # Create cowplot objects for map grid
# plot.1 <- ggdraw() + draw_image(here("Figs/fig_cal_FM_70kHz.png")) 
# plot.2 <- ggdraw() + draw_image(here("Figs/fig_cal_FM_120kHz.png"))
# plot.3 <- ggdraw() + draw_image(here("Figs/fig_cal_FM_200kHz.png"))
# plot.4 <- ggdraw() + draw_image(here("Figs/fig_cal_FM_333kHz.png")) 
# 
# # Create final figure
# plot.all <- plot_grid(plot.1, plot.2, plot.3, plot.4, 
#                      nrow = 2)  
# 
# # plot.all <- plot_grid(plot.1, plot.2, plot.3, plot.4, 
# #                       nrow = 2, 
# #                       labels = c("70 kHz","120 kHz","200 kHz","333 kHz"))  
# 
# # Save final figure
# ggsave(plot.all, filename = here("Figs/fig_cal_FM_AllFreqs2.png"),
#        width = 8, height = 8)
