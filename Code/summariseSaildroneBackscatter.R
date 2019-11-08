# Load libraries
library(mgcv);library(here); library(tidyverse); library(xts); library(sf);
library(concaveman); 

theme_set(theme_bw())

# Load Saildrone data
load(here("Output/nasc_final.Rdata"))
load(here("Data/Nav/nav_data_saildrone.Rdata"))

# Add SOG to nasc
nasc.sd <- nasc %>% 
  mutate(time.align  = align.time(datetime, 60),
         nasc.comp = NASC.50 - NASC.5, # Substract upper 5 m from Saildrone NASC
         loc = cut(lat, c(0,34.7,40.430520,48.490, 55), labels = FALSE),
         region = as.factor(case_when(
           loc == 1 ~ "SCB",
           loc == 2 ~ "CenCoast",
           loc == 3 ~ "WaOr",
           loc == 4 ~ "Van",
           TRUE ~ "Other")),
         region = fct_reorder(region, loc)) %>% 
  left_join(select(nav.sd, vessel.orig = vessel.name, time.align, SOG)) %>%
  filter(!is.na(SOG)) %>% 
  arrange(desc(nasc.comp)) %>% 
  mutate(rank = seq_along(nasc.comp),
         sog.bin = cut(SOG, seq(0, ceiling(max(SOG)), 0.2)),
         vessel = "Saildrone")

# Summarise Saildrone NASC by region and SOG bin
nasc.sd.sog.summ <- nasc.sd %>% 
  group_by(region,sog.bin) %>% 
  summarise(n.int = n(),
            sum.nasc = sum(NASC.50),
            sum.nasc.n = sum.nasc/n.int)

# Plot sum of NASC vs. SOG bit, by retion
ggplot(nasc.sd.sog.summ, aes(sog.bin, log(sum.nasc), colour = region)) + 
  geom_line(aes(group = region)) + geom_point() +
  xlab("SOG (kn)") + ylab(expression(log(Sigma("NASC")))) +
  labs(color = "Region") +
  theme(axis.text.x = element_text(angle = 90))

ggsave(here("Figs/fig_sog_sum-nasc_comp.png"))

# Plot normalized sum of NASC vs. SOG bit, by retion
ggplot(nasc.sd.sog.summ, aes(sog.bin, log(sum.nasc.n), colour = region)) + 
  geom_line(aes(group = region)) + geom_point() +
  xlab("SOG (kn)") + ylab(expression(italic(log)(Sigma(italic(s)[A])/"N"))) +
  labs(color = "Region") +
  theme(axis.text.x = element_text(angle = 90))

ggsave(here("Figs/fig_sog_sum-nasc-n_comp.png"),
       height = 4, width = 7)

# Assign Saildrone nav to regions
nav.sd <- nav.sd %>% 
  mutate(loc = cut(lat, c(0,34.7,40.430520,48.490, 55), labels = FALSE),
         region = case_when(
           loc == 1 ~ "SCB",
           loc == 2 ~ "CenCoast",
           loc == 3 ~ "WaOr",
           loc == 4 ~ "Van",
           TRUE ~ "Other"))

# Compute median Saildrone SOG
nav.sd.summ <- nav.sd %>% 
  group_by(region) %>% 
  summarise(med.sog = median(SOG))

# Calculate median Saildrone NASC 
nasc.sd.summ <- nasc.sd %>% 
  group_by(region) %>% 
  summarise(med.log.nasc = median(log(NASC.50))) 

# Combine median SOG and NASC
nasc.sog.comp <- nav.sd.summ %>% 
  left_join(nasc.sd.summ)

# Plot median NASC vs. SOG for survey regions
ggplot(nasc.sog.comp, aes(med.sog, med.log.nasc, label = region, colour = region)) + 
  geom_text(aes(fontface = "bold"))

ggsave(here("Figs/fig_sog_nasc_comp_saildrone.png"))
  
nasc.sd.sf <- select(nasc.sd, long, lat, vessel.orig) %>% 
  st_as_sf(coords = c("long","lat"), crs = 4326)

sd.hull <- concaveman(nasc.sd.sf)

# Plot NASC.50-NASC.5 vs. SOG to look for FAD effect
ggplot(nasc.sd, aes(SOG, NASC.50 - NASC.5)) +
  geom_point() +
  ggtitle("Saildrone NASC vs. Speed")

ggsave(here::here("Figs", "fig_nasc_vs_sog_saildrone.png"))

load("D:/CODE/R_packages/EstimateCPS/1807RL/Output/nasc_final.Rdata")

nasc <- nasc %>% 
  mutate(nasc.comp = NASC.50) %>% 
  arrange(desc(nasc.comp)) %>% 
  mutate(nasc.rank = seq_along(nasc.comp),
         vessel = "Lasker")

# nasc.sf <- nasc %>% 
#   select(nasc.rank, long, lat) %>% 
#   st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
#   st_intersection(sd.hull)
# 
# save(nasc.sf, file = here("Output/nasc_overlap_lasker.Rdata"))

load(here("Output/nasc_overlap_lasker.Rdata"))

nasc <- nasc %>% 
  filter(nasc.rank %in% nasc.sf$nasc.rank) %>% 
  mutate()


n.int <- 250

nasc.ranks <- filter(select(nasc, nasc.rank, nasc.comp, vessel), nasc.rank <= n.int) %>% 
  bind_rows(filter(select(nasc.sd, rank, nasc.comp, vessel), rank <= n.int))

ggplot(nasc.ranks, aes(rank, nasc.comp, colour = vessel)) + geom_point() +
  ggtitle("Ranked Saildrone and Lasker NASC")

ggsave(here::here("Figs", "fig_ranked_nasc_comparison.png"))


nasc.comp <- select(nasc, vessel, NASC.5, NASC.20, NASC.40, NASC.70) %>% 
  bind_rows(select(nasc.sd, vessel, NASC.5, NASC.20, NASC.40, NASC.70))


png(here("Figs/fig_nasc_comp_base.png"))
par(mfrow = c(2,2))
qqplot(log(nasc$NASC.20 - nasc$NASC.5 + 1), log(nasc.sd$NASC.20 - nasc.sd$NASC.5 + 1),
       main = "~5 to 20 m",
       xlab = "log(NASC; RL)", ylab = "log(NASC; SD)",
       xlim = c(0, 15), ylim = c(0, 15))
abline(1,1)

qqplot(log(nasc$NASC.40 - nasc$NASC.20 + 1), log(nasc.sd$NASC.40 - nasc.sd$NASC.20 + 1),
       main = "~20 to 40 m",
       xlab = "log(NASC; RL)", ylab = "log(NASC; SD)",
       xlim = c(0, 15), ylim = c(0, 15))
abline(1,1)

qqplot(log(nasc$NASC.70 - nasc$NASC.40 + 1), log(nasc.sd$NASC.70 - nasc.sd$NASC.40 + 1),
       main = "~40 to 70 m",
       xlab = "log(NASC; RL)", ylab = "log(NASC; SD)",
       xlim = c(0, 15), ylim = c(0, 15))
abline(1,1)

qqplot(log(nasc$NASC.70 + 1), log(nasc.sd$NASC.70 + 1),
       main = "~5 to 70 m",
       xlab = "log(NASC; RL)", ylab = "log(NASC; SD)",
       xlim = c(0, 15), ylim = c(0, 15))
abline(1,1)
dev.off()


# nasc.sd %>% 
#   filter(NASC.50 > 500000) %>% 
#   select(filename, datetime, NASC.50, nasc.comp) %>% 
#   arrange(filename, desc(NASC.50)) %>% 
#   write_csv(here("Output/big_nasc_saildrone_1807RL.csv"))

# nasc.sd.interp <- interp(nasc.sd$NASC.50, nasc.sd$)

gam1 <- mgcv::gam(NASC.50 ~ s(SOG), family = nb(link = log), data = nasc.sd)

plot(gam1)

summary(gam1)

# Compute Saildrone NASC pdf by depths between 5 and 50 m
nasc.cols <- c("NASC.5","NASC.10","NASC.15","NASC.20","NASC.25","NASC.30","NASC.35",
               "NASC.40","NASC.45","NASC.50")

nasc.cols.pdf <- c("n.5","n.10","n.15","n.20","n.25","n.30","n.35",
                   "n.40","n.45","n.50")

nasc.sd.pdf <- nasc.sd %>% 
  select(nasc.cols) %>% 
  mutate(n.5  = NASC.5,
         n.10 = NASC.10 - NASC.5,
         n.15 = NASC.15 - NASC.10,
         n.20 = NASC.20 - NASC.15,
         n.25 = NASC.25 - NASC.20,
         n.30 = NASC.30 - NASC.25,
         n.35 = NASC.35 - NASC.30,
         n.40 = NASC.40 - NASC.35,
         n.45 = NASC.45 - NASC.40,
         n.50 = NASC.50 - NASC.45) %>% 
  select(nasc.cols.pdf) %>% 
  as_tibble() %>% 
  gather() %>% 
  mutate(depth = -as.numeric(str_replace(key,"n.",""))) %>% 
  group_by(key,depth) %>% 
  summarise(nasc.sum = sum(value)) %>% 
  mutate(nasc.sum.pdf = nasc.sum/sum(.$nasc.sum),
         vessel = "Saildrone") %>% 
  arrange(desc(depth))

nasc.pdf <- nasc %>% 
  select(nasc.cols) %>% 
  mutate(n.5  = NASC.5,
         n.10 = NASC.10 - NASC.5,
         n.15 = NASC.15 - NASC.10,
         n.20 = NASC.20 - NASC.15,
         n.25 = NASC.25 - NASC.20,
         n.30 = NASC.30 - NASC.25,
         n.35 = NASC.35 - NASC.30,
         n.40 = NASC.40 - NASC.35,
         n.45 = NASC.45 - NASC.40,
         n.50 = NASC.50 - NASC.45) %>% 
  select(nasc.cols.pdf) %>% 
  as_tibble() %>% 
  gather() %>% 
  mutate(depth = -as.numeric(str_replace(key,"n.",""))) %>% 
  group_by(key,depth) %>% 
  summarise(nasc.sum = sum(value)) %>% 
  mutate(nasc.sum.pdf = nasc.sum/sum(.$nasc.sum),
         vessel = "Lasker") %>% 
  arrange(desc(depth)) %>% 
  bind_rows(nasc.sd.pdf)

ggplot(nasc.pdf, aes(depth,nasc.sum.pdf, colour = vessel)) + 
  geom_line(size = 1) +   geom_point() + 
  xlab("Depth (m)") + ylab("Probability density") + 
  labs(color = "Vessel") +
  coord_flip()

ggsave(here("Figs/fig_nasc_depth_pdf.png"))

write_csv(nasc.pdf, here("Output/nasc_depth_pdf.csv"))

nasc.sd.pdf.reg <- nasc.sd %>% 
  select(nasc.cols, lat) %>% 
  mutate(n.5  = NASC.5,
         n.10 = NASC.10 - NASC.5,
         n.15 = NASC.15 - NASC.10,
         n.20 = NASC.20 - NASC.15,
         n.25 = NASC.25 - NASC.20,
         n.30 = NASC.30 - NASC.25,
         n.35 = NASC.35 - NASC.30,
         n.40 = NASC.40 - NASC.35,
         n.45 = NASC.45 - NASC.40,
         n.50 = NASC.50 - NASC.45,
         loc = cut(lat, c(0,34.7,40.430520,48.490, 55), labels = FALSE),
         region = case_when(
           loc == 1 ~ "SCB",
           loc == 2 ~ "CenCoast",
           loc == 3 ~ "WaOr",
           loc == 4 ~ "Van",
           TRUE ~ "Other")) %>% 
  select(nasc.cols.pdf, region) %>% 
  as_tibble() %>% 
  gather("key","value",-region) %>% 
  mutate(depth = -as.numeric(str_replace(key,"n.",""))) %>% 
  group_by(region, key, depth) %>% 
  summarise(nasc.sum = sum(value)) 

nasc.sd.pdf.reg.summ <- data.frame()

for (i in unique(nasc.sd.pdf.reg$region)) {
  tmp <- filter(nasc.sd.pdf.reg, region == i) %>% 
    mutate(nasc.sum.pdf = nasc.sum/sum(.$nasc.sum),
           vessel = "Saildrone") %>% 
    arrange(region, desc(depth)) 
  
  nasc.sd.pdf.reg.summ <- bind_rows(nasc.sd.pdf.reg.summ, tmp)
}

nasc.pdf.reg <- nasc %>% 
  select(nasc.cols, lat) %>% 
  mutate(n.5  = NASC.5,
         n.10 = NASC.10 - NASC.5,
         n.15 = NASC.15 - NASC.10,
         n.20 = NASC.20 - NASC.15,
         n.25 = NASC.25 - NASC.20,
         n.30 = NASC.30 - NASC.25,
         n.35 = NASC.35 - NASC.30,
         n.40 = NASC.40 - NASC.35,
         n.45 = NASC.45 - NASC.40,
         n.50 = NASC.50 - NASC.45,
         loc = cut(lat, c(0,34.7,40.430520,48.490, 55), labels = FALSE),
         region = case_when(
           loc == 1 ~ "SCB",
           loc == 2 ~ "CenCoast",
           loc == 3 ~ "WaOr",
           loc == 4 ~ "Van",
           TRUE ~ "Other")) %>% 
  select(nasc.cols.pdf, region) %>% 
  as_tibble() %>% 
  gather("key","value",-region) %>% 
  mutate(depth = -as.numeric(str_replace(key,"n.",""))) %>% 
  group_by(region, key, depth) %>% 
  summarise(nasc.sum = sum(value)) 

nasc.pdf.reg.summ <- data.frame()

for (i in unique(nasc.pdf.reg$region)) {
  tmp <- filter(nasc.pdf.reg, region == i) %>% 
    mutate(nasc.sum.pdf = nasc.sum/sum(.$nasc.sum),
           vessel = "Lasker") %>% 
    arrange(region, desc(depth)) 
  
  nasc.pdf.reg.summ <- bind_rows(nasc.pdf.reg.summ, tmp)
}

nasc.pdf.reg.final <- nasc.pdf.reg.summ %>% 
  bind_rows(nasc.sd.pdf.reg.summ)

ggplot(nasc.pdf.reg.final, aes(depth, nasc.sum.pdf, colour = vessel)) + 
  geom_line(size = 1) +   geom_point() + 
  facet_wrap(~region) +
  xlab("Depth (m)") + ylab("Probability density") + 
  labs(color = "Vessel") +
  coord_flip()

ggsave(here("Figs/fig_nasc_depth_pdf_region.png"))

write_csv(nasc.pdf.reg.final, here("Output/nasc_depth_pdf_region.csv"))
