# Load backscatter data
load("E:/CODE/R_packages/estimATM/2107RL/Output/nasc_cps.Rdata")

library(tidyverse)
library(lubridate)

nasc <- nasc.cps %>% 
  mutate(datetime_pdt = with_tz(datetime, tzone = "America/Los_Angeles"))

nasc.summ <- nasc %>% 
  group_by(transect) %>% 
  summarize(
    start = format(min(datetime_pdt), format = "%F %T", tz = "America/Los_Angeles", usetz = TRUE),
    end = format(max(datetime_pdt), format = "%F %T", tz = "America/Los_Angeles", usetz = TRUE)
  ) %>% 
  arrange(start)

write_csv(nasc.summ, here::here("Output/transect_summ_norcal_2107RL.csv"))
