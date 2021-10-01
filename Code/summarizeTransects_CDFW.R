# Load backscatter data
load(here::here("Output/nasc_cps.Rdata"))

library(tidyverse)
library(lubridate)

nasc <- nasc.cps %>% 
  mutate(datetime_pdt = with_tz(datetime, tzone = "America/Los_Angeles"))

nasc.summ <- nasc %>% 
  filter(!transect %in% c("4142","046.1","046.2","048.1","048.2")) %>% 
  mutate(transect = str_replace(transect, "-\\d", "")) %>%  
  group_by(transect) %>% 
  summarize(
    start = format(min(datetime_pdt), format = "%F %T", tz = "America/Los_Angeles", usetz = TRUE),
    end = format(max(datetime_pdt), format = "%F %T", tz = "America/Los_Angeles", usetz = TRUE)
  ) %>% 
  arrange(start)

write_csv(nasc.summ, here::here("Output/transect_summ_norcal_2107RL.csv"))
