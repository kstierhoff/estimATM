library(tidyverse)
library(atm)
abund <- read_csv("Output/abundance_table_all.csv") %>% 
  mutate(estimated.wg2 = estimate_ts(Species, TL)$estimated.wg,
         biomass2 = abundance * estimated.wg2)

abund %>% 
  group_by(Species, Stock) %>% 
  summarise(a.tot = sum(abundance),
            b.tot = sum(biomass),
            b.tot2 = sum(biomass2))


weight.vec.sar <- 4.446313e-06 * (seq(4, 30))^3.197

4.446313e-06 * (seq(4, 30))^3.197

4.446313e-06 * ((3.574 + 4 * 10 * 1.149)/10)^3.197

0.000373926

4.446313e-06 * (4)^3.197

estimate_ts("Sardinops sagax", seq(4:30))$estimated.wg

estimate_ts("Sardinops sagax", seq(4:30), units = "cm")$estimated.wg


