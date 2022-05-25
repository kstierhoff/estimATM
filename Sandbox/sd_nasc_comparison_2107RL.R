sd119.local <- atm::extract_csv(filename = "C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Backscatter/SD/2107SD1059_119_CPS-Final 38 kHz CPS_nasc_cps.csv")

sd119.ast3 <- atm::extract_csv("C:/KLS/CODE/R_packages/estimATM/2107RL/Data/Backscatter/SD/2107SD1059_119_CPS-Final 38 kHz CPS.csv")


ggplot() + 
  geom_point(data = sd119.ast3, aes(long, lat, size = ), colour = "red") +
  geom_point(data = sd119.local, aes(long, lat)) + 
  coord_map()
