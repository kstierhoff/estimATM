species <- "Clupea pallasii"

df <- nasc.ns.temp

df.sub <- filter(df, stratum == 2)

if (species == "Clupea pallasii") {
  prop <- df.sub$prop.her
  sigmawg <- df.sub$sigmawg.her
}
else if (species == "Engraulis mordax") {
  prop <- df.sub$prop.anch
  sigmawg <- df.sub$sigmawg.anch
}
else if (species == "Sardinops sagax") {
  prop <- df.sub$prop.sar
  sigmawg <- df.sub$sigmawg.sar
}
else if (species == "Scomber japonicus") {
  prop <- df.sub$prop.mack
  sigmawg <- df.sub$sigmawg.mack
}
else if (species == "Trachurus symmetricus") {
  prop <- df.sub$prop.jack
  sigmawg <- df.sub$sigmawg.jack
}

ggplot() +
geom_point(data = filter(df, is.na(prop.her)), 
           aes(long, lat, colour = transect.orig))

biomass.total <- c(biomass.total, (mean((df.sub$cps.nasc * 
                                           prop/(4 * pi * sigmawg))) * 10^3 * stratum.area$area[stratum.area$stratum == 
                                                                                                  j])/1852/1852/1e+06)
