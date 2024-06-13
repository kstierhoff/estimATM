pie.spp <- c("Clupea pallasii","Engraulis mordax","Sardinops sagax","Scomber japonicus","Trachurus symmetricus")

# Set species colors
sardine.color      <- '#FF0000'
anchovy.color      <- '#00CD66'
jack.mack.color    <- '#0000FF'
jacksmelt.color    <- '#A020F0'
pac.mack.color     <- '#00FFFF'
pac.herring.color  <- '#F5DEB3'
rnd.herring.color  <- '#F0B81D'
other.color        <- 'gray'

pie.columns <- c("Engraulis mordax" = "Anchovy", "Trachurus symmetricus" = "JackMack", 
                 "Atherinopsis californiensis" = "Jacksmelt", "Clupea pallasii" = "PacHerring", 
                 "Scomber japonicus" = "PacMack", "Etrumeus acuminatus" = "RndHerring", 
                 "Sardinops sagax" = "Sardine")

pie.columns[names(pie.columns) %in% pie.spp]

pie.labels <- c("Engraulis mordax" = "Anchovy", "Trachurus symmetricus" = "J. Mackerel", 
                "Atherinopsis californiensis" = "Jacksmelt", "Clupea pallasii" = "P. herring", 
                "Scomber japonicus" = "P. mackerel", "Etrumeus acuminatus" = "R. herring", 
                "Sardinops sagax" = "Sardine")

pie.labels[names(pie.labels) %in% pie.spp]

pie.colors <- c("Engraulis mordax" = anchovy.color, "Trachurus symmetricus" = jack.mack.color, 
                "Atherinopsis californiensis" = jacksmelt.color, "Clupea pallasii" = pac.herring.color, 
                "Scomber japonicus" = pac.mack.color, "Etrumeus acuminatus" = rnd.herring.color,
                "Sardinops sagax" = sardine.color)

pie.colors[names(pie.colors) %in% pie.spp]
