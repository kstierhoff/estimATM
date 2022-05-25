tmp.names <- c("cps.nasc", "cps.NASC")
tmp.df <- data.frame("cps.nasc" = NA, "cps.NASC" = NA)

"cps.NASC" %in% tmp.names | "cps.nasc" %in% tmp.names

"cps.NASC" %in% colnames(tmp.df) | "cps.nasc" %in% colnames(tmp.df)
