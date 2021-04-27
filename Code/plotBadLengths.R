# Select species
scientificName.sub <- "Trachurus symmetricus"

# Subset lengths and models
lengths.sub     <- filter(lengths, scientificName == scientificName.sub)
lengths.all.sub <- filter(lengths.all, 
                          scientificName == scientificName.sub,
                          season ==  tolower(survey.season))

lw.df.sub <- filter(lw.df, scientificName == scientificName.sub)

l.bad.sar <- filter(lengths.all.sub, haul %in% c(33, 35, 38)) %>% 
  mutate(totalLength_mm = standardLength_mm) %>% 
  mutate(SL = convert_length(scientificName, totalLength_mm, "TL", "SL"))

l.bad.jack <- filter(lengths.all.sub, haul %in% c(33)) %>% 
  mutate(totalLength_mm = forkLength_mm) #%>%  mutate(FL = convert_length(scientificName, totalLength_mm, "TL", "FL"))

lw.df2 <- lw.df %>% 
  mutate(FL = convert_length(scientificName, L, "TL", "FL")) %>% 
  mutate(SL = convert_length(scientificName, L, "TL", "SL")) %>%
  mutate(TL = convert_length(scientificName, L, "SL", "TL")) %>%
  filter(scientificName == "Sardinops sagax")

ggplot() + 
  # Plot L/W data from all years/surveys
  geom_point(data = lengths.all.sub, 
             aes(totalLength_mm, weightg),
             colour = "gray70", alpha = 0.3) +
  # Plot L/W data for current survey
  geom_point(data = lengths.sub,
             aes(totalLength_mm, weightg, 
                 fill = factor(haul), label = label), 
             shape = 21, alpha = 0.9, size = 3) + 
  geom_point(data = l.bad.jack,
             aes(totalLength_mm, weightg, label = label),
             shape = 21, alpha = 0.9, size = 3, fill = "red") +
  # scale_fill_manual(name = "Sex", values = c("female" = "pink", "male" = "lightblue",
  #                                            "unknown" = "#FFC926", "missing" = "purple")) +
  # scale_colour_manual(name = "Flagged", values = c("NA" = NA, "TRUE" = "black")) +
  # Plot seasonal length models for each species
  geom_line(data = lw.df.sub, aes(L, W), alpha = 0.75, colour = "gray50", linetype = 'dashed') +
  # geom_line(data = lw.df2, aes(FL, W), alpha = 0.75, colour = "red", linetype = 'dashed') +
  # geom_line(data = lw.df2, aes(SL, W), alpha = 0.75, colour = "blue", linetype = 'dashed') +
  # Plot individuals with missing lengths and weights
  geom_point(data = filter(lengths.sub, missing.length == TRUE), 
             aes(totalLength_mm, weightg, label = label),
             shape = 21, fill = 'red',  size = 2.5, colour = NA, alpha = 0.5) +
  geom_point(data = filter(lengths.sub, missing.weight == TRUE), 
             aes(totalLength_mm, weightg, label = label),
             shape = 21, fill = 'blue', size = 2.5, colour = NA, alpha = 0.5) +
  # Format plot
  xlab("Total length (mm)") + ylab("Mass (g)") 
