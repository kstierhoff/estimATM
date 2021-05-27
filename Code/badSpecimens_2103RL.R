# filter lengths for only sardine and jacks

bl <- lengths %>% 
  filter(haul %in% c(33, 35, 38)) %>% 
  mutate(
    nativeLength_2 = case_when(
      scientificName == "Sardinops sagax" ~ 
        convert_length("Sardinops sagax", .$standardLength_mm, "TL", "SL"),
      scientificName == "Trachurus symmetricus" ~ 
        convert_length("Trachurus symmetricus", .$nativeLength_mm, "TL", "FL"),
      TRUE ~ NA_real_))

weight.comp <- ggplot() + 
  geom_histogram(data = lengths,
                 aes(weightg), alpha = 0.5) + 
  geom_histogram(data = bl, aes(weightg), fill = "yellow", alpha = 0.5) + 
  facet_wrap(~scientificName, scales = "free", nrow = 1) +
  theme_bw()

length.comp <- ggplot() + 
  geom_histogram(data = lengths,
                 aes(nativeLength_mm), alpha = 0.5) + 
  geom_histogram(data = bl, aes(nativeLength_mm), fill = "yellow", alpha = 0.5) + 
  facet_wrap(~scientificName, scales = "free", nrow = 1) +
  theme_bw()

# library(cowplot)

plot_grid(weight.comp, length.comp, ncol = 1)

lengths %>% 
  filter(scientificName %in% c("Sardinops sagax","Trachurus symmetricus")) %>% 
  group_by(scientificName, haul) %>% 
  summarise(
    mean.l = mean(nativeLength_mm))

ggplot(filter(lengths, scientificName %in% c("Sardinops sagax","Trachurus symmetricus")), 
              aes(nativeLength_mm,weightg)) + 
  geom_point() +
  geom_point(data = filter(bl, scientificName %in% c("Sardinops sagax","Trachurus symmetricus")),
             aes(nativeLength_mm, weightg), 
             colour = "blue", size = 3) +
  geom_point(data = filter(bl, scientificName %in% c("Sardinops sagax","Trachurus symmetricus")),
             aes(nativeLength_2, weightg), 
             shape = 21, colour = "green", size = 3) +
  facet_wrap(~scientificName) +
  theme_bw()
