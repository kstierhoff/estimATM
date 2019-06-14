# Extract bathymetry along each transect ----------------------------------
# Create a df for transect profiles
tx.prof <- data.frame()

# Extract bathy along each transect and combine
for (i in unique(transects$group)) {
  tx.tmp <- filter(transects, group == i)
  tmp <- get.transect(bathy_dem, tx.tmp$Longitude[1], tx.tmp$Latitude[1],
                      tx.tmp$Longitude[nrow(tx.tmp)], tx.tmp$Latitude[nrow(tx.tmp)],
                      distance = TRUE) %>% 
    mutate(transect = unique(tx.tmp$Transect),
           type = unique(tx.tmp$Type)) %>% 
    filter(depth < 0)
  
  tx.prof <- bind_rows(tx.prof,tmp)
}

# Calculate the .99 quantile for each transect type
type.ecdf <- tx.prof %>% 
  group_by(type) %>% 
  summarise(depth.99 = quantile(ecdf(abs(depth)),.99))

# Plot ECDF by type, showing the .99 quantile
tx.ecdf.plot <- ggplot(tx.prof, aes(abs(depth))) + 
  stat_ecdf() +
  geom_vline(data = type.ecdf, aes(xintercept = depth.99), linetype = "dashed") +
  facet_wrap(~type, scales = "free") +
  ylab("ECDF") + xlab("Depth (m)")

# Save plot
ggsave(tx.ecdf.plot, filename = here("Figs/fig_transect_depth_ecdf.png"))


# Same analysis, but for WA/OR nearshore transects only -------------------
# Restrict ECDF to nearshore transects off OR/WA
type.ecdf.WAOR <- tx.prof %>% 
  filter(type == "Nearshore", between(transect, 133, 211)) %>% 
  group_by(type) %>% 
  summarise(depth.99 = quantile(ecdf(abs(depth)),.99))

# Plot ECDF by type, showing the .99 quantile
tx.ecdf.WAOR.plot <- tx.prof %>% 
  filter(type == "Nearshore", between(transect, 133, 211)) %>% 
  ggplot(aes(abs(depth))) + 
  stat_ecdf() +
  geom_vline(data = type.ecdf.WAOR, aes(xintercept = depth.99), linetype = "dashed") +
  ylab("ECDF") + xlab("Depth (m)")

# Save plot
ggsave(tx.ecdf.WAOR.plot, filename = here("Figs/fig_transect_depth_ecdf_WAOR.png"))
