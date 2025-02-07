# Combine NASC cluster plots from core and nearshore survey areas
# List core figures
cluster.path.core <- fs::dir_ls(here("Figs"), regexp = "fig_nasc_cluster_map.png")
# List nearshore figures
cluster.path.ns   <- fs::dir_ls(here("Figs"), regexp = "fig_nasc_cluster_map_ns.png")

# Get aspect ratio for map image
image.aspect <- magick::image_read(cluster.path.core) %>% 
  image_info() %>% 
  mutate(aspect = width/height) %>% 
  pull()

# Create cowplot objects for map grid
cluster.fig.core <- ggdraw() + draw_image(cluster.path.core) 
cluster.fig.ns   <- ggdraw() + draw_image(cluster.path.ns)

# Create final figure
cluster.fig.all <- plot_grid(cluster.fig.core, cluster.fig.ns, 
                             nrow = 1, labels = c("a)","b)"))  

# Save final figure
ggsave(cluster.fig.all, 
       filename = here("Figs/fig_nasc_cluster_map_combo.png"),
       height = map.height, width = map.height*image.aspect*2) 
