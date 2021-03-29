library(tidyverse)

wind.df <- read_csv(here::here("Data/Nav/lasker_met_data.csv")) %>% 
  filter(wind_speed_p < 50) %>% 
  mutate(
    wind_brg = case_when(
      wind_dir < 180 ~ wind_dir + 180,
      TRUE ~ wind_dir - 180),
    wind_angle = (wind_dir/360)*2*pi) %>% 
  # slice_sample(prop = 0.05) %>% 
  arrange(datetime)

ggplot(wind.df, aes(long, lat)) + 
  # geom_point() + 
  geom_spoke(aes(angle = wind_angle, radius = sqrt(wind_speed_p+1)/50, colour = wind_speed_p)) + 
  scale_colour_viridis_c(option = "inferno") + 
  coord_map()

ggplot() + 
  geom_path(data = wind.df, aes(long, lat)) + 
  # geom_point() + 
  geom_spoke(data = slice_sample(wind.df, prop = 0.05), 
             aes(long, lat, 
                 angle = wind_angle, 
                 radius = sqrt(wind_speed_p+1)/50, 
                 colour = wind_speed_p), 
             arrow = arrow(length = unit(0.2,"cm"))) + 
  scale_colour_viridis_c(option = "inferno") + 
  coord_map()


# ggplot() + 
#   geom_point(data = wind.df, aes(long, lat, colour = wind_speed_p), size = 2) + 
#   # # geom_point() + 
#   # geom_spoke(data = slice_sample(wind.df, prop = 0.05), 
#   #            aes(long, lat, angle = wind_dir, 
#   #                radius = sqrt(wind_speed+1)/50, 
#   #                colour = wind_speed), 
#   #            arrow = arrow(length = unit(0.2,"cm"))) + 
#   scale_colour_viridis_c(option = "inferno") + 
#   coord_map()

ggplot(wind.df) + 
  geom_spoke(aes(datetime, angle = wind_dir_p, radius = wind_speed_p))
