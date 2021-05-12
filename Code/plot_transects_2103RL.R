tx.ends = nasc %>% 
  group_by(vessel.name, transect.orig) %>% 
  slice(which.min(long))



ggplot(filter(nasc, vessel.name == "RL"), aes(long, lat, colour = transect.orig)) + 
  geom_point() + 
  geom_text(data = filter(tx.ends, vessel.name == "RL"), aes(long, lat, label = transect.orig), color = "black") +
  coord_map()

ggplot(filter(nasc, vessel.name == "LBC"), aes(long, lat, colour = transect.orig)) + 
  geom_point() + 
  geom_text(data = filter(tx.ends, vessel.name == "LBC"), aes(long, lat, label = transect.orig), color = "black") +
  coord_map()
