ggplot(filter(nasc, transect.orig %in% c("048-1","048-2","048.2")), 
       aes(long, lat, colour = transect.orig)) + 
  geom_point()

ggplot(filter(nasc, transect.orig %in% c("042-1","042-2","042-3")), 
       aes(long, lat, colour = transect.orig)) + 
  geom_point()

ggplot(filter(nasc, transect.orig %in% c("041-1","041-2","041-3")), 
       aes(long, lat, colour = transect.orig)) + 
  geom_point()

       

nasc.density %>% 
  # filter(transect.orig %in% c("048-1","048-2","048.2")) %>% 
  group_by(transect) %>% 
  summarise(mean.dens = mean(density))

filter(nasc.density, transect == 10) %>% ggplot(aes(long, lat, colour = density)) + geom_point()

ggplot() + 
  geom_point(data = filter(nasc, transect == 10) , aes(long, lat, size = cps.nasc)) +
  geom_point(data = filter(nasc.density, transect == 10, is.na(density)) , aes(long, lat, colour = int.key))

