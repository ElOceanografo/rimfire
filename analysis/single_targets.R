library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

load("single_targets.Rdata")
sound.speed <- 1500

targets <- filter(targets, TS_uncomp > -60, Target_range < 100)

ggplot(targets, aes(x=TS_comp, y=..density..)) +
  geom_histogram() +
  facet_grid(Lake ~ trip)

ggplot(targets, aes(x=Ping_number, y=Target_range, size=TS_comp)) +
  geom_point(alpha=0.5, shape=1) + 
  facet_grid(Lake ~ trip)

filter(targets, Lake %in% c("Cherry", "Eleanor")) %>%
ggplot(aes(x=Target_longitude, y=Target_latitude)) +
  geom_point(size=0.5, alpha=0.5) + 
  facet_wrap(~trip)

target.density <- targets %>%
  mutate(range = round(Target_range),
         pulse.length = Transmitted_pulse_length * 1e-3 * sound.speed,
         radius = range * tan(3.5 * pi / 180) + 0.2,
         volume = pi * radius^2 * pulse.length) %>%
  group_by(trip, Lake, Ping_number, range) %>%
  summarize(TS_mean = mean(TS_comp),
            volume = mean(volume),
            count = n(),
            density = count / volume)


ggplot(target.density, aes(y=-range, x=Ping_number, size=density, color=TS_mean)) +
  geom_point(alpha=0.3) + facet_grid(Lake ~ trip) 


target.density <- target.density %>%
  group_by(trip, Lake, range) %>%
  summarize(count = mean(count),
            volume = mean(volume))

ggplot(target.density, aes(x=-range, y=count)) + 
  geom_point() + geom_smooth(method="gam", method.args=list(family="poisson"))+
  coord_flip() + facet_grid(Lake ~ trip)

area.density <- targets %>%
  group_by(trip, Lake, Ping_number) %>%
  summarize(max_depth = max(Target_range),
            radius = max_depth * tan(3.5 * pi / 180),
            volume = pi/3 * radius^2 * max_depth,
            count = n(),
            density = count / volume,
            area.density = density * max_depth,
            lon = mean(Target_longitude),
            lat = mean(Target_latitude))

filter(area.density, Lake %in% c("Cherry", "Eleanor")) %>%
  ggplot(aes(x=lon, y=lat, size=count, color=volume)) +
  geom_point() + scale_color_viridis() + facet_wrap(~trip)

ggplot(area.density, aes(x=count, y=..density..)) +
  geom_histogram(binwidth=1) + facet_grid(Lake ~ trip)

ggplot(area.density, aes(x=density, y=..density..)) +
  geom_histogram() + xlim(0, 2) + facet_grid(Lake ~ trip)

area.density %>%
  group_by(trip, Lake) %>%
  summarize(Density = mean(density)) %>% 
  spread(trip, Density)

area.density %>%
  group_by(trip, Lake) %>%
  summarize(SD = sd(density)) %>%
  spread(trip, SD)
