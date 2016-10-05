library(ggplot2)
library(scales)
library(viridis)
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)

load("acoustics.Rdata")
zoop.ts <- read.csv("nets/zoop_ts.csv")
fish.ts <- read.csv("fish_TS.csv")

dB.mean <- function(x, na.rm=T) 10 * log10(mean(10^(x/10), na.rm=na.rm))

zoop.ts %>%
  group_by(trip, Lake, freq) %>%
  summarize(TS = 10*log10(mean(10^(TS/10) * proportion))) %>%
  dcast(trip + Lake ~ freq, value.var="TS") %>%
  mutate(delta = `120` - `710`)

echo <- mutate(echo,
               hour = hour(datetime),
               minute = minute(datetime))

latlon <- echo %>%
  group_by(trip,Lake,Interval) %>%
  summarise(Lat_M = mean(Lat_M, na.rm=T),
            Lon_M = mean(Lon_M, na.rm=T))
  
echo <- dcast(echo, 
           trip+Lake+Date_M+hour+minute+Interval+Layer_depth_max ~ freq,
           value.var="Sv_mean")
echo <- left_join(echo, latlon)
echo <- mutate(echo,
            Sv_120 = `120`,
            Sv_710 = `710`,
            delta = Sv_120 - Sv_710,
            class = "Zooplankton",
            Sv_zoop = Sv_710,
            Sv_fish = Sv_120)
echo$class[echo$delta > -10 | echo$Sv_120 > -60] <- "Fish"
echo$Sv_zoop[echo$class == "Fish"] <- NA
echo$Sv_fish[echo$class == "Zooplankton" | echo$Sv_fish < -90] <- -Inf


p <- ggplot(echo, aes(x=Interval, y=Layer_depth_max, fill=delta)) +
  geom_tile() + scale_y_reverse(limits=c(50, 0)) + scale_fill_gradient2() +
  facet_grid(Lake ~ trip)
ggsave("graphics/dB_difference.png", p, width=12.5, height=7.61, units="in")


p <- ggplot(echo, aes(x=Interval, y=Layer_depth_max, fill=class)) +
  geom_tile() + 
  scale_y_reverse(limits=c(30, 0)) + ylab("Depth (m)") +
  facet_grid(Lake ~ trip)
ggsave("graphics/echo_classification.png", p, width=12.5, height=7.61, units="in")

p <- ggplot(echo, aes(x=Interval, y=Layer_depth_max, fill=Sv_zoop)) +
  geom_tile() + scale_y_reverse(limits=c(30, 0)) + scale_fill_viridis() +
  facet_grid(Lake ~ trip) + 
  xlab("Depth (m)") + ggtitle("Zooplankton")
ggsave("graphics/echo_zooplankton.png", p, width=12.5, height=7.61, units="in")


p <- ggplot(echo, aes(x=Interval, y=Layer_depth_max, fill=Sv_fish)) +
  geom_tile() + 
  scale_y_reverse(limits=c(60, 0)) + 
  scale_fill_viridis(limits=c(-80, -40), oob=squish) +
  facet_grid(Lake ~ trip) +
  xlab("Depth (m)") + ggtitle("Fish")
ggsave("graphics/echo_fish.png", p, width=12.5, height=7.61, units="in")



################################################################################
# Depth profiles
################################################################################

# sv = sum(sigma_i * n_i)
ts.display.table <- zoop.ts %>%
  filter(freq == 710) %>%
  mutate(percent = round(proportion*100),
         weight = round(weight * 1e6, 1),
         TS = round(TS, 1)) %>%
  select(-freq, -total, -proportion) %>%
  melt(measure.vars = c("TS", "weight", "n", "percent")) %>%
  dcast(trip + Lake ~ model + variable)
write.csv(ts.display.table, "nets/ts.display.table.csv")


mean.ts.zoop <- zoop.ts %>%
  filter(freq == 710) %>%
  group_by(trip, Lake) %>%
  summarize(sigma = sum(10^(TS/10) * proportion),
            TS = 10*log10(sigma),
            weight = sum(weight * proportion)) %>%
  mutate(class = "Sv_zoop") %>%
  as.data.frame()

mean.ts.fish <- fish.ts %>%
  select(trip, Lake, sigma, TS) %>%
  mutate(weight = NA, class = "Sv_fish")

mean.ts <- rbind(mean.ts.zoop, mean.ts.fish)

profiles <- echo %>%
  group_by(trip, Lake, Layer_depth_max) %>%
  summarise(Sv_zoop = dB.mean(Sv_zoop, na.rm=T),
            Sv_fish = dB.mean(Sv_fish, na.rm=T)) %>%
  melt(measure.vars=c("Sv_zoop", "Sv_fish"), variable.name="class", value.name="Sv") %>%
  mutate(sv = 10^(Sv / 10))
profiles$sv[is.na(profiles$sv)] <- 0
profiles <- left_join(profiles, mean.ts)
profiles <- mutate(profiles, 
                   density = sv / sigma, 
                   biomass = density * weight)

p <- filter(profiles, class=="Sv_zoop", Layer_depth_max <= 25) %>% 
  ggplot(aes(y=Layer_depth_max, x=biomass, linetype=Lake, shape=Lake)) + 
  geom_point(size=1) + geom_path() +
  facet_wrap(~trip, nrow=1, scales="free_x") +
  scale_y_reverse(limits=c(25, 0), expand=c(0, 0)) + 
  ylab("Depth (m)") + xlab(expression(Biomass~(g~m^-3))) + 
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
p
ggsave("graphics/zoop_profiles.png", p, width=7, height=4, units="in")

profiles.fish <- profiles %>%
  filter(class=="Sv_fish") %>%
  mutate(depth = round(Layer_depth_max / 5 ) * 5) %>%
  group_by(Lake, trip, depth) %>%
  summarise(density=mean(density))

p <- ggplot(profiles.fish, aes(y=depth, x=density*1e3, linetype=Lake, shape=Lake)) + 
  geom_point() + geom_path() +
  facet_wrap(~trip, nrow=1, scales="free_x") +
  scale_y_reverse(limits=c(60, 0), expand=c(0, 0)) + 
  ylab("Depth (m)") + xlab(expression(Fish~"/"~1000~m^3)) + 
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
p
ggsave("graphics/fish_profiles.png", p, width=7, height=4, units="in")


lake_areas = data.frame(
  Lake = c("Tahoe", "Independence", "Cherry", "Eleanor"),
  area = c(490, 2.6, 6.3, 3.9)
)
lake_areas <- mutate(lake_areas, area = area * 1000^2) # convert areas to m^2
  
lake.biomass <- profiles %>%
  filter(class=="Sv_zoop", Layer_depth_max < 25) %>%
  group_by(Lake, trip) %>%
  summarise(biomass = sum(biomass)) %>%
  left_join(lake_areas, by="Lake") %>%
  mutate(total = signif(biomass * area / 1e6, 3)) %>% # convert g to mt
  select(-area)

lake.biomass %>%
  select(-biomass) %>%
  spread(trip, total)

lake.biomass %>%
  select(-total) %>%
  spread(trip, biomass)

ggplot(filter(lake.biomass), aes(x=trip, y=biomass, fill=Lake)) +
  geom_bar(stat="identity", position="dodge")

ggplot(filter(lake.biomass), aes(x=trip, y=total, fill=Lake)) +
  geom_bar(stat="identity", position="dodge")


################################################################################
# Track lines
################################################################################
tracks <-  echo %>%
  filter(Layer_depth_max < 50, class=="Zooplankton") %>%
  group_by(trip, Lake, Interval, Lat_M, Lon_M) %>%
  summarise(Sv = dB.mean(Sv_zoop, na.rm=T),
            sv = 10^(Sv / 10),
            bottom = max(Layer_depth_max),
            sa = bottom * sv) %>%
  left_join(mean.ts) %>%
  mutate(density = sv / sigma,
         biomass = density * weight) %>%
  select(trip, Lake, Lon_M, Lat_M, Interval, sv, sa, bottom, density, biomass)

save(tracks, file="tracks.Rdata")

