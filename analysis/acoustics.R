library(ggplot2)
library(scales)
library(viridis)
library(reshape2)
library(dplyr)
library(lubridate)

load("acoustics.Rdata")

dB.mean <- function(x, na.rm=T) 10 * log10(mean(10^(x/10), na.rm=na.rm))

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
echo$class[echo$delta > -3 | echo$Sv_120 > -60] <- "Fish"
echo$Sv_zoop[echo$class == "Fish"] <- NA
echo$Sv_fish[echo$class == "Zooplankton" | echo$Sv_fish < -90] <- -Inf


p <- ggplot(echo, aes(x=Interval, y=Layer_depth_max, fill=delta)) +
  geom_tile() + scale_y_reverse() + scale_fill_gradient2() +
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
profiles <- echo %>%
  group_by(trip, Lake, Layer_depth_max) %>%
  summarise(Sv_zoop = dB.mean(Sv_zoop, na.rm=T),
            Sv_fish = dB.mean(Sv_fish, na.rm=T)) %>%
  melt(measure.vars=c("Sv_zoop", "Sv_fish"), variable.name="class", value.name="Sv") %>%
  mutate(sv = 10^(Sv / 10))
profiles$sv[is.na(profiles$sv)] <- 0

x_scale <- 1e6
p <- filter(profiles, class=="Sv_zoop") %>% 
  ggplot(aes(x=Layer_depth_max, y=sv * x_scale, color=Lake)) + 
    geom_point() + geom_line() +
    facet_grid(. ~ trip) + 
    scale_x_reverse(limits=c(25, 0)) + coord_flip() + ylim(0, 2) +
    xlab("Depth (m)") + ylab(expression(Mean~s[v]~(mm^2~m^-3))) + 
    ggtitle("Zooplankton") +
    theme_bw()
# p
ggsave("graphics/zoop_profiles.pdf", p, width=12.5, height=7.61, units="in")

p <- filter(profiles, class=="Sv_fish") %>% 
  ggplot(aes(x=Layer_depth_max, y=sv * x_scale, color=Lake)) + 
  geom_point(size=0.5) + geom_path() +#geom_smooth(span=0.2, se=F) +
  facet_grid(. ~ trip) + coord_flip() +
  scale_x_reverse(limits=c(60, 0)) + 
  scale_y_continuous(limits=c(0, 10)) +
  xlab("Depth (m)") + ylab(expression(Mean~s[v]~(mm^2~m^-3))) + 
  ggtitle("Fish") +
  theme_bw()
# p
ggsave("graphics/fish_profiles.pdf", p, width=12.5, height=7.61, units="in")


################################################################################
# Track lines
################################################################################
tracks <- echo %>%
  filter(Layer_depth_max < 50) %>%
  group_by(trip, Lake, Interval, Lat_M, Lon_M) %>%
  summarise(Sv_zoop = dB.mean(Sv_zoop, na.rm=T),
            Sv_fish = dB.mean(Sv_fish, na.rm=T),
            bottom = max(Layer_depth_max))%>%
  melt(measure.vars=c("Sv_zoop", "Sv_fish"), variable.name="class", value.name="Sv") %>%
  mutate(class = gsub("Sv_", "", class),
         sv = 10^(Sv/10))


save(tracks, file="tracks.Rdata")


# zoop.ts <- read.csv("nets/zoop_ts.csv")
# zoop.ts <- mutate(zoop.ts,
#                   freq = as.factor(freq),
#                   trip = as.character(trip),
#                   Lake = as.character(Lake),
#                   sigma = 10^(TS/10),
#                   sigma.total = sigma * proportion)
# 
# echo <- group_by(zoop.ts, trip, Lake, freq) %>%
#   summarize(sigma.total = sum(sigma.total, na.rm=T)) %>%
#   right_join(echo, by=c("trip", "Lake", "freq"))
# 
# 
# proportions <- dcast(zoop.ts, trip + Lake + freq ~ Group, value.var="proportion")
# echo <- left_join(echo, proportions, by=c("trip", "Lake", "freq"))
# 
# echo <- mutate(echo,
#                sv_mean = 10^(Sv_mean / 10),
#                n.total = sv_mean / sigma.total,
#                n.copepods = n.total * Copepods,
#                n.cladocerans = n.total * Cladocerans)

# 
# densities <- echo %>%
#   filter(freq == 710) %>%
#   select(trip, Lake, Interval, Layer, n.total, n.copepods, n.cladocerans)
# 
# 
# density_limits <- c(0, 8)
# p <- ggplot(densities, aes(x=Interval, y=Layer, fill=log10(n.total))) +
#   geom_tile() + 
#   scale_fill_viridis(limits=density_limits) +
#   scale_y_reverse(limits=depth_limits) + 
#   facet_grid(Lake ~ trip, scales="free_x") + 
#   ylab("Depth (m)") + ggtitle("Total density")
# 
# p <- ggplot(densities, aes(x=Interval, y=Layer, fill=log10(n.copepods))) +
#   geom_tile() + 
#   scale_fill_viridis(limits=density_limits) +
#   scale_y_reverse(limits=depth_limits) + 
#   facet_grid(Lake ~ trip, scales="free_x") + 
#   ylab("Depth (m)") + ggtitle("Copepod density")
# 
# p <- ggplot(densities, aes(x=Interval, y=Layer, fill=log10(n.cladocerans))) +
#   geom_tile() + 
#   scale_fill_viridis(limits=density_limits) +
#   scale_y_reverse(limits=depth_limits) + 
#   facet_grid(Lake ~ trip, scales="free_x") + 
#   ylab("Depth (m)") + ggtitle("Cladoceran density")
# 
# ggplot(zoop.ts, aes(x=TS, fill=Group)) + geom_histogram(position="dodge")
