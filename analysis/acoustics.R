library(ggplot2)
library(scales)
library(viridis)
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)

load("acoustics.Rdata")
load("net_data.Rdata")
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
  area = c(490, 2.6, 6.3, 3.9) # in km^2
)
lake_areas <- mutate(lake_areas, area = area * 1000^2) # convert areas to m^2
  
lake.biomass <- profiles %>%
  filter(class=="Sv_zoop", Layer_depth_max < 25) %>%
  group_by(Lake, trip) %>%
  summarise(biomass = mean(biomass),
            area.biomass = sum(biomass)) %>%
  left_join(lake_areas, by="Lake") %>%
  mutate(total = signif(area.biomass * area / 1e6, 3)) %>% # convert g to mt
  select(-area)

lake.biomass %>%
  select(-biomass, -area.biomass) %>%
  spread(trip, total)

lake.biomass %>%
  select(-total, -area.biomass) %>%
  spread(trip, biomass)

ggplot(filter(lake.biomass), aes(x=trip, y=biomass, fill=Lake)) +
  geom_bar(stat="identity", position="dodge")

ggplot(filter(lake.biomass), aes(x=trip, y=total, fill=Lake)) +
  geom_bar(stat="identity", position="dodge")


net.totals <- counts %>%
  filter(! Group %in% c("Rotifers", "Nematode", "Hydroids")) %>%
  group_by(trip, Lake) %>%
  summarize(count = sum(Count)) %>%
  ungroup() %>%
  left_join(net.meta) %>%
  mutate(num.density = count / VolFiltered / Dilution,
         biovolume = Biovolume / VolFiltered / TERCSplitFactor) %>%
  select(trip, Lake, count, biovolume, num.density)

net.totals <- left_join(net.totals, lake.biomass, by=c("trip", "Lake"))

net.totals.barplot <- net.totals
net.totals.barplot[is.na(net.totals.barplot)] <- 0
net.totals.dummy <- net.totals.barplot[grepl("2014", net.totals.barplot$trip), ]
net.totals.dummy$Lake <- gsub("Cherry", "Tahoe", net.totals.dummy$Lake)
net.totals.dummy$Lake <- gsub("Eleanor", "Independence", net.totals.dummy$Lake)
net.totals.dummy$biomass <- 0
net.totals.dummy$biovolume <- 0

net.totals.barplot <- rbind(net.totals.barplot,
                            net.totals.dummy)


p1 <- ggplot(net.totals.barplot, aes(x=trip, y=biovolume, fill=Lake)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("") + ylab(expression(Net~biovolume~(mL~m^-3))) +  ggtitle("a.") +
  scale_fill_grey() +
  theme_minimal() + 
  theme(panel.border = element_rect(fill="#00000000", colour="grey"),
        plot.title=element_text(hjust=0))

p2 <- ggplot(net.totals.barplot, aes(x=trip, y=biomass, fill=Lake)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_grey() +
  xlab("") + ylab(expression(Acoustic~biomass~(g~m^-3))) + ggtitle("b.") +
  theme_minimal() + 
  theme(panel.border = element_rect(fill="#00000000", colour="grey"),
        plot.title=element_text(hjust=0))
p <- gridExtra::grid.arrange(p1, p2, ncol=1)
ggsave("graphics/seasonal_biomass.png", p)


# Regression of net and acoustic biomass
mod.net.acoustic <- lm(biovolume ~ 0 + biomass, net.totals)
summary(mod.net.acoustic)
confint(mod.net.acoustic)

mod.net.acoustic.robust <- MASS::rlm(biovolume ~ 0 + biomass, net.totals)
summary(mod.net.acoustic.robust)

net.totals$label.y <- net.totals$biovolume
net.totals$label.y[net.totals$Lake == "Independence"] <- net.totals$label.y[net.totals$Lake == "Independence"] + 0.05
net.totals$label.y[net.totals$Lake == "Tahoe"] <- net.totals$label.y[net.totals$Lake == "Tahoe"] - 0.05


png("graphics/net_vs_acoustics.png", width=800, height=750, pointsize = 28)
mar.default <- par("mar")
par(mar=c(5, 5, 3, 2))
plot(biovolume~biomass, net.totals, xlim=c(0, 1.2), ylim=c(0, 3), pch=16, bty='n',
     xlab=expression(Acoustic~biomass~(g~m^-3)), 
     ylab=expression(Net~biovolume~(mL~m^-3)))
text(net.totals$biomass, net.totals$label.y, paste(net.totals$Lake, net.totals$trip), 
     pos=4, cex=0.8, col="#666666")
lines(0:1, predict(mod.net.acoustic, newdata=list(biomass=0:1)))
par(mar=mar.default)
dev.off()

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

