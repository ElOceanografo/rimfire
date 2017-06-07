library(ggplot2)
library(scales)
library(viridis)
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)

load("acoustics.Rdata")
load("net_data.Rdata")
zoop.ts <- read.csv("nets/zoop_ts.csv", stringsAsFactors = F)
fish.ts <- read.csv("fish_TS.csv", stringsAsFactors = F)

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
           trip+Lake+Date_M+hour+minute+datetime+Interval+Layer_depth_max ~ freq,
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
echo <- filter(echo, Sv_fish < -40) # eliminate a few noise spikes I didn't catch in EV


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
  facet_grid(Lake ~ trip, scales="free_x") + 
  ylab("Depth (m)") + ggtitle("Zooplankton")
ggsave("graphics/echo_zooplankton.png", p, width=12.5, height=7.61, units="in")


p <- ggplot(echo, aes(x=Interval, y=Layer_depth_max, fill=Sv_fish)) +
  geom_tile() + 
  scale_y_reverse(limits=c(60, 0)) + 
  scale_fill_viridis(limits=c(-80, -40), oob=squish) +
  facet_grid(Lake ~ trip, scales="free_x") +
  ylab("Depth (m)") + ggtitle("Fish")
ggsave("graphics/echo_fish.png", p, width=12.5, height=7.61, units="in")


# Example echogram plot

echogram.120 <- read.csv("../acoustics/Exports/cherry.2014.06.24.120kHz.echogram_figure.csv") %>%
  mutate(freq = 120) %>%
  mutate(datetime = ymd_hms(paste(Date_M, Time_M), tz="UTC")) %>%
  filter(Interval > 3700)

echogram.710 <- read.csv("../acoustics/Exports/cherry.2014.06.24.710kHz.echogram_figure.csv") %>%
  mutate(freq = 710) %>%
  mutate(datetime = ymd_hms(paste(Date_M, Time_M), tz="UTC")) %>%
  filter(Interval > 3700)

save(echogram.120, echogram.710, file="example_echogram_data.Rdata")



with(echogram.120,
     oce::geodDist(first(Lon_M), first(Lat_M), last(Lon_M), last(Lat_M))
)

echogram <- rbind(echogram.120, echogram.710) %>%
  mutate(width = 1,
         datetime = datetime - hm("07:00"))

db.120 <- select(echogram.120, Layer_depth_max, datetime, Sv_mean)
db.710 <- select(echogram.710, Layer_depth_max, datetime, Sv_mean)
db.diff <- left_join(db.120, db.710, by=c("Layer_depth_max", "datetime")) %>%
  mutate(delta = Sv_mean.x - Sv_mean.y,
         class = "Zoop",
         datetime = datetime - hm("07:00"))
db.diff$class[db.diff$delta > 0] <- "Fish"
db.diff$class[db.diff$Sv_mean.y < -80] <- "Empty"

echo.col <- c("#FFFFFF", rev(viridis(24, option="A")))

p1 <-  ggplot() +
  geom_tile(aes(x=datetime, y=Layer_depth_max, fill=Sv_mean, width=width), 
            data=filter(echogram, freq == 120)) +
  scale_fill_gradientn(colors=echo.col, limits=c(-80, -50), oob=squish, name=expression(S[v~120])) +
  geom_point(aes(x=datetime, y=Layer_depth_max, color=Sv_mean),
             data=filter(echogram, freq == 120, Sv_mean > -75),
             shape=15, size=0.7) +
  scale_color_gradientn(colors=echo.col, limits=c(-80, -50), oob=squish, guide=F) +
  scale_x_datetime(expand=c(0, 0), name="Time") +
  scale_y_reverse(limits=c(20, 1.5), expand=c(0, 0), name="Depth (m)") + 
  ggtitle("a") + theme_bw() + 
  theme(plot.title=element_text(hjust=0),
      panel.background=element_rect(fill="grey50"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p2 <- filter(echogram, freq==710) %>%
  ggplot(aes(x=datetime, y=Layer_depth_max, fill=Sv_mean, width=1)) +
  geom_tile() +
  scale_x_datetime(expand=c(0, 0), name="Time") +
  scale_y_reverse(limits=c(20, 1.5), expand=c(0, 0), name="Depth (m)") + 
  scale_fill_gradientn(colors=echo.col, limits=c(-80, -50), oob=squish, name=expression(S[v~710])) +
  ggtitle("b") + theme_bw() + 
  theme(plot.title=element_text(hjust=0),
        panel.background=element_rect(fill="grey50"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p3 <-  ggplot() +
  geom_tile(aes(x=datetime, y=Layer_depth_max, fill=class, width=1), data=db.diff) +
  scale_fill_manual(labels=c("", "F", "Z"), name="Class",
                      values=c("white", "red", "lightskyblue")) +
  geom_point(aes(x=datetime, y=Layer_depth_max), data=filter(db.diff, class=="Fish"),
             color="red", shape=15, size=0.7) +
  scale_x_datetime(expand=c(0, 0), name="Time") +
  scale_y_reverse(limits=c(20, 1.5), expand=c(0, 0), name="Depth (m)") +
  ggtitle("c") + theme_bw() +
  theme(plot.title=element_text(hjust=0),
        panel.background=element_rect(fill="grey50"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png("graphics/example_echograms.png", width=7, height=6, units="in", res=300)
grid.arrange(p1, p2, p3, ncol=1)
dev.off()

################################################################################
# Depth profiles
################################################################################


mean.ts.zoop <- zoop.ts %>%
  filter(freq == 710) %>%
  group_by(trip, Lake) %>%
  summarize(sigma = sum(10^(TS/10) * proportion),
            TS = 10*log10(sigma),
            weight = sum(weight * proportion),
            volume = sum(volume * proportion)) %>%
  mutate(class = "Sv_zoop") %>%
  as.data.frame()
 
ts.display.table <- zoop.ts %>%
  filter(freq == 710) %>%
  mutate(percent = round(proportion*100),
         weight = round(weight * 1e6, 1),
         TS = round(TS, 1)) %>%
  select(-freq, -total, -proportion) %>%
  melt(measure.vars = c("TS", "weight", "n", "percent")) %>%
  dcast(trip + Lake ~ model + variable) %>%
  left_join(select(mean.ts.zoop, trip, Lake, TS, weight)) %>%
  mutate(TS = round(TS, 1),
         weight = round(weight * 1e6, 1))
write.csv(ts.display.table, "nets/ts.display.table.csv")

mean.ts.fish <- fish.ts %>%
  select(trip, Lake, sigma, TS, weight) %>%
  mutate(volume = NA, class = "Sv_fish")

mean.ts <- rbind(mean.ts.zoop, mean.ts.fish)

profiles <- echo %>%
  group_by(trip, Lake, Layer_depth_max) %>%
  summarise(Sv_zoop = dB.mean(Sv_zoop, na.rm=T),
            Sv_fish = dB.mean(Sv_fish, na.rm=T)) %>%
  melt(measure.vars=c("Sv_zoop", "Sv_fish"), variable.name="class", value.name="Sv") %>%
  mutate(sv = 10^(Sv / 10),
         class = as.character(class))
profiles$sv[is.na(profiles$sv)] <- 0
profiles <- left_join(profiles, mean.ts)
profiles <- mutate(profiles, 
                   density = sv / sigma, 
                   biomass = density * weight)

# aggregate the fish in bigger bins
profiles.fish <- profiles %>%
  filter(class=="Sv_fish") %>%
  mutate(depth = round(Layer_depth_max / 5 ) * 5) %>%
  group_by(Lake, trip, depth) %>%
  summarise(density=mean(density),
            biomass = mean(biomass))


p1 <- filter(profiles, class=="Sv_zoop", Layer_depth_max <= 25) %>% 
  ggplot(aes(y=Layer_depth_max, x=biomass, linetype=Lake, shape=Lake)) + 
  geom_point(size=1) + geom_path() +
  facet_wrap(~trip, nrow=1, scales="free_x") +
  scale_y_reverse(limits=c(25, 0), expand=c(0, 0)) + 
  ylab("Depth (m)") + xlab(expression(Zooplankton~biomass~(g~m^-3))) + 
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))

p2 <- ggplot(profiles.fish, aes(y=depth, x=biomass, linetype=Lake, shape=Lake)) + 
  geom_point() + geom_path() +
  facet_wrap(~trip, nrow=1, scales="free_x") +
  scale_y_reverse(limits=c(50, 0), expand=c(0, 0)) + 
  ylab("Depth (m)") + xlab(expression(Fish~biomass~(g~m^-3))) + 
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
p <- cowplot::plot_grid(p1, p2, ncol=1, rel_heights = c(31.5/50, 1))
ggsave("graphics/profiles.png", p, width=8, height=7, units="in")

################################################################################
# Average densities and biomasses by lake
################################################################################

lake_areas = data.frame(
  Lake = c("Tahoe", "Independence", "Cherry", "Eleanor"),
  area = c(490, 2.6, 6.3, 3.9) # in km^2,
)
lake_areas <- mutate(lake_areas, 
                     area = area * 1000^2, # convert areas to m^2
                     Lake = as.character(Lake))

lake.biomass <- profiles %>%
  filter(! (class=="Sv_zoop" & Layer_depth_max > 30)) %>%
  group_by(Lake, trip, class) %>%
  summarise(density = mean(density),
            biomass = mean(biomass),
            area.biomass = sum(biomass)) %>%
  left_join(lake_areas, by="Lake") %>%
  mutate(total = signif(area.biomass * area / 1e3, 3),# convert g to kg
         density = signif(density, 3),
         area.biomass = signif(area.biomass, 3),
         biomass = signif(biomass, 3)) %>% 
  select(-area)


reshape2::dcast(lake.biomass, Lake + class ~ trip, value.var="density")
reshape2::dcast(lake.biomass, Lake + class ~ trip, value.var="biomass")
reshape2::dcast(lake.biomass, Lake + class ~ trip, value.var="total")

lake.biomass.display <- lake.biomass %>%
  reshape2::dcast(Lake + trip ~ class, value.var="total") %>%
  mutate(ratio = Sv_zoop / Sv_fish) %>%
  arrange(trip, Lake) %>%
  rename(Zoop = Sv_zoop, Fish = Sv_fish) %>%
  mutate(Zoop = signif(Zoop, 2),
         Fish = signif(Fish, 2),
         ratio = signif(ratio, 2)) %>%
  select(trip, Lake, Zoop, Fish, ratio)
write.csv(lake.biomass.display, "lake_biomass_display.csv", row.names = F)

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

# placeholder rows in data frame so all bars in barplot have same width
net.totals.barplot <- net.totals
net.totals.barplot$biovolume[is.na(net.totals.barplot$biovolume)] <- 0.1
net.totals.dummy <- net.totals.barplot[grepl("2014", net.totals.barplot$trip), ]
net.totals.dummy$Lake <- gsub("Cherry", "Tahoe", net.totals.dummy$Lake)
net.totals.dummy$Lake <- gsub("Eleanor", "Independence", net.totals.dummy$Lake)
net.totals.dummy$biomass <- 0
net.totals.dummy$biovolume <- 0

net.totals.barplot <- rbind(net.totals.barplot,
                            net.totals.dummy)
ii <- net.totals.barplot$Lake == "Tahoe" & net.totals.barplot$trip == "2013-10"
# nudge the Tahoe 2013-10 values up just slightly so the bar shows up on the plot
net.totals.barplot$biomass[ii] <- net.totals.barplot$biomass[ii] + 0.005
net.totals.barplot$biovolume[ii] <- net.totals.barplot$biovolume[ii] + 0.02

p1 <- ggplot(net.totals.barplot, aes(x=trip, y=biovolume, fill=Lake)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("") + ylab(expression(Net~biovolume~(mL~m^-3))) +  ggtitle("a") +
  scale_fill_grey(start=0.8, end=0) +
  theme_minimal() + 
  theme(panel.border = element_rect(fill="#00000000", colour="grey"),
        plot.title=element_text(hjust=0))

p2 <- ggplot(net.totals.barplot, aes(x=trip, y=biomass, fill=Lake)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_grey(start=0.8, end=0) +
  xlab("") + ylab(expression(Acoustic~biomass~(g~m^-3))) + ggtitle("b") +
  theme_minimal() + 
  theme(panel.border = element_rect(fill="#00000000", colour="grey"),
        plot.title=element_text(hjust=0))
p <- gridExtra::grid.arrange(p1, p2, ncol=1)
ggsave("graphics/seasonal_biomass.png", p, width=6, height=5, units="in")


# Regression of net and acoustic biomass
net.totals <- counts %>%
  filter(! Group %in% c("Rotifers", "Nematode", "Hydroids")) %>%
  group_by(trip, Lake) %>%
  summarize(count = sum(Count)) %>%
  ungroup() %>%
  left_join(net.meta) %>%
  mutate(Biovolume = replace(Biovolume, is.na(Biovolume), 0.1)) %>%
  mutate(num.density = count / VolFiltered / Dilution,
         biovolume = Biovolume / VolFiltered / TERCSplitFactor) %>%
  select(trip, Lake, count, biovolume, num.density)

net.times <- read.csv("nets/net_times.csv") %>%
  mutate(net.time = ymd_hm(net.time))
# 
# plot(Sv_zoop ~ datetime, filter(echo, Lake=="Eleanor", trip=="2013-10"))
# 
# filter(echo, Lake=="Cherry", trip=="2014-04") %>%
#   ggplot(aes(x=Interval, y=Layer_depth_max, fill=Sv_zoop)) + 
#   geom_tile() + scale_y_reverse() + scale_fill_viridis(limits=c(-80, -60))

net.echo <- echo %>%
  mutate(datetime = ymd_hm(paste(Date_M, hour, minute))) %>%
  filter(class=="Zooplankton") %>%
  select(trip, Lake, datetime, Layer_depth_max, Sv_zoop) %>%
  left_join(net.times) %>%
  mutate(delta = abs(datetime - net.time)) %>%
  filter(delta < 60 * 10, Layer_depth_max < 30) %>%
  left_join(mean.ts.zoop) %>%
  mutate(sv_zoop = 10^(Sv_zoop/10),
         density_acoustic = sv_zoop / sigma,
         biomass_acoustic = density_acoustic * weight) %>%
  group_by(trip, Lake) %>%
  summarize(sv = mean(sv_zoop, na.rm=T),
            density_acoustic = mean(density_acoustic, na.rm=T),
            biomass_acoustic = mean(biomass_acoustic, na.rm=T)) %>%
  ungroup()  %>%
  left_join(net.totals) %>%
  left_join(mean.ts.zoop) %>%
  mutate(biovolume.calc = num.density * volume ,
         biomass.calc = num.density * weight) %>%
  rename(biovolume_gc = biovolume)
net.echo
pairs(~ sv + density_acoustic + biomass_acoustic + biovolume_gc + num.density + biovolume.calc,
      net.echo, pch=16, cex=2)

net.echo.sub <- filter(net.echo, !(Lake=="Eleanor" & trip=="2014-04"))
mod.net.acoustic <- lm(biovolume_gc ~ 0 + biomass_acoustic, net.echo.sub)
summary(mod.net.acoustic)
confint(mod.net.acoustic)

mod.net.acoustic.outlier <- lm(biovolume_gc ~ 0 + biomass_acoustic, net.echo)
summary(mod.net.acoustic.outlier)
confint(mod.net.acoustic.outlier)


net.echo$label.y <- net.echo$biovolume_gc
net.echo$label.x <- net.echo$biomass_acoustic
select(net.echo, Lake, trip, label.x, label.y)

# net.echo$label.y[3] <- net.echo$label.y[3] + 0.02
net.echo$label.y[4] <- net.echo$label.y[4] + 0.15
net.echo$label.x[4] <- net.echo$label.x[4] - 0.5
net.echo$label.y[2] <- net.echo$label.y[2] - 0.13
net.echo$label.x[2] <- net.echo$label.x[2] - 0.5
net.echo$label.y[1] <- net.echo$label.y[1] - 0.03
net.echo$label.x[6] <- net.echo$label.x[6] - 5

png("graphics/net_vs_acoustics.png", width=1000, height=700, pointsize = 24)
mar.default <- par("mar")
par(mar=c(5, 5, 3, 2))
plot(biovolume_gc ~ biomass_acoustic, net.echo.sub, xlim=c(0, 20), ylim=c(-0.1, 2.5),
     pch=16, bty='n', xlab=expression(Acoustic~biomass~(g~m^-3)),
     ylab=expression(Net~biovolume~(mL~m^-3)))
points(biovolume_gc ~ biomass_acoustic, net.echo[6, ])
text(net.echo$label.x, net.echo$label.y, paste(net.totals$Lake, net.totals$trip), 
     pos=4, cex=0.8, col="#666666")
lines(0:15, predict(mod.net.acoustic, newdata=list(biomass_acoustic=0:15)))
lines(0:15, predict(mod.net.acoustic.outlier, newdata=list(biomass_acoustic=0:15)), lty=2)
par(mar=mar.default)
dev.off()


################################################################################
# Track lines
################################################################################
echo$Sv_zoop[echo$Layer_depth_max > 30] <- NA
tracks <- echo %>%
  group_by(trip, Lake, Interval) %>%
  summarise(Lat_M = mean(Lat_M, na.rm=T),
            Lon_M = mean(Lon_M, na.rm=T),
            Sv_zoop = dB.mean(Sv_zoop, na.rm=T),
            Sv_fish = dB.mean(Sv_fish, na.rm=T),
            bottom = max(Layer_depth_max)) %>%
  melt(measure.vars=c("Sv_zoop", "Sv_fish"), variable.name="class", value.name="Sv") %>%
  mutate(sv = 10^(Sv / 10))
# tracks$sv[is.na(tracks$sv)] <- 0
tracks <- left_join(tracks, mean.ts)
tracks <- mutate(tracks, 
                 density = sv / sigma, 
                 biomass = density * weight)
tracks <- filter(tracks, is.finite(Lon_M), is.finite(Lat_M))

save(tracks, file="tracks.Rdata")

