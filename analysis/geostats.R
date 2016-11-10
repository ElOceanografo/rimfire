library(ggplot2)
library(scales)
library(viridis)
library(reshape2)
library(dplyr)
library(gstat)
library(nlme)
EARTH_RADIUS = 6371

load("tracks.Rdata")
load("lake_outlines.Rdata")


xy <- mapproj::mapproject(tracks$Lon_M, tracks$Lat_M, proj="mercator")
tracks$x <- xy$x * EARTH_RADIUS
tracks$y <- xy$y * EARTH_RADIUS

xy <- mapproj::mapproject(lakes$Longitude, lakes$Latitude, proj="mercator")
lakes$x <- xy$x * EARTH_RADIUS
lakes$y <- xy$y * EARTH_RADIUS
lakes$z <- 0

inlet.xy <- data.frame(
  Lake = c("Cherry", "Eleanor", "Independence", "Tahoe"),
  inlet.x = c(20, 27.5, -27.5, -9),
  inlet.y = c(4578, 4574, 4778.5, 4730)
)

ggplot() + 
  geom_polygon(aes(x=x, y=y, group=Lake), 
               data=filter(lakes, Lake %in% c("Cherry", "Eleanor")),
               fill="light grey") +
  geom_point(aes(x, y, color=log10(density)), 
             data=filter(tracks, Lake %in% c("Cherry", "Eleanor")), alpha=0.2) +
  scale_color_viridis(oob=squish) +
  coord_equal() + xlab("x (m)") + ylab("y (m)") +
  facet_grid(class ~ trip) + 
  theme_bw()


ggplot() + 
  geom_polygon(aes(x=x, y=y, group=Lake), 
               data=filter(lakes, Lake == "Independence"), fill="light grey") +
  geom_point(aes(x, y, size=log10(density), color=log10(density)), 
             data=filter(tracks, Lake == "Independence"), alpha=0.2) +
  scale_color_viridis() +
  coord_equal() + xlab("x (m)") + ylab("y (m)") +
  theme_bw()

ggplot() + 
  geom_polygon(aes(x=x, y=y, group=Lake), 
               data=filter(lakes, Lake == "Tahoe"), fill="light grey") +
  geom_point(aes(x, y, size=log10(density), color=log10(density)), 
             data=filter(tracks, Lake == "Tahoe"), alpha=0.2) +
  scale_color_viridis() +
  coord_equal() + xlab("x (m)") + ylab("y (m)") +
  theme_bw()

ggplot(tracks, aes(x=Interval, y=Sv, color=class)) + 
  geom_line() + facet_wrap(~trip+Lake, scales="free_x")


filter(tracks, Lake=="Cherry", class=="Sv_zoop", trip=="2013-10") %>%
  ggplot(aes(x=Interval, y=sv)) + geom_line() + geom_point()


tracks <- filter(tracks, sv < 1e-4, Lake != "Tahoe") %>%
  left_join(inlet.xy) %>%
  mutate(inlet.dist = sqrt((inlet.x - x)^2 + (inlet.y - y)^2))


shore.dists <- list()
for (lake in c("Cherry", "Eleanor", "Independence")) {
  print(lake)
  tr <- filter(tracks, Lake==lake)
  lk <- filter(lakes, Lake==lake)
  dists <- rep(0, nrow(tr))
  for (i in 1:nrow(tr)) {
    xy <- tr[i, c("x", "y")]
    dx <- xy[[1]] - lk$x
    dy <- xy[[2]] - lk$y
    dists[i] <- min(sqrt(dx^2 + dy^2), na.rm=T)
  }
  shore.dists[[lake]] <- data.frame(
    trip = tr$trip,
    Lake = lake,
    Interval = tr$Interval,
    class = tr$class,
    shore.dist = dists)
}
shore.dists <- plyr::ldply(shore.dists, .id=NULL)
tracks <- left_join(tracks, shore.dists, by=c("trip", "Lake", "Interval", "class"))


ggplot() +
  geom_polygon(aes(x=x, y=y, group=Lake), 
               data=filter(lakes, Lake %in% c("Cherry", "Eleanor")),
               fill="light grey") +
  geom_point(data=filter(tracks, Lake %in% c("Cherry", "Eleanor")), 
             aes(x=x, y=y, color=shore.dist)) +
  scale_color_viridis() + coord_equal()

# modeling biomass
tracks %>%
  filter(class=="Sv_zoop") %>%
  mutate(shore.round = round(shore.dist, 1), inlet.round=round(inlet.dist)) %>%
  group_by(Lake, trip, shore.round, inlet.round) %>%
  summarise(biomass = mean(biomass)) %>%
  ggplot(aes(x=inlet.round, y=shore.round, fill=log10(biomass))) +
  geom_tile() + scale_fill_viridis() +
  facet_grid(Lake ~ trip)

tracks %>%
  filter(class=="Sv_fish") %>%
  mutate(shore.round = round(shore.dist, 1), inlet.round=round(inlet.dist)) %>%
  group_by(Lake, trip, shore.round, inlet.round) %>%
  summarise(density = mean(density)) %>%
  ggplot(aes(x=inlet.round, y=shore.round, fill=log10(density))) +
  geom_tile() + scale_fill_viridis() +
  facet_grid(Lake ~ trip)


## Plots of biomass vs. inlet distance

p <- filter(tracks, class=="Sv_zoop") %>%
  ggplot(aes(x=inlet.dist, y=biomass)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") +
  scale_y_log10(name=expression(Biomass~(mg~m^-3))) +
  # annotation_logticks(sides="lr", color = "grey", linewidth=0.5) +
  xlab("Distance to inlet (km)") +
  facet_grid(Lake~trip, scales="free_x") +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
ggsave("graphics/inlet_distance.png", p, w=8, h=5, units="in")

p <- filter(tracks, class=="Sv_fish") %>%
  ggplot(aes(x=inlet.dist, y=density)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") +
  scale_y_log10(name=expression(Density~(fish~m^-3))) +
  # annotation_logticks(sides="lr", color = "grey", linewidth=0.5) +
  xlab("Distance to inlet (km)") +
  facet_grid(Lake~trip, scales="free_x") +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))

## Plots of biomass/density vs. shore distance

p <- filter(tracks, class=="Sv_zoop") %>%
  ggplot(aes(x=shore.dist, y=biomass)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") +
  scale_y_log10(name=expression(Biomass~(mg~m^-3))) +
  # annotation_logticks(sides="lr", color = "grey", linewidth=0.5) +
  xlab("Distance to shore (km)") +
  facet_grid(Lake~trip) +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
ggsave("graphics/shore_distance.png", p, w=8, h=5, units="in")

p <- filter(tracks, class=="Sv_fish") %>%
  ggplot(aes(x=shore.dist, y=density)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") +
  scale_y_log10(name=expression(Density~(fish~m^-3))) +
  # annotation_logticks(sides="lr", color = "grey", linewidth=0.5) +
  xlab("Distance to shore (km)") +
  facet_grid(Lake~trip) +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))


# correlation coefficients
tracks %>%
  filter(!is.na(shore.dist), !is.na(inlet.dist)) %>%
  group_by(Lake, trip) %>%
  summarize(r = cor(shore.dist, inlet.dist)^2) %>%
  dcast(Lake ~ trip)

######################
# Modeling zooplankton
######################
tracks.zoop <- filter(tracks, class=="Sv_zoop")
tracks.fish <- filter(tracks, class=="Sv_fish", Lake != "Independence")

filter(tracks.zoop, Lake=="Cherry", trip=="2013-10") %>%
ggplot(aes(x=Interval, y=biomass)) + 
  geom_point() + geom_line()

i <- which(tracks.zoop$Lake=="Cherry" & tracks.zoop$trip=="2013-10" & tracks.zoop$Interval==90)
tracks.zoop$biomass[i] <- tracks.zoop$biomass[i-1]


models <- plyr::dlply(tracks.zoop, c("Lake", "trip"), function(df) {
  lm(log10(biomass) ~ inlet.dist + shore.dist, data=df)
})

(model.summary <- plyr::ldply(models, function(res) {
  beta <- signif(res$coefficients, 3)
  s <- summary(res)
  r2 <- round(s$adj.r.squared, 2)
  p.intercept <- signif(s$coefficients[1, 4], 3)
  p.inlet <- signif(s$coefficients[2, 4], 3)
  p.shore <- signif(s$coefficients[3, 4], 3)
  c(beta, R2=r2, p.intercept=p.intercept, p.inlet=p.inlet, p.shore=p.shore)
}))
write.csv(model.summary, "spatial_model_summary.csv")

tracks.zoop$pred <- 0
lake <- "Independence"
trip <- "2013-10"
ii <- tracks.zoop$Lake == lake & tracks.zoop$trip == trip
mod <- models[[paste(lake, trip, sep=".")]]
tracks.zoop$pred[ii] <- predict(mod, newdata=tracks.zoop[ii, ])
for (lake in c("Cherry", "Eleanor")) {
  for (trip in c("2013-10", "2014-04", "2014-06", "2014-09")) {
    ii <- tracks.zoop$Lake == lake & tracks.zoop$trip == trip
    mod <- models[[paste(lake, trip, sep=".")]]
    tracks.zoop$pred[ii] <- predict(mod, newdata=tracks.zoop[ii, ])
  }
}
tracks.zoop <- mutate(tracks.zoop, resid = log10(biomass) - pred)

vg.emp.zoop <- plyr::dlply(tracks.zoop, c("trip", "Lake"), function (df) {
  span <- sqrt(diff(range(df$x))^2 + diff(range(df$y))^2)
  z <- df$resid / sd(df$resid)
  variogram(resid ~ 1, locations = ~ x + y, data=na.exclude(df),
             cutoff=span/1.9, width=0.25) 
  })

vg.emp.df.zoop <- plyr::ldply(vg.emp.zoop, function(v) data.frame(dist = v$dist, gamma=v$gamma))

p <- ggplot(vg.emp.df.zoop, aes(x=dist, y=gamma, linetype=Lake)) +
  geom_line() + #geom_point() +
  ylab(expression(gamma)) + xlab("Lag (km)") +
  facet_grid(. ~ trip) +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
p
ggsave("graphics/variograms.png", p, width=10, height=3, units="in")

###############
# Modeling fish
###############

filter(tracks.fish, Lake=="Cherry", trip=="2013-10") %>%
ggplot(aes(x=y, y=density, color=Interval)) + 
  geom_path() + geom_point() +
  facet_grid(Lake ~ trip) + scale_y_log10()

vg.emp.fish <- plyr::dlply(tracks.fish, c("trip", "Lake"), function (df) {
  span <- sqrt(diff(range(df$x))^2 + diff(range(df$y))^2)
  variogram(sqrt(density) ~ 1, locations = ~ x + y, data=df,
             cutoff=2, width=0.1) 
  })

vg.emp.df.fish <- plyr::ldply(vg.emp.fish, function(v) data.frame(dist = v$dist, gamma=v$gamma))

p <- filter(vg.emp.df.fish, ! (Lake == "Cherry" & trip == "2013-10")) %>%
  ggplot(aes(x=dist, y=gamma, linetype=Lake)) +
  geom_line() + #geom_point() +
  ylab(expression(gamma)) + xlab("Lag (km)") +
  facet_grid(. ~ trip) +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
p
