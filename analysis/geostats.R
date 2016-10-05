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
  geom_point(aes(x, y, color=biomass), 
             data=filter(tracks, Lake %in% c("Cherry", "Eleanor")), alpha=0.2) +
  scale_color_viridis(oob=squish) +
  coord_equal() + xlab("x (m)") + ylab("y (m)") +
  facet_wrap(~ trip) + 
  theme_bw()


ggplot() + 
  geom_polygon(aes(x=x, y=y, group=Lake), 
               data=filter(lakes, Lake == "Independence"), fill="light grey") +
  geom_point(aes(x, y, size=biomass, color=biomass), 
             data=filter(tracks, Lake == "Independence"), alpha=0.2) +
  scale_color_viridis() +
  coord_equal() + xlab("x (m)") + ylab("y (m)") +
  theme_bw()

ggplot() + 
  geom_polygon(aes(x=x, y=y, group=Lake), 
               data=filter(lakes, Lake == "Tahoe"), fill="light grey") +
  geom_point(aes(x, y, size=biomass, color=biomass), 
             data=filter(tracks, Lake == "Tahoe"), alpha=0.2) +
  scale_color_viridis() +
  coord_equal() + xlab("x (m)") + ylab("y (m)") +
  theme_bw()

ggplot(tracks, aes(x=Interval, y=biomass, color=Lake)) + 
  geom_path() + facet_wrap(~trip+Lake, scales="free")


filter(tracks, Lake=="Cherry", trip=="2013-10") %>%
  ggplot(aes(x=Interval, y=sv)) + geom_line()


tracks <- filter(tracks, biomass < 500, Lake != "Tahoe") %>%
  left_join(inlet.xy) %>%
  mutate(inlet.dist = sqrt((inlet.x - x)^2 + (inlet.y - y)^2))


shore.dists <- list()
for (lake in c("Cherry", "Eleanor", "Independence")) {
  print(lake)
  tr <- filter(tracks, Lake==lake)
  lk <- filter(lakes, Lake==lake)
  dists <- rep(0, nrow(tr))
  inter <- tr$Interval
  for (i in 1:nrow(tr)) {
    xy <- tr[i, c("x", "y")]
    dx <- xy[[1]] - lk$x
    dy <- xy[[2]] - lk$y
    dists[i] <- min(sqrt(dx^2 + dy^2))
  }
  shore.dists[[lake]] <- data.frame(
    Lake = lake,
    Interval = inter,
    shore.dist = dists)
}
shore.dists <- plyr::ldply(shore.dists, .id=NULL)
tracks <- left_join(tracks, shore.dists)

ggplot() +
  geom_polygon(aes(x=x, y=y, group=Lake), 
               data=filter(lakes, Lake %in% c("Cherry", "Eleanor")),
               fill="light grey") +
  geom_point(data=filter(tracks, Lake %in% c("Cherry", "Eleanor")), 
             aes(x=x, y=y, color=shore.dist)) +
  scale_color_viridis() + coord_equal()

# modeling biomass
tracks %>%
  mutate(shore.round = round(shore.dist, 1), inlet.round=round(inlet.dist)) %>%
  group_by(Lake, trip, shore.round, inlet.round) %>%
  summarise(biomass = mean(biomass)) %>%
  ggplot(aes(x=inlet.round, y=shore.round, fill=log10(biomass))) +
  geom_tile() + scale_fill_viridis() +
  facet_grid(Lake ~ trip)

p <- ggplot(tracks, aes(x=inlet.dist, y=biomass)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") +
  scale_y_log10(name=expression(Biomass~(mg~m^-3))) +
  # annotation_logticks(sides="lr", color = "grey", linewidth=0.5) +
  xlab("Distance to inlet (km)") +
  facet_grid(Lake~trip, scales="free_x") +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
ggsave("graphics/inlet_distance.png", p, w=8, h=5, units="in")

p <- ggplot(tracks, aes(x=shore.dist, y=biomass)) + 
  geom_point(size=0.5) +
  geom_smooth(method="lm") +
  scale_y_log10(name=expression(Biomass~(mg~m^-3))) +
  # annotation_logticks(sides="lr", color = "grey", linewidth=0.5) +
  xlab("Distance to shore (km)") +
  facet_grid(Lake~trip) +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
ggsave("graphics/shore_distance.png", p, w=8, h=5, units="in")

# correlation coefficients
tracks %>%
  group_by(Lake, trip) %>%
  summarize(r = cor(shore.dist, inlet.dist)^2) %>%
  dcast(Lake ~ trip)

models <- plyr::dlply(tracks, c("Lake", "trip"), function(df) {
  lm(log10(biomass) ~ inlet.dist + shore.dist, data=df)
})
(model.summary <- plyr::ldply(models, function(res) {
  beta <- coefficients(res)
  r2 <- summary(res)$adj.r.squared
  c(beta, R2=r2)
}))


mod <- lm(log10(biomass) ~ 0 + trip*Lake*(inlet.dist+shore.dist), data=tracks)
summary(mod)
tracks$resid <- resid(mod)

vg.emp <- plyr::dlply(filter(tracks), c("trip", "Lake"), function (df) {
  span <- sqrt(diff(range(df$x))^2 + diff(range(df$y))^2)
  z <- df$resid / sd(df$resid)
  variogram(resid ~ 1, locations = ~ x + y, data=na.exclude(df),
             cutoff=2, width=0.1) 
  })

vg.emp.df <- plyr::ldply(vg.emp, function(v) data.frame(dist = v$dist, gamma=v$gamma))

p <- ggplot(vg.emp.df, aes(x=dist, y=gamma, linetype=Lake)) +
  geom_line() + #geom_point() +
  ylab(expression(gamma)) + xlab("Lag (km)") +
  facet_grid(. ~ trip) +
  theme_minimal() + theme(panel.border = element_rect(fill="#00000000", colour="grey"))
p
ggsave("graphics/variograms.png", p, width=10, height=3, units="in")

