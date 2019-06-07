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
  summarise(density = mean(density, na.rm=TRUE)) %>%
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
p
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
p
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
  summarize(r = cor(shore.dist, inlet.dist)) %>%
  dcast(Lake ~ trip)
ggplot(tracks, aes(x=inlet.dist, y=shore.dist)) + geom_point() + facet_grid(Lake ~ trip)

######################
# Modeling zooplankton
######################
tracks.zoop <- filter(tracks, class=="Sv_zoop")
tracks.fish <- filter(tracks, class=="Sv_fish", Lake != "Independence")

filter(tracks.zoop, Lake=="Cherry", trip=="2013-10") %>%
  ggplot(aes(x=Interval, y=biomass)) + 
    geom_point() + geom_line()

models <- plyr::dlply(tracks.zoop, c("Lake", "trip"), function(df) {
  lm(log10(biomass) ~ inlet.dist + shore.dist, data=df)
})

(model.summary <- plyr::ldply(models, function(res) {
  beta <- signif(res$coefficients, 3)
  s <- summary(res)
  r2 <- round(s$adj.r.squared, 2)
  p.intercept <- round(s$coefficients[1, 4], 3)
  p.inlet <- round(s$coefficients[2, 4], 3)
  p.shore <- round(s$coefficients[3, 4], 3)
  c(beta, R2=r2, p.intercept=p.intercept, p.inlet=p.inlet, p.shore=p.shore)
}))
model.summary$p.intercept[model.summary$p.intercept < 0.001] <- "< 0.001"
model.summary$p.shore[model.summary$p.shore < 0.001] <- "< 0.001"
model.summary$p.inlet[model.summary$p.inlet < 0.001] <- "< 0.001"
model.summary <- model.summary %>%
  arrange(trip, Lake) %>%
  select(trip, Lake, `(Intercept)`, p.intercept, inlet.dist, p.inlet, shore.dist, p.shore, R2)
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
ggsave("graphics/variograms.png", p, width=7, height=2, units="in")
ggsave("graphics/variograms.eps", p, width=7, height=2, units="in")

#################################
# Kriging and biomass integration
#################################

grid.inside.polygon <- function(x, y, dx, dy) {
  range.x <- range(x)
  range.y <- range(y)
  range.x <- range.x - ((range.x + c(-dx, dx)) %% dx)
  range.y <- range.y - ((range.y + c(-dy, dy)) %% dy)
  grid <- expand.grid(seq(range.x[1], range.x[2], by=dx),
                      seq(range.y[1], range.y[2], by=dy))
  names(grid) <- c("x", "y")
  in.lake <- sp::point.in.polygon(grid$x, grid$y, x, y)
  in.lake <- in.lake == 1
  return(grid[in.lake, ])
}

indy <- filter(lakes, Lake=="Independence")
g <- grid.inside.polygon(indy$x, indy$y, .1, .1)
plot(y ~ x, indy)
points(g$x, g$y, col="red")

vg.model.zoop <- plyr::llply(vg.emp.zoop, fit.variogram, model=vgm(0.2, "Sph", 1))
# Not enough spatial extent/data in Cherry to fit a meaningful
# variogram model, assume Eleanor one is okay for this date
vg.model.zoop$`2014-04.Cherry` <- vg.model.zoop$`2014-04.Eleanor`

lake.grids <- plyr::ddply(filter(lakes, Lake!="Tahoe"), "Lake", function(df) {
  grid <- grid.inside.polygon(df$x, df$y, dx=0.1, dy=0.1)
  grid$x <- round(grid$x, 1)
  grid$y <- round(grid$y, 1)
  return(grid)
})

lake.grids <- lake.grids %>%
  left_join(inlet.xy) %>%
  mutate(inlet.dist = sqrt((inlet.x - x)^2 + (inlet.y - y)^2))

lake.grids$shore.dist <- 0
for (Lake in c("Cherry", "Eleanor", "Independence")) {
  shore <- filter(lakes, Lake==Lake)
  ii <- lake.grids$Lake == Lake
  dists <- rep(0, sum(ii))
  for (j in 1:length(dists)) {
    dx <- lake.grids$x[ii][j] - shore$x
    dy <- lake.grids$y[ii][j] - shore$y
    dists[j] <- min(sqrt(dx^2 + dy^2), na.rm=T)
  }
  lake.grids$shore.dist[ii] <- dists
}
ggplot(lake.grids, aes(x=x, y=y, color=shore.dist)) + 
  geom_point() + scale_color_viridis() +
  facet_wrap(~Lake, scales="free")
lake.grids.list <- plyr::dlply(lake.grids, "Lake")
for (i in 1:length(lake.grids.list)) {
  sp::coordinates(lake.grids.list[[i]]) <- ~ x + y
}


zoop.kriged <- list()
for (trip.lake in names(vg.model.zoop)) {
  trip <- strsplit(trip.lake, ".", fixed=TRUE)[[1]][[1]]
  Lake <- strsplit(trip.lake, ".", fixed=TRUE)[[1]][[2]]
  grid <- lake.grids.list[[Lake]]
  vg <- vg.model.zoop[[trip.lake]]
  data <- tracks.zoop[tracks.zoop$Lake==Lake & tracks.zoop$trip==trip, ]
  print(paste(trip.lake, trip, Lake, nrow(data)))
  sp::coordinates(data) <- ~ x + y
  gs <- gstat(NULL, "logbiomass", log10(biomass) ~ 1,#shore.dist + inlet.dist, 
              data=data, model=vg)
  zoop.kriged[[trip.lake]] <- predict(gs, grid)
}

zoop.kriged <- plyr::ldply(zoop.kriged, function(spdf) {
  df <- data.frame(cbind(sp::coordinates(spdf), spdf$logbiomass.pred, spdf$logbiomass.var))
  names(df) <- c("x", "y", "logbiomass.pred", "logbiomass.var")
  return(df)
})
zoop.kriged <- cbind(colsplit(zoop.kriged$.id, "\\.", c("trip", "Lake")), zoop.kriged)
zoop.kriged <- zoop.kriged %>%
  mutate(logbiomass.sd = sqrt(logbiomass.var),
         logbiomass.lo = logbiomass.pred - 1.96 * logbiomass.sd,
         logbiomass.hi = logbiomass.pred + 1.96 * logbiomass.sd)
  
zoop.kriged %>%
  filter(Lake == "Independence") %>%
  ggplot(aes(x=x, y=y, fill=logbiomass.pred)) +
    geom_raster() + scale_fill_viridis() 

zoop.kriged %>%
  filter(Lake != "Independence") %>%
  tidyr::gather("var", "value", logbiomass.pred, logbiomass.sd) %>%
  ggplot() +
    geom_raster(aes(x=x, y=y, fill=value)) + scale_fill_viridis() +#limits=c(0, 10)) +
    geom_path(aes(x=x, y=y, group=Lake), data=filter(tracks.zoop, Lake!="Independence")) +
    facet_grid(var ~ trip) + coord_equal()

zoop.integrated <-  zoop.kriged %>%
  group_by(trip, Lake) %>%
  # integration cells are 100m^2, divided by 1000 g/kg
  summarise(biomass.lo = sum(10^logbiomass.lo) * 100^2/1e3,
            biomass = sum(10^logbiomass.pred) * 100^2/1e3,
            biomass.hi = sum(10^logbiomass.hi) * 100^2/1e3) %>%
  ungroup()

ybreaks = c(1e3, 1e4, 1e5, 1e6)
ymbreaks = c(seq(1e3, 1e4, 1e3), seq(1e4, 1e5, 1e4), seq(1e5, 1e6, 1e5))
ggplot(zoop.integrated, aes(x=trip, y=biomass, shape=Lake, linetype=Lake)) +
  geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=biomass.lo, ymax=biomass.hi), 
                position=position_dodge(0.2), width=0.18) +
  xlab("Date") + 
  scale_y_log10(expression(Biomass~(kg)), 
                breaks=ybreaks, minor_breaks=ymbreaks, 
                labels=trans_format('log10',math_format(10^.x))) +
  theme_minimal()
