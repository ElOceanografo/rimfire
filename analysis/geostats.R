library(ggplot2)
library(scales)
library(viridis)
library(reshape2)
library(dplyr)

library(gstat)
EARTH_RADIUS = 6371

load("tracks.Rdata")
load("lake_outlines.Rdata")


tracks <- filter(tracks, class=="zoop")
xy <- mapproj::mapproject(tracks$Lon_M, tracks$Lat_M, proj="mercator")
tracks$x <- xy$x * EARTH_RADIUS
tracks$y <- xy$y * EARTH_RADIUS

xy <- mapproj::mapproject(lakes$Longitude, lakes$Latitude, proj="mercator")
lakes$x <- xy$x * EARTH_RADIUS
lakes$y <- xy$y * EARTH_RADIUS
lakes$z <- 0

ggplot() + 
  geom_polygon(aes(x=x, y=y, group=Lake), data=lakes, fill="light grey") +
  geom_point(aes(x, y, size=sv, color=sv), data=tracks, alpha=0.2) +
  scale_color_viridis(limits=c(0, 1e-6), oob=squish) +
  coord_equal() + xlab("x (m)") + ylab("y (m)") +
  facet_grid(. ~ trip) + 
  theme_bw()

# 
# zl <- range(tracks$sv, na.rm=T)
# par(mfrow=c(2, 2), mar=rep(0, 4))
# for (t in unique(tracks$trip)) {
#   df <- filter(tracks, trip == t)
#   plot3D::polygon3D(lakes$x, lakes$y, lakes$z, zlim=zl, box=F, d=1.5)
#   plot3D::scatter3D(df$x, df$y, df$sv, type="h", pch=NA, add=T,
#                     col=magma(24))
# }
# par(mfrow=c(1, 1))

vg.emp <- plyr::dlply(filter(tracks, bottom > 15), c("trip", "Lake"), function (df) {
   variogram(log(sv) ~ 1, locations = ~ x + y, data=na.exclude(df),
             cutoff=3) 
  })

vg.emp.df <- plyr::ldply(vg.emp, function(v) data.frame(dist = v$dist, gamma=v$gamma))

ggplot(vg.emp.df, aes(x=dist, y=gamma, color=Lake)) +
  geom_line() + geom_point() +
  ylim(0, 1.25) + ylab(expression(gamma)) + xlab("Lag (km)") +
  facet_grid(. ~ trip) +
  theme_bw()
