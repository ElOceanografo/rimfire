library(dplyr)
library(ggplot2)


load("acoustics.Rdata")
load("lake_outlines.Rdata")

tracklines <- group_by(echo, trip, Lake, freq, Interval) %>%
  summarise(Longitude = mean(Lon_M, na.rm=T), Latitude = mean(Lat_M, na.rm=T))

km.longitude <- 1 / (111 * cos(38 * pi / 180))
x0 <- -119.85
y0 <- 38.025
scale.bar <- data.frame(
  x = c(x0, x0 + km.longitude),
  y = c(y0, y0)
)  

p <- ggplot() + 
  geom_polygon(aes(x=Longitude, y=Latitude, group=Lake), data=lakes, fill="light grey") +
  geom_path(aes(x=Longitude, y=Latitude, group=Lake), tracklines) +
  geom_line(aes(x, y), scale.bar, lwd=1.5) + 
  annotate("text", x = x0 + km.longitude / 2, y = y0 - 0.003, label = "1 km") +
  facet_wrap( ~ trip) +
  coord_map() + theme_bw()
ggsave("graphics/tracklines.pdf", p, width=8, height=7.61, units="in")

