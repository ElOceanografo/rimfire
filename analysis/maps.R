library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)

load("tracks.Rdata")
load("lake_outlines.Rdata")

# Large-scale map of California
countries <- map_data("world") %>%
  filter(region %in% c("USA", "Canada", "Mexico"))
states <- map_data("state") %>%
  filter(region %in% c("oregon", "idaho", "nevada", "arizona"))

lake_centers <- lakes %>%
  group_by(Lake) %>%
  summarise(long = mean(Longitude),
            lat = mean(Latitude))

p.cal <- ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), data=countries, fill="#eeeeee") + 
  geom_path(aes(x=long, y=lat, group=group), data=states, color="dark grey") + 
  geom_path(aes(x=long, y=lat, group=group), data=countries) + 
  geom_polygon(aes(x=Longitude, y=Latitude, group=Lake), data=lakes) +
  geom_point(aes(x=long, y=lat), data=lake_centers) +
  geom_text_repel(aes(x=long, y=lat, label=Lake), data=lake_centers, nudge_x=3) +
  coord_map("sinusoidal", xlim=c(-125, -113), ylim=c(30, 44)) + theme_bw() +
  ggtitle("a") + theme(plot.title=element_text(hjust=0)) +
  scale_x_continuous(breaks=c(-125, -120, -115)) +
  xlab("Longitude") + ylab("Latitude") 


# Independence and Tahoe
# Independence
km.longitude <- 1 / (111 * cos(39.2* pi / 180))
x0 <- -120.3
y0 <- 39.432
scale.bar <- data.frame(
  x = c(x0, x0 + km.longitude),
  y = c(y0, y0)
)
p.ind <- ggplot() +
  geom_polygon(aes(x=Longitude, y=Latitude, group=Lake), fill="light grey",
               data=filter(lakes, Lake=="Independence")) +
  geom_path(aes(x=Lon_M, y=Lat_M, group=Lake), 
            data=filter(tracks, Lake=="Independence")) +
  geom_line(aes(x, y), scale.bar, lwd=1.5) + 
  annotate("text", x = x0 + km.longitude / 2, y = y0 + 0.003, label = "1 km") +
  ggtitle("b") +
  scale_x_continuous(breaks=c(-120.33, -120.31, -120.29)) +
  coord_map(ylim=c(39.41, 39.47), xlim=c(-120.33, -120.28)) + 
  theme_bw() + theme(plot.title=element_text(hjust=0))

# Tahoe
x0 <- -120.16
y0 <- 39.23
scale.bar <- data.frame(
  x = c(x0, x0 + 5*km.longitude),
  y = c(y0, y0)
)
p.tahoe <- ggplot() +
  geom_polygon(aes(x=Longitude, y=Latitude, group=Lake), fill="light grey",
               data=filter(lakes, Lake=="Tahoe")) +
  geom_line(aes(x=Lon_M, y=Lat_M, group=Lake), 
            data=filter(tracks, Lake=="Tahoe", Lat_M > 39)) +
  geom_line(aes(x, y), scale.bar, lwd=1.5) + 
  annotate("text", x = x0 + 5*km.longitude / 2, y = y0 + 0.02, label = "5 km") +
  ggtitle("c") +
  coord_map() + theme_bw() + theme(plot.title=element_text(hjust=0))

# Cherry and Eleanor
lakes_ce <- filter(lakes, Lake %in% c("Cherry", "Eleanor"))
track_ce <- filter(tracks, Lake %in% c("Cherry", "Eleanor"))
km.longitude <- 1 / (111 * cos(38 * pi / 180))
x0 <- -119.85
y0 <- 38.02
scale.bar <- data.frame(
  x = c(x0, x0 + km.longitude),
  y = c(y0, y0)
)

lake.labels <- data.frame(
  x = c(-119.885, -119.85),
  y = c(38.01, 38.0),
  label = c("Cherry", "Eleanor")
)

p.cherry.eleanor <- ggplot() +
  geom_polygon(aes(x=Longitude, y=Latitude, group=Lake), fill="light grey",
               data=lakes_ce) +
  geom_text(aes(x=x, y=y, label=label), lake.labels, col="#555555") +
  geom_path(aes(x=Lon_M, y=Lat_M, group=Lake), 
            data=track_ce) +
  geom_line(aes(x, y), scale.bar, lwd=1.5) + 
  annotate("text", x = x0 + km.longitude / 2, y = y0 + 0.003, label = "1 km") +
  facet_wrap(~trip) + theme_minimal() + 
  ggtitle("d") + theme(panel.border = element_rect(fill=NA), plot.title=element_text(hjust=0)) +
  coord_map()

lay <- matrix(c(1, 2, 3, 
                4, 4, 4,
                4, 4, 4), byrow=T, nrow=3)
p.map <- grid.arrange(grobs=list(p.cal, p.ind, p.tahoe, p.cherry.eleanor), layout_matrix=lay)
ggsave("graphics/maps.png", p.map, width=7, height=10, units="in")


