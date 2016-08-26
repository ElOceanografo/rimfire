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

p <- ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group), data=countries, fill="#eeeeee") + 
  geom_path(aes(x=long, y=lat, group=group), data=states, color="dark grey") + 
  geom_path(aes(x=long, y=lat, group=group), data=countries) + 
  geom_polygon(aes(x=Longitude, y=Latitude, group=Lake), data=lakes) +
  geom_point(aes(x=long, y=lat), data=lake_centers) +
  geom_text_repel(aes(x=long, y=lat, label=Lake), data=lake_centers, nudge_x=3) +
  coord_map("sinusoidal", xlim=c(-125, -113), ylim=c(31, 43)) + theme_bw() +
  xlab("Longitude") + ylab("Latitude") 

ggsave("graphics/map_california.png", p, width=4, height=4, units="in")

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
  annotate("text", x = x0 + km.longitude / 2, y = y0 + 0.002, label = "1 km") +
  ggtitle("a.") +
  coord_map() + theme_bw() + theme(plot.title=element_text(hjust=0))

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
  annotate("text", x = x0 + 5*km.longitude / 2, y = y0 + 0.01, label = "5 km") +
  ggtitle("b.") +
  coord_map() + theme_bw() + theme(plot.title=element_text(hjust=0))

png("graphics/map_indy_tahoe.png", width = 4, height=8, units="in", res=150)
grid.arrange(p.ind, p.tahoe, ncol=1, heights=c(1, 2.1))
dev.off()

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

p.cherry.eleanor <- ggplot() +
  geom_polygon(aes(x=Longitude, y=Latitude, group=Lake), fill="light grey",
               data=lakes_ce) +
  geom_path(aes(x=Lon_M, y=Lat_M, group=Lake), 
            data=track_ce) +
  geom_line(aes(x, y), scale.bar, lwd=1.5) + 
  annotate("text", x = x0 + km.longitude / 2, y = y0 + 0.003, label = "1 km") +
  facet_wrap(~trip) + theme_minimal() + 
  theme(panel.border = element_rect(fill=NA)) +
  coord_map()

ggsave("graphics/map_cherry_eleanor.png", p.cherry.eleanor, width=7, height=7, units="in")

