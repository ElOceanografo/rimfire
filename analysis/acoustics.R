library(ggplot2)
library(viridis)
library(reshape2)
library(dplyr)
library(lubridate)

load("acoustics.Rdata")
zoop.ts <- read.csv("nets/zoop_ts.csv")
zoop.ts <- mutate(zoop.ts,
                  freq = as.factor(freq),
                  trip = as.character(trip),
                  Lake = as.character(Lake),
                  sigma = 10^(TS/10),
                  sigma.total = sigma * proportion)

echo <- group_by(zoop.ts, trip, Lake, freq) %>%
  summarize(sigma.total = sum(sigma.total, na.rm=T)) %>%
  right_join(echo, by=c("trip", "Lake", "freq"))

proportions <- dcast(zoop.ts, trip + Lake + freq ~ Group, value.var="proportion")
echo <- left_join(echo, proportions, by=c("trip", "Lake", "freq"))

echo <- mutate(echo,
               sv_mean = 10^(Sv_mean / 10),
               n.total = sv_mean / sigma.total,
               n.copepods = n.total * Copepods,
               n.cladocerans = n.total * Cladocerans)


# echograms
color_scale_limits = c(-120, -40)
depth_limits = c(50, 0)

for (lakename in c("Cherry", "Eleanor")) {
  p <- ggplot(filter(echo, Lake==lakename),
         aes(x=Interval, y=Layer, fill=Sv_mean)) +
    geom_tile() + 
    scale_fill_viridis(limits=color_scale_limits) +
    scale_y_reverse(limits=depth_limits) + 
    facet_grid(freq ~ trip, scales="free_x") + 
    ylab("Depth (m)") + ggtitle(lakename)
  # print(p)
  ggsave(paste0("graphics/", lakename, "_echograms.png"), p, width=12.5, height=7.61, units="in")
}


dB.mean <- function(x, na.rm=T) 10 * log10(mean(10^(x/10), na.rm=na.rm))

profiles <- dcast(echo, trip + Lake + freq + Layer ~ ., dB.mean, na.rm=T,
                  value.var="Sv_mean")
profiles <- rename(profiles, Sv = .)

p <- ggplot(profiles, aes(x=Layer, y=Sv, color=Lake)) + 
  geom_point() + geom_smooth() +
  facet_grid(freq ~ trip) + 
  scale_x_reverse() + coord_flip() + #limits=c(50, 0)) 
  xlab("Depth (m)") + ylab("Mean Sv") + 
  ggtitle("Backscatter depth profiles") +
  theme_bw()
ggsave("graphics/acoustic_profiles.pdf", p, width=12.5, height=7.61, units="in")

p <- ggplot(profiles, aes(x=Layer, y=Sv, color=trip)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(se=F) +
  facet_grid(freq ~ Lake) + 
  scale_x_reverse() + coord_flip() + #limits=c(50, 0)) 
  xlab("Depth (m)") + ylab("Mean Sv") + 
  ggtitle("Backscatter depth profiles") +
  theme_bw()
ggsave("graphics/acoustic_profiles_date.pdf", p, width=8, height=8, units="in")


densities <- echo %>%
  filter(freq == 710) %>%
  select(trip, Lake, Interval, Layer, n.total, n.copepods, n.cladocerans)


density_limits <- c(0, 8)
p <- ggplot(densities, aes(x=Interval, y=Layer, fill=log10(n.total))) +
  geom_tile() + 
  scale_fill_viridis(limits=density_limits) +
  scale_y_reverse(limits=depth_limits) + 
  facet_grid(Lake ~ trip, scales="free_x") + 
  ylab("Depth (m)") + ggtitle("Total density")

p <- ggplot(densities, aes(x=Interval, y=Layer, fill=log10(n.copepods))) +
  geom_tile() + 
  scale_fill_viridis(limits=density_limits) +
  scale_y_reverse(limits=depth_limits) + 
  facet_grid(Lake ~ trip, scales="free_x") + 
  ylab("Depth (m)") + ggtitle("Copepod density")

p <- ggplot(densities, aes(x=Interval, y=Layer, fill=log10(n.cladocerans))) +
  geom_tile() + 
  scale_fill_viridis(limits=density_limits) +
  scale_y_reverse(limits=depth_limits) + 
  facet_grid(Lake ~ trip, scales="free_x") + 
  ylab("Depth (m)") + ggtitle("Cladoceran density")
