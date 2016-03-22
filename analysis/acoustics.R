library(ggplot2)
library(viridis)
library(reshape2)
library(dplyr)
library(lubridate)

load("acoustics.Rdata")

# echograms
color_scale_limits = c(-120, -40)
depth_limits = c(50, 0)

for (lakename in c("Cherry", "Eleanor")) {
  p <- ggplot(filter(echo, lake==lakename),
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

profiles <- dcast(echo, trip + lake + freq + Layer ~ ., dB.mean, na.rm=T,
                  value.var="Sv_mean")
profiles <- rename(profiles, Sv = .)

p <- ggplot(profiles, aes(x=Layer, y=Sv, color=lake)) + 
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
  facet_grid(freq ~ lake) + 
  scale_x_reverse() + coord_flip() + #limits=c(50, 0)) 
  xlab("Depth (m)") + ylab("Mean Sv") + 
  ggtitle("Backscatter depth profiles") +
  theme_bw()
ggsave("graphics/acoustic_profiles_date.pdf", p, width=8, height=8, units="in")
