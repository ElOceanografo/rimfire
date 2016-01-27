library(ggplot2)
library(viridis)
library(reshape2)
# library(plyr)
library(dplyr)
library(lubridate)

ev.export.dir <- "../acoustics/Exports/"
files.120 <- list.files(ev.export.dir, "120kHz")
files.710 <- list.files(ev.export.dir, "710kHz")

files <- data.frame(filename = paste(ev.export.dir, c(files.120, files.710), sep=""),
                    freq = c(rep(c('120', '710'), each=8)))
files$lake <- "Cherry"
files$lake[grepl("eleanor", files$file)] <- "Eleanor"

echo <- plyr::ddply(files, "filename", function(x) read.csv(as.character(x$filename)))
echo <- right_join(files, echo)
echo$trip <- paste(substr(echo$Date_M, 1, 4), substr(echo$Date_M, 5, 6), sep="-")
echo$Sv_mean[echo$Sv_mean < -200 | echo$Sv_mean > 0] <- NA
echo$datetime <- ymd_hms(paste(echo$Date_M, echo$Time_M))


# echogram
color_scale_limits = c(-120, -40)
depth_limits = c(50, 0)

for (lakename in c("Cherry", "Eleanor")) {
  p <- ggplot(filter(echo, lake==lakename),
         aes(x=Interval, y=Layer, fill=Sv_mean)) +
    geom_tile() + 
    scale_fill_viridis(limits=color_scale_limits) +
    scale_y_reverse(limits=depth_limits) +
    facet_grid(freq ~ Date_M, scales="free_x") + 
    ylab("Depth (m)") + ggtitle("Eleanor")
  print(p)
}


dB.mean <- function(x, na.rm=T) 10 * log10(mean(10^(x/10), na.rm=na.rm))

profiles <- dcast(echo, trip + lake + freq + Layer ~ ., dB.mean, na.rm=T,
                  value.var="Sv_mean")
profiles <- rename(profiles, Sv = .)

ggplot(profiles, aes(x=Layer, y=Sv, color=lake)) + 
  geom_point() + geom_smooth() +
  facet_grid(freq ~ trip) + 
  scale_x_reverse() + coord_flip() + #limits=c(50, 0)) + coord_flip() + 
  xlab("Depth (m)") + ylab("Mean Sv") + 
  ggtitle("Backscatter depth profiles") +
  theme_bw()

