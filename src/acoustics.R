library(ggplot2)
library(reshape2)
# library(plyr)
library(dplyr)
library(lubridate)

ev.export.dir <- "../acoustics/Exports/"
files.120 <- list.files(ev.export.dir, "120kHz")
files.710 <- list.files(ev.export.dir, "710kHz")

echo120 <- plyr::ldply(paste(ev.export.dir, files.120, sep=""), read.csv)
echo120 <- rename(echo120, Sv_120 = Sv_mean, NASC_120 = NASC)
echo710 <- plyr::ldply(paste(ev.export.dir, files.710, sep=""), read.csv)
echo710 <- rename(echo710, Sv_710 = Sv_mean, NASC_710 = NASC)

echo <- plyr::join(echo120, select(echo710, Layer, Date_M, Time_M, Sv_710, NASC_710),
                   type="full", by=c("Layer", "Date_M", "Time_M"))
interval2 <- data.frame(Interval = unique(echo$Interval))
interval2$Interval2 <- 1:nrow(interval2)
echo <- join(echo, interval2, by="Interval")

echo <- melt(echo, measure.vars = c("Sv_120", "Sv_710", "NASC_120", "NASC_710"))
echo[c("variable", "Frequency")] <- colsplit(echo$variable, "_", c("variable", "Frequency"))
echo <- dcast(echo, ... ~ variable)
echo$Sv[echo$Sv < -100] <- NA

# Echogram plots

color_scale <- scale_fill_gradientn(
  colours=rev(c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0",
                "#225ea8", "#253494", "#081d58")), 
  limits=c(-90, -75),
  na.value="#00007F",
  name=expression(S[v]~(dB~re.~1~m^-1))
)

echo$datetime = ymd_hm(paste(echo$Date_M, substr(echo$Time_M, 1, 6)))
echo$Depth_mean = round(echo$Depth_mean, 1)

ggplot(filter(echo120, Date_M=="20131029"), aes(x=Interval, y=-Layer, fill=Sv_120)) + 
  geom_tile() + color_scale


dB.mean <- function(x, na.rm=T) 10 * log10(mean(10^(x/10), na.rm=na.rm))

profiles <- dcast(echo, Date_M + Frequency + Depth_mean ~ ., dB.mean, na.rm=T,
                  value.var="Sv")

profiles <- plyr::rename(profiles, c("." = "Sv"))
profiles$Date_M <- as.factor(profiles$Date_M)
head(profiles)

ggplot(profiles, aes(y=Sv, x=-Depth_mean, color=Date_M)) + geom_path() + 
  facet_wrap(~Frequency) + xlim(-50, 0) + 
  coord_flip()
