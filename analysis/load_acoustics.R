library(reshape2)
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
echo$Lat_M[echo$Lat_M > 90] <- NA
echo$Lon_M[echo$Lon_M > 90] <- NA
echo$datetime <- ymd_hms(paste(echo$Date_M, echo$Time_M))

echo <- group_by(echo, trip, lake) %>%
  mutate(Interval = plyr::mapvalues(Interval, unique(Interval), 1:length(unique(Interval))),
         Interval = Interval - min(Interval))

save(echo, file="acoustics.Rdata")
