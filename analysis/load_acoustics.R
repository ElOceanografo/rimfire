library(reshape2)
library(dplyr)
library(lubridate)

CAL.120 <- -0.2
CAL.710 <- -3.0

# Volume backscatter
ev.export.dir <- "../acoustics/Exports/"
files.120 <- list.files(ev.export.dir, "120kHz", full.names = T)
files.710 <- list.files(ev.export.dir, "710kHz", full.names = T)

files <- data.frame(filename = c(files.120, files.710),
                    freq = c(rep(c('120', '710'), c(9, 10))))
files$Lake <- "Cherry"
files$Lake[grepl("eleanor", files$file)] <- "Eleanor"
files$Lake[grepl("tahoe", files$file)] <- "Tahoe"
files$Lake[grepl("independence", files$file)] <- "Independence"

echo <- plyr::ddply(files, "filename", function(x) read.csv(as.character(x$filename)))
echo <- right_join(files, echo)
echo$trip <- paste(substr(echo$Date_M, 1, 4), substr(echo$Date_M, 5, 6), sep="-")
echo$Sv_mean[echo$Sv_mean < -200 | echo$Sv_mean > 0] <- NA
echo$Lat_M[echo$Lat_M > 90] <- NA
echo$Lon_M[echo$Lon_M > 90] <- NA
echo$datetime <- ymd_hms(paste(echo$Date_M, echo$Time_M))

echo <- group_by(echo, trip, Lake) %>%
  mutate(Interval = plyr::mapvalues(Interval, unique(Interval), 1:length(unique(Interval))),
         Interval = Interval - min(Interval))
echo$Sv_mean[echo$freq == 120] <- echo$Sv_mean[echo$freq == 120] + CAL.120
echo$Sv_mean[echo$freq == 710] <- echo$Sv_mean[echo$freq == 710] + CAL.120

save(echo, file="acoustics.Rdata")

# Single targets
target.files <- data.frame(filename = list.files(ev.export.dir, "target", full.names = T))
target.files$Lake <- "Cherry"
target.files$Lake[grepl("eleanor", target.files$file)] <- "Eleanor"
target.files$Lake[grepl("tahoe", target.files$file)] <- "Tahoe"
target.files$Lake[grepl("independence", target.files$file)] <- "Independence"
target.files$freq <- 120
target.files$beam <- "Split"
target.files$freq[target.files$Lake == "Independence"] <- 710
target.files$beam[target.files$Lake == "Independence"] <- "Single"

targets <- plyr::ddply(target.files, "filename", function(x) read.csv(as.character(x$filename)))
targets <- right_join(target.files, targets) %>%
  select(filename, Lake, freq, Ping_date, Ping_time, Ping_milliseconds, Target_range,
         TS_comp, TS_uncomp, Angle_minor_axis, Angle_major_axis, Transmitted_pulse_length,
         Target_latitude, Target_longitude, Ping_number)

targets$Target_latitude[targets$Target_latitude < -180] <- NA
targets$Target_longitude[targets$Target_longitude < -180] <- NA
targets$datetime <- ymd_hms(paste(targets$Ping_date, targets$Ping_time))
targets$trip <- strftime(targets$datetime, "%Y-%m")
# apply calibration offsets 
targets$TS_comp[targets$freq == 120] <- targets$TS_comp[targets$freq == 120] + CAL.120
targets$TS_uncomp[targets$freq == 120] <- targets$TS_uncomp[targets$freq == 120] + CAL.120
targets$TS_comp[targets$freq == 710] <- targets$TS_comp[targets$freq == 710] + CAL.710
targets$TS_uncomp[targets$freq == 710] <- targets$TS_uncomp[targets$freq == 710] + CAL.710

save(targets, file="single_targets.Rdata")
