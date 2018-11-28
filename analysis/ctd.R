library(dplyr)
library(tidyr)
library(ggplot2)

ctd.files <- data.frame(
  filename = c("ctd/CerryLk October132_down.txt", "ctd/Cherry Lake June24_2014_down.txt",
    "ctd/CherryLake_Rimfire_down.txt", "ctd/Cherry Lake Sept14_down.txt", 
    "ctd/Lake Eleanor_Rimfire_down.txt", "ctd/Lake Eleanor Rim Fire June 2014_down.txt", 
    "ctd/Lake Eleanor Sept14_down.txt", "ctd/Lk Eleanor Oct2013_2_down.txt"),
  trip = c("2013-10", "2014-06", 
           "2014-04", "2014-09",
           "2014-04", "2014-06",
           "2014-09", "2013-10"),
  Lake = rep(c("Cherry", "Eleanor"), each=4),
  stringsAsFactors = F)

cols <- list("timeS", "prSM", "t090C", "c0uS", "bat", "xmiss", "wetStar",
          "seaTurbMtr", "sbeox0Mg", "par", "ph", "depFM", "accM",
          "density00", "dz", "sal00", "specc")


ctd <- plyr::ddply(ctd.files, c("trip", "Lake"), function(df) {
    read.csv(df$filename, header=F, skip=242, comment.char="#", col.names=cols)
  }) %>%
  select(trip, Lake, prSM, t090C)
names(ctd) <- c("trip", "Lake", "Depth", "Temperature")

ctd.tahoe <- read.csv("ctd/LTP1444_down.txt", skip=137, header=F) %>%
  mutate(trip="2013-10", Lake="Tahoe") %>%
  select(trip, Lake, V2, V3)
names(ctd.tahoe) <- c("trip", "Lake", "Depth", "Temperature")


ctd.indy <- read.csv("ctd/Independenc_EXO_SD_13C100952_102013_212309.csv") %>%
  mutate(trip = "2013-10", Lake="Independence") %>%
  select(trip, Lake, Depth.m, Temp..C)
names(ctd.indy) <- c("trip", "Lake", "Depth", "Temperature")

ctd <- rbind(ctd, ctd.indy, ctd.tahoe) %>%
  mutate(Depth = round(Depth)) %>%
  group_by(trip, Lake, Depth) %>%
  summarize(Temperature = mean(Temperature, na.rm=T)) %>%
  ungroup()

# 2-m bins
binwidth <- 3
ctd.binned <- ctd %>%
  mutate(Depth = binwidth * round(Depth/binwidth)) %>%
  group_by(trip, Lake, Depth) %>%
  summarize(Temperature = mean(Temperature, na.rm=T)) %>%
  ungroup()

p <- ggplot() +
  geom_path(aes(x=Temperature, y=Depth, linetype=Lake), data=ctd) +
  # geom_point(aes(x=Temperature, y=Depth, shape=Lake), data=ctd.binned) +
  scale_y_reverse("Depth (m)", limits=c(50, 0)) +
  scale_x_continuous(expression(Temperature~("°C"))) +
  facet_grid(. ~ trip) + theme_minimal()
p
ggsave("graphics/ctd.png", p)


