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
  })


ggplot(ctd, aes(x=t090C, y=prSM, linetype=Lake)) +
  geom_path() + 
  scale_y_reverse("Depth (m)", limits=c(50, 0)) +
  xlab(expression(Temperature~("°C"))) +
  facet_grid(. ~ trip) + theme_minimal()


ggplot(ctd, aes(x=bat, y=prSM, linetype=Lake)) +
  geom_path() + 
  scale_y_reverse("Depth (m)", limits=c(50, 0)) +
  xlab(expression(Attenuation~(m^-1))) +
  facet_grid(. ~ trip) + theme_minimal()
