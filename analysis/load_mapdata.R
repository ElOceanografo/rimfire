library(sp)
library(rgdal)



water.1 <- rgdal::readOGR("NHD_H_1804_Shape/Shape", "NHDWaterbody")
water.1 <- spTransform(water, CRS("+proj=longlat"))

sort(unique(water.1@data$GNIS_NAME))
lake.ids.1 <- which(water.1[["GNIS_NAME"]] %in% c("Cherry Lake", "Lake Eleanor"))

lakes.1 <- plyr::ldply(lake.ids.1, function(i) {
  df <- as.data.frame(water.1@polygons[[i]]@Polygons[[1]]@coords)
  df$Lake <- water.1[["GNIS_NAME"]][i]
  names(df) <- c("Longitude", "Latitude", "Lake")
  return(df)
}) 


water.2 <- rgdal::readOGR("NHD_H_1605_Shape/Shape", "NHDWaterbody")
water.2 <- spTransform(water.2, CRS("+proj=longlat"))

sort(unique(water.2@data$GNIS_NAME))
lake.ids.2 <- which(water.2[["GNIS_NAME"]] %in% c("Independence Lake", "Lake Tahoe"))

lakes.2 <- plyr::ldply(lake.ids.2, function(i) {
  df <- as.data.frame(water.2@polygons[[i]]@Polygons[[1]]@coords)
  df$Lake <- water.2[["GNIS_NAME"]][i]
  names(df) <- c("Longitude", "Latitude", "Lake")
  return(df)
}) 

lakes <- rbind(lakes.2, lakes.1)
lakes$Lake <- trimws(gsub("Lake", "", lakes$Lake))

lakes <- droplevels(lakes)
# library(ggplot2)
# ggplot(lakes, aes(Longitude, Latitude, group=Lake)) + geom_polygon()

save(lakes, file="lake_outlines.Rdata")
