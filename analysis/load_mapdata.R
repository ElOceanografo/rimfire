library(sp)
library(rgdal)
water <- rgdal::readOGR("Shape", "NHDWaterbody")
water <- spTransform(water, CRS("+proj=longlat"))

sort(unique(water@data$GNIS_NAME))
lake.ids <- which(water[["GNIS_NAME"]] %in% c("Cherry Lake", "Lake Eleanor"))

lakes <- plyr::ldply(lake.ids, function(i) {
  df <- as.data.frame(water@polygons[[i]]@Polygons[[1]]@coords)
  df$Lake <- water[["GNIS_NAME"]][i]
  names(df) <- c("Longitude", "Latitude", "Lake")
  return(df)
}) 

# ggplot(lakes, aes(Longitude, Latitude)) + geom_polygon()

save(lakes, file="lake_outlines.Rdata")
