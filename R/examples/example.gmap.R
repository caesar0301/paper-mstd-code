library(ggmap)
library(RgoogleMaps)

CenterOfMap <- geocode("30.273501, 120.176522")
Baltimore <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 12,
                     maptype = "terrain", source = "osm")
BaltimoreMap <- ggmap(Baltimore)
BaltimoreMap 