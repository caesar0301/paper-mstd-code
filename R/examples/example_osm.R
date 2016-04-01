library(OpenStreetMap)

nm = c("osm", "maptoolkit-topo",
       "waze", "mapquest", "mapquest-aerial",
       "bing", "stamen-toner", "stamen-terrain",
       "stamen-watercolor", "osm-german", "osm-wanderreitkarte",
       "mapbox", "esri", "esri-topo",
       "nps", "apple-iphoto", "skobbler",
       "opencyclemap", "osm-transport",
       "osm-public-transport", "osm-bbike", "osm-bbike-german")

png(width=1200,height=2200)
par(mfrow=c(6,4))
for(i in 1:length(nm)){
  print(nm[i])
  map = openmap(c(lat= 38.05025395161289,   lon= -123.03314208984375),
                c(lat= 36.36822190085111,   lon= -120.69580078125),
                minNumTiles=9,type=nm[i])
  plot(map)
  title(nm[i],cex.main=4)
}
dev.off()
