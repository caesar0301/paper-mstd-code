library(movr)
library(rgeos)
library(maptools)
library(qlcVisualize)
library(RColorBrewer)
library(animation)
library(akima)
library(fields)
library(classInt)

# Read map data
ms <- readShapeSpatial('../maps/senegal/senegal_arr_2014_wgs.shp')
ms.border <- gUnionCascaded(ms)
ms.border.owin <- as(ms.border, 'owin')

pdf('figures/senegal_map.pdf', width=8, height=6)
plot(ms, col="grey", lwd=0.5)
dev.off()

# Read mobile network topology
mobnet <- read.csv('../data/senegal_site_lonlat.csv', head=T)

# Read people data
hm <- readRDS('rdata/heatmap.senegal.p06.i1h.rds')
hm.geo <- hm %>% left_join(mobnet, by=c('site'='site_id'))

# Plot heatmap
plot.hourly <- function(hour) {
  par(mar = c(4,4,2,2), mgp=c(2.5,1,0), cex.lab=1.5, cex.axis=1.5)
  hm.hour <- hm.geo %>% dplyr::filter(interval==378768 + hour)
  key.levels <- heatmap.plot.levels(log10(hm.hour$users), 30)
  ticks <- unique(round(key.levels))
  key.at <- seq(min(ticks), max(ticks), 1)
  key.labels <- sapply(key.at, function(at){ bquote(10^.(at)) })
  main.title <- paste("Population dist. in Senegal, Africa (", hour, ")", sep='')
  heatmap.plot(hm.hour$lon, hm.hour$lat, log10(hm.hour$users),
               nx=150, ny=150, levels=key.levels,
               key.axes=axis(4, at=key.at, labels=key.labels),
               plot.title=title(main=main.title, xlab="Longitude", ylab="Latitude"))
}

for(i in seq(0, 23)){
  pdf(paste('figures/senegal_heatmap_h', i, '.pdf', sep=''), width=8, height=5.5)
  plot.hourly(i)
  dev.off()
}

saveGIF({
  for (hour in seq(0, 23)){
    plot.hourly(hour)
  }},
  movie.name = "senegal_heatmap_animation.gif",
  interval=0.9,
  ani.width=800,
  ani.height=550
)

