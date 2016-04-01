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
hm <- read.csv('../data/scl_fine_p06_hm_i1h', sep=',', head=F)
colnames(hm) <- c('interval', 'site', 'users')
hm.geo <- hm %>% left_join(mobnet, by=c('site'='site_id'))

# Plot heatmap
for(i in seq(0, 23)){
  pdf(paste('figures/senegal_heatmap_h', i, '.pdf', sep=''), width=8, height=6)
  hm.hour <- hm.geo %>% dplyr::filter(interval==378768 + i)
  heatmap.plot(hm.hour$lon, hm.hour$lat, log10(hm.hour$users),
               nx=200, ny=200, level=30,
               plot.title=title(main=paste("Population dist. in Senegal, Africa (", i, ")", sep='')))
  dev.off()
}

saveGIF({
  for (hour in seq(0, 23)){
    par(mar = c(4,4,2,2), mgp=c(2.5,1,0), cex.lab=1.5, cex.axis=1.5)
    hm.hour <- hm.geo %>% dplyr::filter(interval==378768 + hour)
    heatmap.plot(hm.hour$lon, hm.hour$lat, log10(hm.hour$users),
                 nx=200, ny=200, level=30, bias=1,
                 plot.title=title(main=paste("Population dist. in Senegal, Africa (", hour, ")", sep='')))
  }}, movie.name = "senegal_heatmap_animation.gif",
  interval=0.9,
  ani.width=850,
  ani.height=600)

