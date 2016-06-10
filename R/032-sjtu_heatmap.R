library(movr)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(animation)
library(akima)
library(fields)
library(classInt)
library(plyr)

SJTU_AREA <- c(121.425545,31.018011, 121.468736,31.050811)

# Read shapefile
ms <- readShapeSpatial('../maps/sjtu/roads_clean.shp')
pdf('figures/sjtu_map.pdf', width=8, height=6)
plot(ms, col="grey", lwd=0.5)
dev.off()

# Read wifi topology
geo <- readRDS('rdata/wifinet.rds')

# Read heatmap
hm <- readRDS('rdata/heatmap.sjtu.1411.i1h.rds')

# Fill value for every interval-loc pair
hm.wide <- hm %>% spread(key=loc, value=users) %>% gather(key=loc, value=users, -interval)
hm.wide$users[is.na(hm.wide$users)] <- 0

# Add geo info to heatmap
hm.geo <- hm.wide %>% inner_join(geo, by=c('loc'='bld_cn'))

# Plot heatmap
xlim <- c(min(hm.geo$x) - 10, max(hm.geo$x) + 10)
ylim <- c(min(hm.geo$y) - 10, max(hm.geo$y) + 10)
for(hour in seq(0, 23)){
  pdf(paste('figures/sjtu_heatmap_h', hour, '.pdf', sep=''), width=8, height=6)
  hm.hour <- hm.geo %>% dplyr::filter(interval==min(hm.geo$interval) + hour)
  heatmap.plot(hm.hour$x, hm.hour$y, log10(hm.hour$users+1),
              nx=40, ny=40, level=20,
              xlim=xlim, ylim=ylim,
              colors=c('white', 'yellow', 'red'),
              axes=TRUE,
              plot.title=title(
               main=paste("Population dist. in Campus (", hour, ")", sep='')))
  dev.off()
}

