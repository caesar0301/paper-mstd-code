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

# Assign color to values
nclr <- 9
pal <- rev(brewer.pal(nclr, 'RdYlBu'))
cls <- classIntervals(hm.geo$users, nclr, style='quantile')
hm.geo$col <- findColours(cls, pal)

# Plot user distribution over space
for(hour in seq(0, 23)){
  pdf(paste('figures/sjtu_space/sjtu_space_h', hour, '.pdf', sep=''), width=10, height=8)
  hm.hour <- hm.geo %>% dplyr::filter(interval == min(hm.geo$interval) + hour)
  vor <- deldir(hm.hour$lon, hm.hour$lat)
  tiles <- tile.list(vor)
  par(xaxt='n', yaxt='n')
  plot(tiles, fillcol=hm.hour$col, showpoints=FALSE, border='white',
       axes=FALSE,  xlab="", ylab="")
  dev.off()
}
