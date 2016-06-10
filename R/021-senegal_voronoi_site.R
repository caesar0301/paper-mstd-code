library(movr)
library(rgeos)
library(maptools)
library(qlcVisualize)
library(RColorBrewer)
library(animation)
library(classInt)

# Read map data
ms <- readShapeSpatial('../maps/senegal/senegal_arr_2014_wgs.shp')
ms.border <- gUnionCascaded(ms)
ms.border.owin <- as(ms.border, 'owin')

# Read mobile network topology
mobnet <- read.csv('../data/senegal_site_lonlat.csv', head=T)

# Read people data
hm <- readRDS('rdata/heatmap.senegal.p06.i1h.rds')

# Assign color to values
nclr <- 9
pal <- rev(brewer.pal(nclr, 'RdYlBu'))
cls <- classIntervals(hm$users, nclr, style='quantile')
hm$col <- findColours(cls, pal)

# Convert to wide form and add geo info.
hm.wide <- hm %>% dplyr::select(-users) %>% spread(key=interval, value=col, fill=pal[1])
hm.geo <- hm.wide %>% left_join(mobnet, by=c('site'='site_id'))

# Get windowed voronoi graph
vor <- voronoi(points=hm.geo[, c('lon', 'lat')], window=ms.border.owin)
 
for(i in seq(0, 23)){
  pdf(paste('figures/senegal_space_h', i, '.pdf', sep=''), width=10, height=8)
  ccol <- hm.geo[, paste(378768 + i, sep='')]
  vmap(vor, col=ccol, lwd=0.1)
  title("Population dist. in Senegal, Africa")
  legend(x=-12.5, y=16.5, legend=leglabs(round(cls$brks)),  fill=pal, bty='n')
  dev.off()
}
