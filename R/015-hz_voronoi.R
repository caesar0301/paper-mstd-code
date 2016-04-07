library(movr)
library(maptools)
library(rgeos)
library(raster)
library(qlcVisualize)
library(RColorBrewer)
library(classInt)

#########################
# Process map
#########################
library(gplots)

area.metro <- c(120.069897,30.180852,120.236048,30.348045)
area.urban <- c(120.167722,30.255391,120.197546,30.281592)
area.rural <- c(120.16678,30.300872,120.200125,30.336536)
area <- area.metro

map <- readShapeSpatial('../maps/hangzhou/roads.shp')

# Read cellular network data
bsnet <- readRDS('rdata/bsnet.rds')
bsnet$lon <- bsnet$lon - 0.01
bsnet$lat <- bsnet$lat - 0.003
bsnet <- bsnet %>% dplyr::filter(in_area(lon, lat, area))

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

b <- bbox(map)
b[1, ] <- area[c(1,3)]
b[2, ] <- area[c(2,4)]
b <- bbox(t(b))
clipped <- gClip(map, b)

# Default call
png('figures/hz_space_site_dist.png', width=800, height=800)
hist2d(bsnet$lon, bsnet$lat, nbins=600)
plot(clipped, lwd=0.3, add=TRUE, col='grey')
dev.off()

#########################
# Visualize site voronoi
#########################

# Read population data
hm <- read.csv('../data/heatmap.hz.fd8.txt', head=F, sep='\t')
colnames(hm) <- c("bs", "interval", "packets", "bytes", "users")
hm.wide <- hm %>% dplyr::select(bs, interval, users) %>%
  spread(key=interval, value=users)
hm.wide[is.na(hm.wide)] <- 0
hm <- hm.wide %>% gather(key=interval, value=users, -bs) %>%
  mutate(interval = as.numeric(interval)) 

# Merge population and network data
hm.geo <- hm %>% inner_join(bsnet, by=c('bs'='bs'))

# Manipulate color
nclr <- 9
pal <- colorRampPalette(rev(brewer.pal(8, "Spectral")), bias=1)(nclr)
cls <- classIntervals(hm.geo$users, nclr, style='quantile')
hm.geo$col <- findColours(cls, pal)

# Plot voronoi graph
hm.hour <- hm.geo %>% dplyr::filter(interval == min(hm.geo$interval) + 14)
vor <- deldir(hm.hour$lon, hm.hour$lat)
tiles <- tile.list(vor)
par(xaxt='n', yaxt='n')
plot(tiles, fillcol=hm.hour$col, showpoints=FALSE, border=NA,
     axes=FALSE,  xlab="", ylab="")
