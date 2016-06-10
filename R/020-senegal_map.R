library(rgeos)
library(maptools)
library(qlcVisualize)
library(RColorBrewer)

# Read mobile network topology in senegal
mobnet <- read.csv('../data/senegal_site_lonlat.csv', head=T)
p <- data.frame(x=mobnet$lon, y=mobnet$lat)

# Read shapefile
ms <- readShapeSpatial('../maps/senegal/senegal_arr_2014_wgs.shp')

# Get aggregated border
ms.border <- gUnionCascaded(ms)

# Compute voronoi graph
del <- deldir(p)

# Convert voronoi t0 geo polygons
vp <- voronoi2polygons(p, ms.border)
proj4string(vp) <- proj4string(ms)

# Merge voronoi and geo polygons
final <- gIntersection(ms.border, vp, byid=TRUE)

### Plot voronoi without color
plot(final)
points(p, pch='.')

### Plot voronoi in a window
hc <- brewer.pal(10, 'Set3')
ms.border.owin <- as(ms.border, 'owin')
v.owin <- voronoi(p, ms.border.owin)
vmap(v.owin, col=hc, outer.border='white', border=NA)

