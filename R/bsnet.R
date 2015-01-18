# Calculate basic properties of HZ cellular network.
# Static features are present.
# By Chenxm
# chenxm35@gmail.com
library(plyr)
library(deldir)

# Read data
bsnet <- read.csv2('../data/bsnet_clean.txt', sep='\t', stringsAsFactors=F)
colnames(bsnet) <- c('city', 'bs', 'lon', 'lat')
bsnet$lon <- as.numeric(bsnet$lon)
bsnet$lat <- as.numeric(bsnet$lat)

# Remove invalid data points
bsnet <- subset(bsnet, lon>=117 & lat>=27 & lon<=122 & lat<=32)

# Add cardisian coordinates of LON/LAT pair, and a plain is assumed to
# replace the sphere surface in a city comparing with the earth radius.
lon.min <- deg2rad(min(bsnet$lon))
lat.min <- deg2rad(min(bsnet$lat))
bsnet <- ddply(bsnet, c("bs"), function(x) {
  x$ccx <- gcd.hf(lon.min, lat.min, deg2rad(x$lon), lat.min)
  x$ccy <- gcd.hf(lon.min, lat.min, lon.min, deg2rad(x$lat))
  return(x)})
saveRDS(bsnet, 'rdata/bsnet.rds')

# Select city area on Baidu Map
# HZ: (120,30) and (120.5,30.5)
# WZ: (120.50,27.83) and (120.88,28.10)
library(omniR)
# Plot voronoi diagram around Leifeng Tower
p1 <- c(120.153405,30.238258)
p2 <- c(120.157312,30.235738)
bsnet.hz <- bsnet[apply(bsnet, 1, function(x)
  in.area(x[3], x[4], c(p1, p2))),]
bsnet.hz <- subset(bsnet.hz, city==0)
rownames(bsnet.hz) <- NULL
# Plot diagram
tv <- deldir(bsnet.hz$ccx - min(bsnet.hz$ccx),
             bsnet.hz$ccy - min(bsnet.hz$ccy))
tv.tel <- tv$summary
plot(tv)