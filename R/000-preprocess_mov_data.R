library(movr)
library(plyr)

###### Hangzhou Data ########

# Read mobile network data
bsnet <- read.csv2('../data/bsnet_clean.txt', sep='\t', stringsAsFactors=F)
colnames(bsnet) <- c('city', 'bs', 'lon', 'lat')
bsnet$lon <- as.numeric(bsnet$lon)
bsnet$lat <- as.numeric(bsnet$lat)
# Remove invalid data points
bsnet <- subset(bsnet, lon>=117 & lat>=27 & lon<=122 & lat<=32)

# City areas (unused here)
area.hz <- c(120,30,120.5,30.5)
area.wz <- c(120.50,27.83,120.88,28.10)

# Add cardisian coordinates of LON/LAT pair, and a plain is assumed to
# replace the sphere surface in a city comparing with the earth radius.
lon.min <- min(bsnet$lon)
lat.min <- min(bsnet$lat)
bsnet <- ddply(bsnet, c("bs"), function(x) {
  x$ccx <- gcd(c(lat.min, lon.min), c(lat.min, x$lon))
  x$ccy <- gcd(c(lat.min, lon.min), c(x$lat, lon.min))
  return(x)
})
saveRDS(bsnet, 'rdata/bsnet.rds')

# Read heatmap data for all
heatmap <- fread('../data/heatmap.hz.txt', head=F, sep='\t')
colnames(heatmap) <- c("bs", "thour", "packets", "bytes", "users")
heatmap[is.na(heatmap)] <- 0
saveRDS(heatmap, 'rdata/heatmap.hz.rds')

# Read heatmap for 8 days
heatmap8d <- read.csv('../data/heatmap.hz.fd8.txt', head=F, sep='\t')
colnames(heatmap8d) <- c("bs", "thour", "packets", "bytes", "users")
heatmap8d[is.na(heatmap8d)] <- 0
heatmap8d <- heatmap8d %>%
  mutate(
    packets = packets / 1024, #KP
    bytes = bytes / 1024,     #KB
    tod = (thour + 8) %% 24,  #GMT+8
    tow = hour2tow(thour)
  ) %>%  # Merged with network data
  left_join(bsnet, by=c('bs')) %>%
  arrange(thour, users) # Ordered by specific fields
saveRDS(heatmap8d, 'rdata/heatmap.hz.fd8.rds')

###### Senegal Data ########

# Network topology
bsnet <- read.csv('../data/senegal_site_lonlat.csv', head=T)
lon.min <- min(bsnet$lon)
lat.min <- min(bsnet$lat)
bsnet <- ddply(bsnet, c("site_id"), function(x) {
  x$ccx <- gcd(c(lat.min, lon.min), c(lat.min, x$lon))
  x$ccy <- gcd(c(lat.min, lon.min), c(x$lat, lon.min))
  return(x)
})
saveRDS(bsnet, 'rdata/senegal.bsnet.rds')

# Heatmap
heatmap <- read.csv('../data/scl_fine_p06_hm_i1h', sep=',', head=F)
colnames(heatmap) <- c('interval', 'site', 'users')
saveRDS(heatmap, 'rdata/heatmap.senegal.p06.i1h.rds')

###### SJTU Data ########

SJTU_AREA <- c(121.425545,31.018011, 121.468736,31.050811)

# Wifi networks
geo <- read.csv('../data/apnames-utf8.csv', head=F, sep=',', stringsAsFactors = F)
colnames(geo) <- c('bld', 'bld_cn', 'type', 'dep', 'lon', 'lat' )
geo <- geo %>%
  dplyr::filter(lon != 'null' & lat != 'null') %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat)) %>%
  dplyr::filter(in_area(lon, lat, SJTU_AREA))
lon.min <- min(geo$lon)
lat.min <- min(geo$lat)
geo <- ddply(geo, c("bld"), function(x) {
  x$ccx <- gcd(c(lat.min, lon.min), c(lat.min, x$lon)) * 1000 # Meter
  x$ccy <- gcd(c(lat.min, lon.min), c(x$lat, lon.min)) * 1000
  return(x)
})
saveRDS(geo, 'rdata/wifinet.rds')

# Heatmap
hm <- read.csv('../data/wifi_1411_hm_i1h', sep=',', head=F, stringsAsFactors = F)
colnames(hm) <- c('interval', 'loc', 'users')
saveRDS(hm, 'rdata/heatmap.sjtu.1411.i1h.rds')
