#!/usr/bin/env R
# Visualize the dynamics of mobile traffic heatmap
# over a city.
library(fields)
library(animation)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GISTools)
library(movr)

####################################################
## Merge the traffic data and network topology
####################################################

# Read cellular network data
bsnet <- readRDS('rdata/bsnet.rds')

# Load map data
map <- readShapeSpatial('../maps/hangzhou/roads.shp')
pdf('figures/hz_map.pdf', width=8, height=6)
plot(map, lwd=0.1, col='grey')
dev.off()

## Read heatmap data
heatmap <- read.csv('../data/heatmap.hz.fd8.txt', head=F, sep='\t')
colnames(heatmap) <- c("bs", "thour", "packets", "bytes", "users")
heatmap[is.na(heatmap)] <- 0
heatmap <- heatmap %>%
  mutate(
    packets = packets / 1024, #KP
    bytes = bytes / 1024,     #KB
    tod = (thour + 8) %% 24,  #GMT+8
    tow = hour2tow(thour)
    ) %>%  # Merged with network data
  left_join(bsnet, by=c('bs')) %>%
  arrange(thour, users) # Ordered by specific fields

# Filter out data of area
# Metropolitan region:
area <- c(120.069897,30.180852,120.236048,30.348045)
# Urban region:
#area <- c(120.167722,30.255391,120.197546,30.281592)
# Rural region:
#area <- c(120.16678,30.300872,120.200125,30.336536)
heatmap.area <- heatmap %>% dplyr::filter(in_area(lon, lat, area))

# Averaged hourly statistics over a week
hm.data <- heatmap.area %>% group_by(bs, tod) %>%
  dplyr::summarise(
    x = median(ccx),
    y = median(ccy),
    z = mean(users))
hm.hourly.raw <- hm.data %>% mutate(z = standardize_st(bs, tod, z)) %>% as.data.frame
hm.hourly <- hm.hourly.raw %>% dplyr::filter(!is.na(z))

#####################################################
## Plot animation of hourly traffic heatmap with plot.heatmap.*style*
#####################################################

for (hour in seq(0,23)){
  png(paste('figures/hz_heatmap_h', hour, '.png', sep=''), width=600, height=420)
  par(mar = c(3,3,3,2), mgp=c(2.5,1,0), cex.lab=1.5, cex.axis=1.5)
  hm.hour <- hm.hourly %>% dplyr::filter(tod == hour)
  heatmap.plot(hm.hour$x, hm.hour$y, hm.hour$z,
               nx=100, ny=100, level=30,
               plot.title=title(main="Population dist. in Hangzhou, China"))
  dev.off()
}

saveGIF({
  for (hour in seq(0,23)){
    hm.hour <- hm.hourly %>% dplyr::filter(tod == hour)
    heatmap.plot(hm.hour$x, hm.hour$y, hm.hour$z,
                 nx=200, ny=200, level=30,
                 plot.title=title(main=sprintf("Time: %02d:00", hour)))
  }},
  movie.name = "hz_heatmap_animation.gif",
  interval=1,
  ani.width=800,
  ani.height=600)

