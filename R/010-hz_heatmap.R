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

# Read cellular network data
bsnet <- readRDS('rdata/bsnet.rds')

# Load map data
map <- readShapeSpatial('../maps/hangzhou/roads.shp')
pdf('figures/hz_map.pdf', width=8, height=6)
plot(map, lwd=0.1, col='grey')
dev.off()

## Read heatmap data
heatmap <- readRDS('rdata/heatmap.hz.fd8.rds')

# Filter out data of area
area.metro <- c(120.069897,30.180852,120.236048,30.348045)
area.urban <- c(120.167722,30.255391,120.197546,30.281592)
area.rural <- c(120.16678,30.300872,120.200125,30.336536)
area <- area.metro
heatmap.area <- heatmap %>% dplyr::filter(in_area(lon, lat, area))

# Averaged hourly statistics over a week
hm.data <- heatmap.area %>% group_by(bs, tod) %>%
  dplyr::summarise(
    x = median(ccx),
    y = median(ccy),
    z = mean(users))
hm.hourly.raw <- hm.data %>% mutate(z = standardize_st(bs, tod, z))
hm.hourly <- hm.hourly.raw %>% dplyr::filter(!is.na(z))

## Plot animation of hourly traffic heatmap
plot.hourly <- function(hour) {
  par(mar = c(5,5,3,2), mgp=c(3,1,0), cex.lab=1.5, cex.axis=1.5)
  hm.hour <- hm.hourly %>% dplyr::filter(tod == hour)
  key.levels <- heatmap.plot.levels(hm.hour$z, 30)
  main.title <- sprintf("Population dist. in Hangzhou (Time: %02d:00)", hour)
  heatmap.plot(hm.hour$x, hm.hour$y, hm.hour$z,
               nx=150, ny=150, nlevels=30,
               plot.title=title(main=main.title, xlab="EW (km)", ylab="SN (km)"))
}

for (hour in seq(0,23)){
  png(paste('figures/hz_heatmap_h', hour, '.png', sep=''), width=600, height=420)
  plot.hourly(hour)
  dev.off()
}

saveGIF({
  for (hour in seq(0,23)){
    plot.hourly(hour)
  }},
  movie.name = "hz_heatmap_animation.gif",
  interval=1,
  ani.width=800,
  ani.height=600)

