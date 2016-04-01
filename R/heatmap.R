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
library(omniR)

# Metropolitan region:
area <- c(120.069897,30.180852,120.236048,30.348045)
# Urban region:
#area <- c(120.167722,30.255391,120.197546,30.281592)
# Rural region:
#area <- c(120.16678,30.300872,120.200125,30.336536)

####################################################
## Merge the traffic data and network topology
####################################################

# Read cellular network data
bsnet <- readRDS('rdata/bsnet.rds')

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
  arrange(thour, users) # Ordered with specific fields
saveRDS(heatmap, 'rdata/heatmap.hz.fd8.rds')

heatmap.area <- heatmap %>% filter(in.area(lon, lat, area))

####################################################
## Averaged hourly statistics over a week
####################################################

hm.hourly <- heatmap.area %>%
  group_by(bs, tod) %>%
  dplyr::summarise(
    x = median(ccx),
    y = median(ccy),
    z = mean(packets)) %>% ##<< Change to other values
  mutate(z = omniR::stscale(bs, tod, z, 0.5)) %>%
  filter(!is.na(z))
  
####################################################
## Plot heatmap of select area elegently
####################################################

# Plot heatmap with tiles; value in each tile is summarized acorss all
# observations in it.
plot.heatmap.tile <- function(data, hour){
  hm <- subset(data, tod == as.numeric(hour))
  # quilt.plot(hm$x, hm$y, hm$z, nx=100, ny=120)
  # Similar implementation of quilt.plot()
  val2D <- vbingrid(hm$x, hm$y, hm$z, nx=100, ny=100, na=0)
  x <- as.numeric(rownames(val2D))
  y <- as.numeric(colnames(val2D))
  image(x, y, val2D, col=topo.colors(10))
  # contour(x, y, val2D, add=TRUE, col=rgb(1,1,1,.7))
  title(sprintf("Time: %02d:00", hour), col="red")
}

# Another implementation of tile style using ggplot2
plot.heatmap.gg <- function(data, hour){
  hm <- subset(data, tod==as.numeric(hour))
  ggplot(hm) + theme_bw() + 
    stat_summary2d(aes(x=x, y=y, z=z), fun=mean, bins=100) + 
    labs(title=sprintf("Time: %02d:00", hour)) +
    scale_fill_gradientn(
      colours=c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
      limits=c(0,1))
}

# Plot heatmap with contours; a smoothing density is present
plot.heatmap.contour <- function(data, hour, nx=50, ny=50){
  hm <- subset(data, tod == as.numeric(hour))
  val2D <- vbingrid(hm$x, hm$y, hm$z, na=0, nx=nx, ny=ny)
  x <- as.numeric(rownames(val2D))
  y <- as.numeric(colnames(val2D))
  filled.contour(x-min(x), y-min(y),
                 val2D,
                 zlim = c(0,1), nlevels=10,
                 xlab="East-West Dist. (km)",
                 ylab="North-South Dist. (km)",
                 col=rev(brewer.pal(10, "Spectral")))
  north.arrow(2, 2, 0.3)
  title(sprintf("Time: %02d:00", hour), col="red")
}

#####################################################
## Plot animation of hourly traffic heatmap with plot.heatmap.*style*
#####################################################

saveGIF({
  for (hour in seq(0,23)){
    par(mar = c(4,4,2,2), mgp=c(2.5,1,0), cex.lab=1.5, cex.axis=1.5)
    plot.heatmap.contour(hm.hourly, hour, 100, 100)
  }}, movie.name = "heatmap_animation.gif", interval=2, ani.width=800,
  ani.height=600)


#####################################################
## Plot hourly traffic heatmap of three specific hours
#####################################################

hours <- c(7, 14, 21)
for (hour in hours){
  fname <- sprintf("figures/heatmap%02d.eps", hour)
  postscript(file=fname, width=8, height=5)
  par(mar = c(4,5,2,2), mgp=c(3,1,0), cex.lab=1.5, cex.axis=1.2, cex.main=1.5)
  plot.heatmap.contour(hm.hourly, hour)
  dev.off()
}

