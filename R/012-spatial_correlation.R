# Explore the spatial correlation of cellular traffic
# We focus on the hourly statistics and the whole week data are
# compressed into one day at average level.
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(omniR)

#####################################
## Data flow
#####################################

# Read data set of heatmap output by `heatmap.R`
# Entire region
heatmap <- readRDS("rdata/heatmap.hz.fd8.rds")

# Metropolitan region
area <- c(120.069897,30.180852,120.236048,30.348045)  
heatmap.m <- filter(heatmap, in.area(lon, lat, area))

# control the spatial effect over time
hm <- heatmap.m %>%
  # add date field
  mutate(
    date = hour2date(thour)) %>%
  filter(date >= "2012-08-20" & date <= "2012-08-26") %>%
  # aggregate hourly traffic stat over a week
  group_by(bs, tod) %>%
  dplyr::summarise(
    x = median(ccx),
    y = median(ccy),
    z = mean(bytes)) %>%
  # Scale over space and time
  mutate(z = omniR::stscale(bs, tod, z)) %>%
  filter( !is.na(z) ) %>%
  as.data.frame()

################################################
#  Calculate distance matrix
################################################

# calculate the pair-wise distances
# this process may be very slowly as for the matrix computing
resolutions <- c(25, 40, 60, 100)
hours <- c(7, 10, 14, 21)
for ( res in resolutions ) {
  for ( hour in hours ){
    hm %>% filter(tod == hour) %>%
      # bin the 2D spatial random fields into corase lattice
      mutate(
        x.int = vbin(x, res),
        y.int = vbin(y, res)) %>%
      group_by(x.int, y.int) %>%
      dplyr::summarise(
        x = 0.5 * (min(x) + max(x)),
        y = 0.5 * (min(y) + max(y)),
        z = mean(z)) %>%
      dist.pw() %>%
      saveRDS(sprintf("rdata/spatial.dist.r%02d.h%02d.rds", res, hour))
  }
}

################################################
#  Calculate empirical spatial correlation
################################################

# calculate spatial correlation of a 2D region
# df - output of dist.pw()
# beta - spatial distance tolerance to associate locations
espcorr <- function(df, beta=0.05){
  # allow spatial distance tolerance of beta Km
  df %>% mutate(
    r.int = findInterval(r, seq(min(r), max(r), by = beta))) %>%
    group_by(r.int) %>%
    dplyr::summarise(
      r = mean(r),
      n = length(z.i),
      corr = cor(z.i, z.j, method="pearson")) %>%
    filter(!is.na(corr))
}

# calculate correlation and merge all data into a single data frame
resolutions <- c(25, 40, 60, 100)
hours <- c(7, 10, 14, 21)
spcor <- do.call(rbind, lapply(resolutions, function(res){
  do.call(rbind, lapply(hours, function(hour){
    df <- readRDS(sprintf("rdata/spatial.dist.r%02d.h%02d.rds", res, hour))
    df %>% espcorr() %>%
      mutate(
        resolution = res,
        hour = hour)
  }))
}))
print(head(spcor))


################################################
#  Visualize empirical spatial correlation
################################################


postscript("figures/spatial_correlation_pl_am.eps", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5)

spcor2 <- filter(spcor, r >= 0.1 & r <= 10)
cols <- c("orangered", "royalblue")

# peak hour
shour <- filter(spcor2, resolution==100 & hour==10)
plot(log10(shour$r), log10(shour$corr), xlim=c(-1, 1), ylim=c(-4, 0),
     xaxt="n", yaxt="n", col="#fc8d59", pch=21, lwd=1.2,
     xlab=expression(paste("Spatial lag ", h, " (km)")),
     ylab=expression(rho(h,0)))
shour <- filter(shour, r >= 0.1 & r <= 4)
model1 <- lm(log10(shour$corr)~log10(shour$r))
abline(model1, col=cols[1], lwd=4, lty=5)

# off-peak hour
shour <- filter(spcor2, resolution==100 & hour==7)
points(log10(shour$r), log10(shour$corr), col="#67a9cf", pch=22, lwd=1.2)
shour <- filter(shour, corr >=0.001)
model2 <- lm(log10(shour$corr)~log10(shour$r))
abline(model2, col=cols[2], lwd=4, lty=5)

# change appearance
minor.ticks.axis(1, 9, mn=-1, mx=1)
minor.ticks.axis(2, 9, mn=-4, mx=0)
minor.ticks.axis(3, 9, mn=-1, mx=1, lab=FALSE)
minor.ticks.axis(4, 9, mn=-4, mx=0, lab=FALSE)
cof1=model1$coefficients[["log10(shour$r)"]]
cof2=model2$coefficients[["log10(shour$r)"]]
legend(-0.9, -3, c("Time 7:00", "Time 10:00 (peak)"), col=rev(cols),
       pch=c(22, 21), cex=1.7)

dev.off()

library(scales)
postscript("figures/spatial_correlation_pl_pm.eps", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5)
spcor2 <- filter(spcor, r >= 0.1 & r <= 10)

# peak hour
shour <- filter(spcor2, resolution==100 & hour==14)
plot(log10(shour$r), log10(shour$corr), xlim=c(-1, 1), ylim=c(-4, 0),
     xaxt="n", yaxt="n", col="#fc8d59", pch=21, lwd=1.2,
     xlab=expression(paste("Spatial lag ", h, " (km)")),
     ylab=expression(rho(h,0)))
shour <- filter(shour, r >= 0.1 & r <= 4)
model1 <- lm(log10(shour$corr)~log10(shour$r))
abline(model1, col=cols[1], lwd=4, lty=5)

# off-peak hour
shour <- filter(spcor2, resolution==100 & hour==21)
points(log10(shour$r), log10(shour$corr), col="#67a9cf", pch=22, lwd=1.2)
model2 <- lm(log10(shour$corr)~log10(shour$r))
abline(model2, col=cols[2], lwd=4, lty=5)

# change appearance
minor.ticks.axis(1, 9, mn=-1, mx=1)
minor.ticks.axis(2, 9, mn=-4, mx=0)
minor.ticks.axis(3, 9, mn=-1, mx=1, lab=FALSE)
minor.ticks.axis(4, 9, mn=-4, mx=0, lab=FALSE)
cof1=model1$coefficients[["log10(shour$r)"]]
cof2=model2$coefficients[["log10(shour$r)"]]
legend(-0.9, -3, c("Time 14:00 (peak)", "Time 21:00"), col=cols,
       pch=c(21, 22), cex=1.7)

dev.off()