# Explore the spatial correlation of cellular traffic
# We focus on the hourly statistics and the whole week data are
# compressed into one day at average level.
library(movr)
library(ggplot2)
library(scales)
library(parallel)

filter <- dplyr::filter

#####################################
## Data flow
#####################################

# Read data set of heatmap
heatmap <- readRDS("rdata/heatmap.hz.fd8.rds")

# Metropolitan region
area <- c(120.069897,30.180852,120.236048,30.348045)  
heatmap.m <- filter(heatmap, in_area(lon, lat, area))

# Normalize over space and time
hm <- heatmap.m %>%
  # add date field
  mutate(date = hour2date(thour)) %>%
  filter(date >= "2012-08-20" & date <= "2012-08-26") %>%
  # aggregate hourly traffic stat over a week
  group_by(bs, tod) %>%
  dplyr::summarise(
    x = median(ccx),
    y = median(ccy),
    z = mean(bytes)) %>%
  # Scale over space and time
  mutate(z = standardize_st(bs, tod, z)) %>%
  filter( !is.na(z) ) %>%
  as.data.frame

################################################
#  Calculate empirical spatial correlation
################################################

# calculate correlation and merge all data into a single data frame
resolution <- 100
hours <- c(7, 10, 14, 21)
spcor <- do.call(rbind, mclapply(hours, function(hour){
  # Get bined data to improve perf.
  hm.bined <- hm %>% filter(tod == hour) %>%
    mutate(x.int = vbin(x, resolution), y.int = vbin(y, resolution)) %>%
    group_by(x.int, y.int) %>%
    dplyr::summarise(
      x = 0.5 * (min(x) + max(x)),
      y = 0.5 * (min(y) + max(y)),
      z = mean(z))
  # Spatial correlation
  spatial.corr(hm.bined$x, hm.bined$y, hm.bined$z) %>%
    mutate(resolution = resolution, hour = hour)
}))
saveRDS(spcor, sprintf('rdata/hz.spcorr.r%02d.rds'))


################################################
#  Visualize empirical spatial correlation
################################################

espcorr <- function(df, beta=0.05){
  # allow spatial distance tolerance of beta Km
  df %>% mutate(
    r.int = findInterval(r, seq(min(r), max(r), by = beta))) %>%
    group_by(r.int) %>%
    dplyr::summarise(
      r = mean(r),
      n = length(z.i),
      corr = cor(z.i, z.j, method="pearson")) %>%
    dplyr::filter(!is.na(corr))
}

# calculate correlation and merge all data into a single data frame
resolutions <- c(100)
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

cols <- c("orangered", "royalblue")
xlim <- c(0.1, 10)
ylim <- c(0.0001, 1)

### AM
pdf("figures/hz_spatial_correlation_pl_am.pdf", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5)
# peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==10)
magplot(shour$r, shour$corr, log='xy', col="#fc8d59", pch=21, xlim=xlim, ylim=ylim,
        xlab=expression(paste("Spatial lag ", h, " (km)")),
        ylab=expression(rho(h,0)), side=1:4, labels=c(T,T,F,F))
pl <- fit_power_law(shour$r, shour$corr, xmax=10, col=cols[1], lwd=4, lty=5)
text(1, 0.2, bquote(y ~ '~' ~ 0.039* x^-0.955), col=cols[1], cex=1.5)
# off-peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==7)
points(shour$r, shour$corr, log='xy', col="#67a9cf", pch=22)
pl <- fit_power_law(shour$r, shour$corr, xmax=10, col=cols[2], lwd=4, lty=5)
text(0.3, 0.02, bquote(y ~ '~' ~ 0.02* x^-0.943), col=cols[2], cex=1.5)
legend(0.15, 0.002, c("Time 7:00", "Time 10:00 (peak)"), col=rev(cols), pch=c(22, 21), cex=1.5)
dev.off()

### PM
pdf("figures/hz_spatial_correlation_pl_pm.pdf", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5)
# peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==14)
magplot(shour$r, shour$corr, log='xy', col="#fc8d59", pch=21, xlim=xlim, ylim=ylim,
        xlab=expression(paste("Spatial lag ", h, " (km)")),
        ylab=expression(rho(h,0)), side=1:4, labels=c(T,T,F,F))
pl <- fit_power_law(shour$r, shour$corr, xmax=10, col=cols[1], lwd=4, lty=5)
text(1, 0.2, bquote(y ~ '~' ~ 0.052* x^-0.869), col=cols[1], cex=1.5)
# off-peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==21)
points(shour$r, shour$corr, log='xy', col="#67a9cf", pch=22)
pl <- fit_power_law(shour$r, shour$corr, xmax=10, col=cols[2], lwd=4, lty=5)
text(0.3, 0.02, bquote(y ~ '~' ~ 0.035* x^-0.758), col=cols[2], cex=1.5)
legend(0.15, 0.002, c("Time 14:00 (peak)", "Time 21:00"), col=rev(cols), pch=c(22, 21), cex=1.5)
dev.off()
