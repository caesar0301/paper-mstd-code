library(movr)
library(ggplot2)
library(scales)
library(parallel)
library(scales)
library(squash)

filter <- dplyr::filter

# Read heatmap
geo <- readRDS('rdata/wifinet.rds')
hm <- readRDS('rdata/heatmap.sjtu.1411.i1h.rds')
hm.geo <- hm %>% inner_join(geo, by=c('loc'='bld_cn'))

# Normalize over space and time
hm <- hm.geo %>%
  # add date field
  mutate(date = hour2date(interval),
         tod = as.numeric(hour2tod(interval))) %>%
  filter(date >= "2014-11-01" & date <= "2014-11-07") %>%
  group_by(loc, tod) %>%
  dplyr::summarise(
    x = median(ccx),
    y = median(ccy),
    z = mean(users)) %>%
  # Scale over space and time
  mutate(z = standardize_st(loc, tod, z)) %>%
  filter( !is.na(z) ) %>%
  as.data.frame %>%
  mutate(tod = as.numeric(tod))

# Calculate spatial correlation
hours <- c(7, 10, 14, 21)
spcor <- do.call(rbind, lapply(hours, function(hour){
  hm.hour <- hm %>% filter(tod == hour)
  spatial.corr(hm.hour$x, hm.hour$y, hm.hour$z, beta=1) %>%
    mutate(hour = hour)
}))
saveRDS(spcor, 'rdata/sjtu.spcorr.rds')

################################################
#  Visualize empirical spatial correlation
################################################

spcor <- readRDS('rdata/sjtu.spcorr.rds')
cols <- c("orangered", "royalblue")

### AM
pdf("figures/sjtu_spatial_correlation_pl_am.pdf", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5, lwd=1.2)
# Peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==10)
magplot(shour$r, shour$corr, log='xy', col="#fc8d59", pch=21,
        xlab=expression(paste("Spatial lag ", h, " (m)")),
        ylab=expression(rho(h,0)), side=1:4, labels=c(T,T,F,F))
pl <- fit_power_law(shour$r, shour$corr, xmax=500, col=cols[1], lwd=4, lty=5)
# Off-peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==7)
points(shour$r, shour$corr, log='xy', col="#67a9cf", pch=22)
pl <- fit_power_law(shour$r, shour$corr, xmax=500, col=cols[2], lwd=4, lty=5)
legend(70, 0.02, c("Time 7:00", "Time 10:00 (peak)"), col=rev(cols), pch=c(22, 21), cex=1.5)
dev.off()

### PM
pdf("figures/sjtu_spatial_correlation_pl_pm.pdf", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5)
# Peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==14)
magplot(shour$r, shour$corr, log='xy', col="#fc8d59", pch=21,
        xlab=expression(paste("Spatial lag ", h, " (m)")),
        ylab=expression(rho(h,0)), side=1:4, labels=c(T,T,F,F))
pl <- fit_power_law(shour$r, shour$corr, xmax=500, col=cols[1], lwd=4, lty=5)
# Off-peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==21)
points(shour$r, shour$corr, log='xy', col="#67a9cf", pch=22)
pl <- fit_power_law(shour$r, shour$corr, xmax=600, col=cols[2], lwd=4, lty=5)
legend(70, 0.02, c("Time 14:00 (peak)", "Time 21:00"), col=rev(cols), pch=c(22, 21), cex=1.5)
dev.off()
