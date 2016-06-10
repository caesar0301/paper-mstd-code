# Explore the spatial correlation of cellular traffic
library(movr)
library(ggplot2)
library(scales)
library(parallel)
library(scales)
library(magicaxis)

filter <- dplyr::filter

#####################################
## Data flow
#####################################

# Read data set of heatmap
heatmap <- readRDS("rdata/heatmap.senegal.p06.i1h.rds")

# Read network topology
mobnet <- readRDS('rdata/senegal.bsnet.rds')
hm.geo <- heatmap %>% inner_join(mobnet, by=c('site'='site_id'))

# Normalize over space and time
hm <- hm.geo %>%
  # add date field
  mutate(date = hour2date(interval, tz='Africa/Accra'),
         tod = as.numeric(hour2tod(interval, tz='Africa/Accra'))) %>%
  filter(date >= "2013-03-18" & date <= "2013-03-24") %>%
  group_by(site, tod) %>%
  dplyr::summarise(
    x = median(ccx),
    y = median(ccy),
    z = mean(users)) %>%
  # Scale over space and time
  mutate(z = standardize_st(site, tod, z)) %>%
  filter( !is.na(z) ) %>%
  as.data.frame %>%
  mutate(tod = as.numeric(tod))

# Calculate spatial correlation
hours <- c(7, 10, 14, 21)
spcor <- do.call(rbind, lapply(hours, function(hour){
  # Get bined data to improve perf.
  hm.hour <- hm %>% filter(tod == hour)
  spatial.corr(hm.hour$x, hm.hour$y, hm.hour$z, beta=5) %>%
    mutate(hour = hour)
}))
# saveRDS(spcor, 'rdata/senegal.spcorr.rds')

################################################
#  Visualize empirical spatial correlation
################################################

spcor <- readRDS('rdata/senegal.spcorr.rds')
cols <- c("orangered", "royalblue")

### AM
pdf("figures/senegal_spatial_correlation_pl_am.pdf", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5, lwd=1.2)

# peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==10)
magplot(shour$r, shour$corr, log='xy', col="#fc8d59", pch=21,
        xlab=expression(paste("Spatial lag ", h, " (km)")),
        ylab=expression(rho(h,0)), side=1:4, labels=c(T,T,F,F))
pl <- fit_truncated_power_law(shour$r, shour$corr, xmax=500, col=cols[1], lwd=4, lty=5)
text(10, 0.03, bquote(y ~ '~' ~ 0.283 * x^-0.638 * e^{x/365}), col=cols[1], cex=1.5)

# off-peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==7)
points(shour$r, shour$corr, log='xy', col="#67a9cf", pch=22)
pl <- fit_truncated_power_law(shour$r, shour$corr, xmax=500, col=cols[2], lwd=4, lty=5)
text(100, 0.15, bquote(y ~ '~' ~ 0.504 * x^-0.475 * e^{x/1029}), col=cols[2], cex=1.5)
legend(5, 0.01, c("Time 7:00", "Time 10:00 (peak)"), col=rev(cols), pch=c(22, 21), cex=1.5)
dev.off()

### PM
pdf("figures/senegal_spatial_correlation_pl_pm.pdf", width=7, height=7)
par(cex=1, tcl=0.5, mgp=c(2.5,0.5,0), cex.lab=2, cex.axis=1.5)

# peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==14)
magplot(shour$r, shour$corr, log='xy', col="#fc8d59", pch=21,
        xlab=expression(paste("Spatial lag ", h, " (km)")),
        ylab=expression(rho(h,0)), side=1:4, labels=c(T,T,F,F))
pl <- fit_truncated_power_law(shour$r, shour$corr, xmax=500, col=cols[1], lwd=4, lty=5)
text(10, 0.03, bquote(y ~ '~' ~ 0.854 * x^-0.821 * e^{x/488}), col=cols[1], cex=1.5)

# off-peak hour
shour <- dplyr::filter(spcor, r >= 0.1 & hour==21)
points(shour$r, shour$corr, log='xy', col="#67a9cf", pch=22)
pl <- fit_truncated_power_law(shour$r, shour$corr, xmax=600, col=cols[2], lwd=4, lty=5)
text(100, 0.15, bquote(y ~ '~' ~ 0.502 * x^-0.421 * e^{x/1611}), col=cols[2], cex=1.5)
legend(5, 0.01, c("Time 14:00 (peak)", "Time 21:00"), col=cols, pch=c(22, 21), cex=1.5)
dev.off()

