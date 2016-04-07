library(movr)
library(rgeos)
library(maptools)
library(fitdistrplus)
library(mixtools)
library(gridExtra)

# Read people data
hm <- readRDS('rdata/heatmap.sjtu.1411.i1h.rds')

# import rmlnorm() and fit.hist() from hz_spdens.R
i = 21
pdf(paste('figures/sjtu_spatial_dens_h', i, '.pdf', sep=''), width=10, height=7)
hm.hour <- hm %>% dplyr::filter(interval == min(hm$interval) + i)
par(mar = c(4,5,2,2), mgp=c(3,1,0), cex=1.3, cex.lab=1.7,
    cex.axis=1.2, cex.main=1.2)
fit.hist(hm.hour$users, xlim=c(-1, 8), main="")
dev.off()

# import mln.hourly() from hz_spdens.R
mlnh <- mln.hourly(hm)

# Plot the hourly tendency
postscript("figures/sjtu_spatial_density_mixture.eps", width=8, height=8)

# plot mu
p1 <- ggplot(mlnh, aes(x = as.factor(h), y=mu, group=p, color=p)) +
  theme_bw() +
  geom_line() +
  geom_point(aes(shape=p), size=3) +
  xlab("Time (hour)") + ylab(expression(mu)) +
  scale_colour_discrete(name="Component",
                        breaks=c("m1", "m2"),
                        labels=c("Model1", "Model2"),
                        l=40) +
  scale_shape_discrete(name  ="Component",
                       breaks=c("m1", "m2"),
                       labels=c("Model1", "Model2"))
# plot sigma
p2 <- ggplot(mlnh, aes(x = as.factor(h), y=sigma, group=p, color=p)) +
  theme_bw() +
  geom_line() +
  geom_point(aes(shape=p), size=3) +
  xlab("Time (hour)") + ylab(expression(sigma)) +
  scale_colour_discrete(name="Component",
                        breaks=c("m1", "m2"),
                        labels=c("Model1", "Model2"),
                        l=40) +
  scale_shape_discrete(name  ="Component",
                       breaks=c("m1", "m2"),
                       labels=c("Model1", "Model2"))
# plot lambda
p3 <- ggplot(mlnh, aes(x = as.factor(h), y=lambda, group=p, color=p)) +
  theme_bw() +
  geom_line() +
  geom_point(aes(shape=p), size=3) +
  xlab("Time (hour)") + ylab(expression(lambda)) +
  scale_colour_discrete(name="Component",
                        breaks=c("m1", "m2"),
                        labels=c("Model1", "Model2"),
                        l=40) +
  scale_shape_discrete(name  ="Component",
                       breaks=c("m1", "m2"),
                       labels=c("Model1", "Model2"))
grid.arrange(p1, p2, p3, nrow=3, ncol=1)
dev.off()
