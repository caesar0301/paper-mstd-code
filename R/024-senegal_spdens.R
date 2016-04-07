library(movr)
library(rgeos)
library(maptools)
library(fitdistrplus)
library(mixtools)
library(gridExtra)

# Read people data
hm <- readRDS('rdata/heatmap.senegal.p06.i1h.rds')

##############################################
## Plot the spatial density of populaiton and fit the distributions:
## Log-normal, Gamma, Weibull, and mixture of log-norms
##############################################

# import rmlnorm() and fit.hist() from hz_spdens.R
i = 21
pdf(paste('figures/senegal_spatial_dens_h', i, '.pdf', sep=''), width=10, height=7)
hm.hour <- hm %>% dplyr::filter(interval == min(hm$interval) + i)
par(mar = c(4,5,2,2), mgp=c(3,1,0), cex=1.3, cex.lab=1.7,
    cex.axis=1.2, cex.main=1.2)
fit.hist(hm.hour$users, xlim=c(-1, 7))
dev.off()

##############################################
## Evaluate components parameters for lnorm-2 mixture
##############################################

mln.hourly <- function(hm){
  do.call(rbind, lapply(7:23, FUN = function(hour){
    # filter hourly data
    z <- dplyr::filter(hm, interval== (min(hm$interval) + hour))$users
    # estimate mixture parameters
    mln <- normalmixEM(log(z), k=2)
    # tidy data into clean form
    mu = mln$mu
    sigma = mln$sigma
    lambda = mln$lambda
    s <- c(1,2)
    if (lambda[1] <= lambda[2])
      s <- c(2,1)
    data.frame(h = hour,
               m1.mu = mu[s[1]],
               m1.sigma = sigma[s[1]],
               m1.lambda = lambda[s[1]],
               m2.mu = mu[s[2]],
               m2.sigma = sigma[s[2]],
               m2.lambda = lambda[s[2]]) %>%
      gather(p, V, m1.mu:m2.lambda) %>%
      separate(p, c("p", "compo")) %>%
      spread(compo, V)
  }))
}
mlnh <- mln.hourly(hm)

# Plot the hourly tendency
postscript("figures/senegal_spatial_density_mixture.eps", width=8, height=8)

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
