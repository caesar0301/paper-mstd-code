library(movr)
library(rgeos)
library(maptools)
library(fitdistrplus)
library(mixtools)
library(gridExtra)

# Read map data
ms <- readShapeSpatial('../maps/senegal/senegal_arr_2014_wgs.shp')
ms.border <- gUnionCascaded(ms)
ms.border.owin <- as(ms.border, 'owin')

pdf('figures/senegal_map.pdf', width=8, height=6)
plot(ms, border="grey", lwd=0.5)
dev.off()

# Read mobile network topology
mobnet <- read.csv('../data/senegal_site_lonlat.csv', head=T)

# Read people data
hm <- read.csv('../data/scl_fine_p06_hm_i1h', sep=',', head=F)
colnames(hm) <- c('interval', 'site', 'users')
hm.geo <- hm %>% left_join(mobnet, by=c('site'='site_id'))

##############################################
## Plot the spatial density of populaiton and fit the distributions:
## Log-normal, Gamma, Weibull, and mixture of log-norms
##############################################

# generate random value of lnorm-2 mixture
rmlnorm <- function(n, mu, sigma, lambda){
  z <- rbinom(n, 1, lambda[1])
  z*rlnorm(n, mu[1], sigma[1]) + (1-z)*rlnorm(n, mu[2], sigma[2])
}

fit.hist <- function(v, log=TRUE, xlim=c(-1,6), ...) {
  v <- v[v>0]
  
  # fit weibull distribution of z value
  fit.wb <- fitdist(v, distr='weibull')
  pars.wb <- fit.wb[['estimate']]
  sim.wb <- rweibull(length(v), pars.wb[1], pars.wb[2])
  
  # fit log-normal distribution of z value
  fit.ln <- fitdist(v, distr='lnorm')
  pars.ln <- fit.ln[['estimate']]
  sim.ln <- rlnorm(length(v), pars.ln[1], pars.ln[2])
  
  # fit the mixture distributions with two log-normals
  pars.ml <- normalmixEM(log(v), k=2)
  sim.mix <- rmlnorm(length(v), pars.ml$mu, pars.ml$sigma, pars.ml$lambda)
  
  # Plot histogram
  if (log) { v <- log(v) }
  v <- v[v>=xlim[1] & v<=xlim[2]]
  h <- hist(v, breaks=seq(min(v)-1, max(v)+1, .2),
            freq=FALSE, xlim=xlim, axes=FALSE, ...)
  
  # Change x-axis lables
  Axis(side=2)
  at2 <- round(log(exp(h$mids)))
  atseq <- seq_along(at2)
  at <- seq(min(atseq), max(atseq), by=ceiling(length(at2)/length(unique(at2))))
  labels <-sapply(at2[at],function(i) as.expression(bquote(2^ .(i))))
  
  # Add fitted densities
  Axis(side=1, at=h$mids[at], labels=labels, lty=NULL)
  lines(density(log(sim.wb)), lty=2, col="#FF00CC", lwd=2)
  lines(density(log(sim.ln)), lty=3, col="#CC0033", lwd=2)
  lines(density(log(sim.mix)), lty=4, col="#000066", lwd=2)
  legend(min(h$breaks) + 0.05 * (max(h$breaks) - min(h$breaks)),
         min(h$density) + 0.85 * (max(h$density) - min(h$density)),
         legend=c("Weibull", "LN", "LN mixture"),
         lty = c(2,3,4),
         col = c("#FF00CC", "#CC0033", "#000066"))
}

i = 21
pdf(paste('figures/senegal_spatial_dens_h', i, '.pdf', sep=''), width=10, height=7)
hm.hour <- hm.geo %>% dplyr::filter(interval == min(hm.geo$interval) + i)
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
mlnh <- mln.hourly(hm.geo)

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
