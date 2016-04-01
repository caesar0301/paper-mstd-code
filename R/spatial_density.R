#!/usr/bin/env R
# Analyze and modeling the spatial density distribution
# of cellular traffic.
library(ggplot2)
library(tidyr)
library(dplyr)
library(fitdistrplus)
library(animation)
library(mixtools)
library(grid)

# Read heatmap.RDS generate by heatmap.R
heatmap.fd8 <- readRDS("rdata/heatmap.hz.fd8.rds")

# average traffic over a week
heatmap <- heatmap.fd8 %>%
  dplyr::group_by(bs, tod) %>%
  dplyr::summarize(
    lat = median(lat),
    lon = median(lon),
    x = median(ccx),
    y = median(ccy),
    z = mean(bytes))

# Metropolitan area
area <- c(120.069897,30.180852,120.236048,30.348045)
heatmap.m <- filter(heatmap, in.area(lon, lat, area))


##############################################
## Plot the spatial density of traffic and fit the distribution with
## Log-normal, Gamma, Weibull, and mixture of log-norms
##############################################

# generate random value of lnorm-2 mixture
rmlnorm <- function(n, mu, sigma, lambda){
  z <- rbinom(n, 1, lambda[1])
  z*rlnorm(n, mu[1], sigma[1]) + (1-z)*rlnorm(n, mu[2], sigma[2])
}

plot.spatial.density <- function(hm, hour){
  hm <- filter(hm, tod==hour)
  v <- hm$z[hm$z>0]
  
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
  
  # plot histogram and fitted densities
  logv <- log(v)
  xlim <- c(0, 15)
  logv <- logv[logv>=xlim[1] & logv<=xlim[2]]
  h <- hist(logv, breaks=seq(min(logv)-1, max(logv)+1, .5),
            freq=FALSE, xlim=xlim, axes=FALSE,
            xlab="Traffic volume of each cell (KB)",
            ylab="Density",
            main=sprintf("Time:%02d:00", hour))
  Axis(side=2)
  at10 <- round(log10(exp(h$mids)))
  atseq <- seq_along(at10)
  at <- seq(min(atseq), max(atseq),
            by=ceiling(length(at10)/length(unique(at10))))
  labels <-sapply(at10[at],function(i) as.expression(bquote(10^ .(i))))
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

# Animation plot
saveGIF({
  for ( hour in sort(unique(heatmap.m$tod))) {
    plot.spatial.density(heatmap.m, hour)
  }}, movie.name = "spatial_density.gif", interval=1, ani.width=600,
  ani.height=400)

hours <- c(7, 14, 21)
for( hour in hours){
  fname <- sprintf("figures/spatial_density%02d.eps", hour)
  postscript(fname, width=8, height=5)
  par(mar = c(4,5,2,2), mgp=c(3,1,0), cex=1.3, cex.lab=1.7,
      cex.axis=1.2, cex.main=1.2)
  plot.spatial.density(heatmap.m, hour)
  dev.off()
}


##############################################
## Evaluate components parameters for lnorm-2 mixture
##############################################

mln.hourly <- function(hm){
  do.call(rbind, lapply(0:23, FUN = function(hour){
    # filter hourly data
    z <- filter(hm, tod==hour & z >0)[["z"]]
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

# Plot the hourly tendency
library(gridExtra)
postscript("figures/spatial_density_mixture.eps", width=8, height=8)
mlnh <- mln.hourly(heatmap.m)
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


##############################################
## Evaluate goodness-of-fit for lnorm-2 mixture distribution
##############################################

# Bootstrap estimation of ks statistic distribution
# x - a vector of univariate observations
# n - the number of bootstrap interations
ks.bootstrap <- function(x, n=1000){
  n = ifelse(n <= 0 , 1000, n)
  
  # lnorm mixture PDF
  pmlnorm <- function(x, meanlog, sdlog, pmix) {
    pmix[1]*plnorm(x,meanlog[1],sdlog[1]) + 
      (1-pmix[1])*plnorm(x,meanlog[2],sdlog[2])
  }
  
  # test ks statistics of lnorm-2 mixture
  ks.mln <- function(seed){
    set.seed(seed)
    x.s <- sample(x, round(0.7*length(x)), replace = FALSE)
    par.s <- normalmixEM(log(x.s), k=2)
    ks.test(x.s, pmlnorm, meanlog=par.s$mu, sdlog=par.s$sigma,
            pmix=par.s$lambda)$statistic
  }
  
  # test ks statistics of weibull distribution
  ks.wb <- function(seed){
    set.seed(seed)
    x.s <- sample(x, round(0.7*length(x)), replace = FALSE)
    fd <- fitdist(x.s, distr='weibull')
    gofstat(fd)[["ks"]]
  }
  
  # test ks statistics of log-normal distribution
  ks.ln <- function(seed){
    set.seed(seed)
    x.s <- sample(x, round(0.7*length(x)), replace = FALSE)
    fd <- fitdist(x.s, distr='lnorm')
    gofstat(fd)[["ks"]]
  }
  
  ks1 <- sapply(1:n, FUN = ks.wb)
  ks2 <- sapply(1:n, FUN = ks.ln)
  ks3 <- sapply(1:n, FUN = ks.mln)
  data.frame(ks.wb = ks1, ks.ln = ks2, ks.mln = ks3)
}

# Bootstrap estimation for each hour from 00:00 to 23:00
# need speedup using, e.g., mclapply() in parallel package
ks.hourly <- do.call(rbind, lapply(0:23, FUN = function(hour){
  print(sprintf("Processing hour %2d:00", hour))
  v <- filter(heatmap.m, tod==hour & z>0)[["z"]]
  ks.bs <- ks.bootstrap(v, 1000) # 1000 runs
  ks.bs$h <- hour
  ks.bs
}))
#saveRDS(ks.hourly, "rdata/spatial.density.ks.rds")

##############################################
# Plot the ks statistics with bootstrap estimation
##############################################

plot.ks.hourly <- function(ks.hourly){
  # make data wide and the model name brief
  ksh <- ks.hourly %>%
    gather(model, ks, ks.wb:ks.mln) %>%
    extract(model, "model", "[[:alpha:]]+\\.([[:alpha:]]+)") %>%
    group_by(h, model) %>%
    summarise(
      ks.5 = quantile(ks, 0.05),
      ks.m = mean(ks),
      ks.95 = quantile(ks, 0.95))
  
  pd <- position_dodge(0.5) # move them .05 to the left and right
  ggplot(ksh, aes(x=as.factor(h), y=ks.m, group=model,
                  shape=model, color=model)) +
    geom_errorbar(aes(ymax = ks.95, ymin = ks.5),
                  colour="black", width=.5, position=pd) +
    geom_line(position=pd) + theme_bw() +
    geom_point(position=pd, size=2, shape=21, fill="white") +
    xlab("Time (hour)") + ylab("K-S statistics") +
    scale_colour_hue(name="",
                     breaks=c("wb", "ln", "mln"),
                     labels=c("Weibull", "LN", "LN mixture"),
                     l=40) +
    # Use darker colors, lightness=40, scientific style
    theme(axis.title.x = element_text(size=18, vjust=-1),
          axis.title.y = element_text(size=18, vjust=2),
          axis.text = element_text(size=16),
          legend.text = element_text(size=18),
          legend.justification=c(1, 0),
          legend.position="top",
          legend.key.width = unit(10, units="mm"))
}

ks.hourly <- readRDS("rdata/spatial.density.ks.rds")
postscript("figures/spatial_density_ks.eps", width=6, height=4)
plot.ks.hourly(filter(ks.hourly, h>=7 & h <= 23))
dev.off()