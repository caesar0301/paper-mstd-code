#!/usr/bin/env R
# Learn the spatio-temporal traffic model based on STCF.
library(tidyr)
library(dplyr)
library(stringr)
library(CompRandFld)
library(RColorBrewer)
library(animation)
library(ggplot2)

# Read data set of heatmap output by `heatmap.R`
# Entire region
heatmap <- readRDS("rdata/heatmap.hz.fd8.rds")

# different regions in the city
regions <- list(
  metro = c(120.069897,30.180852,120.236048,30.348045),
  urban = c(120.167722,30.255391,120.197546,30.281592),
  rural = c(120.16678,30.300872,120.200125,30.336536))

region <- "urban"
resolution <- c(10, 10)

################################################
#  Preprocessing: manipulate spatial and temporal resolutions
################################################

hm <- heatmap %>%
  # select spatial and temporal regions
  filter(in.area(lon, lat, regions[[region]])) %>%
  mutate(date = hour2date(thour)) %>%
  filter(date >= "2012-08-20" & date <= "2012-08-26") %>%
  # divide area into blocks
  mutate(
    ccx = ccx - min(ccx),
    ccy = ccy - min(ccy),
    thour = thour - min(thour) + 1,
    x = vbin(ccx, resolution[1], type="c"),
    y = vbin(ccy, resolution[2], type="c"),
    block = str_c(vbin(ccx, resolution[1]),
                  vbin(ccy, resolution[2]), sep="_")) %>%
  # remove early morning data
  filter(tod >= 6 & tod <= 23) %>%
  # generate statistics of each block
  group_by(thour, block) %>%
  dplyr::summarise(
    date = unique(date),
    x = median(x),
    y = median(y),
    z = sum(bytes)) %>%
  as.data.frame() %>%
  # scale data over space and time
  mutate(st = stscale(block, thour, z)) %>%
  filter(!is.na(st)) %>%
  # order chronology
  arrange(thour) %>%
  as.data.frame()

spatial.grid <- hm %>%
  select(block, x, y) %>%
  group_by(block) %>%
  dplyr::summarise(
    x = unique(x),
    y = unique(y))

# transform data into wide form
hm.wide <- hm %>%
  select(thour, block, st) %>%
  spread(key = thour, value = st) %>%
  left_join(spatial.grid, by = "block") %>%
  # replace NA with column mean
  sapply(function(x) {x[is.na(x)] <- mean(x,na.rm=TRUE);x}) %>%
  as.data.frame()

################################################
#  Visualizing traffic distribution
################################################

plot.heatmap <- function(x, y, z, nx, ny){
  val2D <- vbingrid(x, y, z, na=0, nx=nx, ny=ny)
  x <- as.numeric(rownames(val2D))
  y <- as.numeric(colnames(val2D))
  filled.contour(x-min(x), y-min(y),
                 val2D, nlevels=10,
                 xlab="East-West Dist. (km)",
                 ylab="North-South Dist. (km)",
                 col=rev(brewer.pal(10, "Spectral")))
  abline(v=x, col="white", lty="dotted")
  abline(h=y, col="white", lty="dotted")
}

saveGIF({
  for (hour in seq(7,23)){
    par(mar = c(4,4,2,2), mgp=c(2.5,1,0), cex.lab=1.5, cex.axis=1.5)
    hm.oh <- filter(hm, thour == hour)
    plot.heatmap(hm.oh$x, hm.oh$y, hm.oh$st, nx=10, ny=10)
    title(sprintf("Time: %02d:00", hour), col="red")
  }}, movie.name=sprintf("heatmap_region_%s.gif", region),
  interval=1, ani.width=600, ani.height=450)

################################################
#  Learn covariance models
################################################

# extract coordinates used by CompRandFld
extract.coords <- function(hm.wide){
  space <- as.matrix(hm.wide[,c("x", "y")])
  class(space) <- "numeric"
  hm.wide2 <- select(hm.wide, num_range("", 1:1000))
  obs <- t(as.matrix(hm.wide2))
  class(obs) <- "numeric"
  obs[is.na(obs)] <- 0
  time <- as.numeric(colnames(hm.wide2))
  list(coords=space, coordt=time, data=obs)
}

# calculate zero-lag empirical covariance
ECovZero <- function(stmat){
  lres <- apply(stmat, 2, function(x) acf(x, type="covariance",plot=F)[0])
  mean(sapply(lres, "[[", 1))
}

idats <- list(mon=1, tue=25, wed=49, thu=73, fri=97, sat=121, sat=145)

# select train and test dataset at daily granularity
prepare.data <- function(hm.wide, day){
  # one day as train data and the following data as test
  start = idats[[day]]-1
  tr = select(hm.wide, block, x, y, num_range("", start:(start+24)))
  ts = select(hm.wide, block, x, y, num_range("", (start+25):(start+48)))
  list(train=extract.coords(tr), test=extract.coords(ts))
}

# Experiment configuration
# models(3): exp, cauchy, gneiting
# days(2): weekday, weekend
# regions(3): urban, rural
rmses <- data.frame()
for( model in c("gneiting") ){
  for ( day in names(idats)[-7] ){
    print(sprintf("Fitting model %s at %s, %s", model, day, region))
    # prepare data
    idat <- prepare.data(hm.wide, day)
    train <- idat$train
    test <- idat$test
    
    # estimate model parameters
    mean <- 0
    sill <- 1
    nugget <- 0
    scale <- 3
    maxtime <- 6
    maxdist <- 3
    
    # Maximum likelihood fitting of the space time random field:
    fit <- FitComposite(data=train$data, coordx=train$coords,
                        coordt=train$coordt, corrmodel=model,
                        maxtime=maxtime, maxdist=maxdist, varest=TRUE)
    saveRDS(fit, sprintf("rdata/stcov.fit.%s.%s.%s.rds", model, day, region))
    
    # Empirical variogram
    vario <- EVariogram(train$data, coordx=train$coords,
                       coordt=train$coordt, maxtime=maxtime, maxdist=maxdist)
    saveRDS(vario, sprintf("rdata/stcov.vario.%s.%s.%s.rds", model, day, region))
    
    # Prediction, kriging
    locs <- test$coords
    xx <- seq(min(locs[,1]), max(locs[,1]), length=resolution[1])
    yy <- seq(min(locs[,2]), max(locs[,2]), length=resolution[2])
    loc_to_pred <- as.matrix(expand.grid(xx, yy))
    time_to_pred <- test$coordt
    data_real <- test$data
    param <- as.list(c(fit$param,fit$fixed))
    pr<-Kri(loc=locs, time=time_to_pred,
            coordx=train$coords, coordt=train$coordt, data=train$data,
            corrmodel=model, param=param, type_krig="Simple")
    saveRDS(pr, sprintf("rdata/stcov.pr.%s.%s.%s.rds", model, day, region))
    
    # calculate prediction error
    print(RMSE(data_real, pr$pred))
    rmses <- rbind(rmses, data.frame(
      m = model, d = day, r = region, e = RMSE(data_real, pr$pred)))
  }
}
saveRDS(rmses, sprintf("rdata/stcov.rmses.%s.rds", region))

################################################
#  Visualizing modeling estimation results
################################################

for( model in c("exp_exp", "exp_cauchy", "gneiting") ){
  for ( day in names(idats)[-7] ){
    fit <- readRDS(sprintf("rdata/stcov.fit.%s.%s.%s.rds",model,day,region))
    vario <- readRDS(sprintf("rdata/stcov.vario.%s.%s.%s.rds", model,day,region))
    
    # plot covariance and spatio-temporal profiles
    postscript(sprintf("figures/stcov_%s_%s_%s.eps", model, day, region),
               width=9, height=3)
    covar <- Covariogram(fit,fix.lagt=3,fix.lags=1,show.cov=TRUE,
                         answer.cov=TRUE, answer.vario=TRUE)
    dev.off()
    
    # QQ plot
    spots <- amatch(vario$bins, covar$lags)
    est.vario <- covar$variogram[spots, 2:7]
    emp.vario <- matrix(vario$variogramst, ncol=6)
    qqplot(est.vario, emp.vario, xlim=c(0))
  }
}


################################################
#  Visualizing prediction results
################################################

read.pred <- function(model, day, region){
  readRDS(sprintf("rdata/stcov.pr.%s.%s.%s.rds", model, day, region))
}

# Q-Q plot
model <- "gneiting"
day <- "fri"
data.pred <- read.pred(model, day, region)
data.real <- prepare.data(hm.wide, day)$test
df <- data.frame(pred = c(data.pred$pred),
                 real = c(data.real$data))
p <- ggplot(df, aes(real, pred)) + theme_bw() + geom_point() +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  xlab("Normalized Empirical Traffic") +
  ylab("Normalized Predicted Traffic")
ggsave(sprintf("figures/stcov_cmp_%s_%s.eps", model, region), p,
       width=5, height=4)