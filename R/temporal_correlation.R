# Visualize the temporal distribution of cellular traffic.
library(tidyr)
library(dplyr)
library(animation)

# Different regions
regions <- list(
  metro = c(120.069897,30.180852,120.236048,30.348045),
  urban.a = c(120.167722,30.255391,120.197546,30.281592),
  rural.b = c(120.16678,30.300872,120.200125,30.336536),
  rural.c = c(120.069735,30.285958,120.125214,30.3331))

#####################################
## Data flow
#####################################

# Read data set of heatmap output by `heatmap.R`
# Entire region
heatmap <- readRDS("rdata/heatmap.hz.fd8.rds")

select.region <- function(heatmap, area){
  heatmap %>%
    filter(in.area(lon, lat, area)) %>%
    mutate(date = hour2date(thour)) %>%
    filter(date >= "2012-08-20" & date <= "2012-08-26") %>%
    # generate hourly summary
    group_by(thour) %>%
    dplyr::summarise(
      date = unique(date),
      bytes = sum(bytes),
      packets = sum(packets),
      users = sum(users)) %>%
    # scale over a day
    group_by(date) %>%
    mutate(
      bytes = standardize(bytes),
      packets = standardize(packets),
      users = standardize(users)) %>%
    arrange(thour) %>%
    as.data.frame()
}

# select a base station randomly from given area
select.random.bs <- function(heatmap, area){
  hm.area <- filter(heatmap, in.area(lon, lat, area))
  bs.all <- unique(hm.area$bs)
  heatmap[heatmap$bs == sample(bs.all, 1),]
}

hm.urban.a <- select.region(heatmap, regions$urban.a)
hm.rural.b <- select.region(heatmap, regions$rural.b)
hm.rural.c <- select.region(heatmap, regions$rural.c)

hm.urban.bs1 <- select.random.bs(heatmap, regions$urban.a)
hm.urban.bs2 <- select.random.bs(heatmap, regions$urban.a)
hm.rural.bs1 <- select.random.bs(heatmap, regions$rural.b)
hm.rural.bs2 <- select.random.bs(heatmap, regions$rural.b)

#####################################
## Visualize time series in different regions
#####################################

library(ggplot2)
par(mfrow=c(1,1))
series <- data.frame(thour=hm.urban.a$thour - min(hm.urban.a$thour),
                     urban=hm.urban.a$packets,
                     rural=hm.rural.b$packets)
series <- gather(series, region, traffic, -thour)
p <- ggplot(series, aes(thour, traffic*100, group=region,
                        color=region, linetype=region)) +
  theme_bw() + geom_line(size=1.2) +
  xlab("Time (hour)") +
  ylab("Normalized Traffic (%)") +
  scale_color_discrete(
    name = "",
    breaks=c("urban", "rural"),
    labels=c("Urban (A)", "Rural (BC)")) +
  scale_linetype_discrete(
    name = "",
    breaks=c("urban", "rural"),
    labels=c("Urban (A)", "Rural (BC)")) +
  theme(legend.position = c(0.8, 0.15),
        legend.background = element_blank(),
        axis.title = element_text(size=17),
        axis.text = element_text(size=13),
        legend.text = element_text(size=15))
ggsave("figures/spatial_corr_regions_series.eps", p, width=6, height=4)

#####################################
## Visualize autocorrelation functions
#####################################

plot.region.acf <- function(hm.region, hm.bs1, hm.bs2,
                            area=c("urban", "rural")){
  if (area == "urban"){
    config <- list(name.region = "Urban",
                   name.bs1 = "Urban BTS#1",
                   name.bs2 = "Urban BTS#2",
                   filename = "figures/spatial_corr_urban.eps")
  } else if (area == "rural") {
    config <- list(name.region = "Rural",
                   name.bs1 = "Rural BTS#1",
                   name.bs2 = "Rural BTS#2",
                   filename = "figures/spatial_corr_rural.eps")
  } else {
    stop("Wrong area parameter")
  }
  
  region <- acf(hm.region$packets, lag.max=max(hm.region$thour), plot=FALSE)
  region.bs1 <- acf(hm.bs1$packets, lag.max=max(hm.bs1$thour), plot=FALSE)
  region.bs2 <- acf(hm.bs2$packets, lag.max=max(hm.bs2$thour), plot=FALSE)
  
  df <- rbind(data.frame(lag=region$lag[,1,1],
                         acf=region$acf[,1,1],
                         type=config$name.region),
              data.frame(lag=region.bs1$lag[,1,1],
                         acf=region.bs1$acf[,1,1],
                         type=config$name.bs1),
              data.frame(lag=region.bs2$lag[,1,1],
                         acf=region.bs2$acf[,1,1],
                         type=config$name.bs2))
  
  p <- ggplot(df, aes(lag, acf, group=type, color=type, linetype=type)) +
    theme_bw() + geom_line(size=1.2) +
    geom_hline(yintercept = c(0.2, -0.2), linetype=3, color="black") +
    xlab(expression(paste("Temporal lag ", u, " (hour)"))) +
    ylab(expression(rho(0, u))) +
    theme(legend.position=c(0.8, 0.8),
          axis.title = element_text(size=17),
          axis.text = element_text(size=13),
          legend.text = element_text(size=15))
  ggsave(config$filename, p, width=6, height=4)
}

plot.region.acf(hm.urban.a, hm.urban.bs1, hm.urban.bs2, "urban")
plot.region.acf(hm.rural.b, hm.rural.bs1, hm.rural.bs2, "rural")