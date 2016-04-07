#!/usr/bin/env R
# Estimate the quality of mobile heatmap data.
library(data.table)
library(bit64)
library(dplyr)
library(tidyr)
library(plotrix)
library(omniR)

# Read data
heatmap <- readRDS('rdata/heatmap.hz.rds')

# Calculate accumulated traffic for each hour
time.density <- heatmap %>%
  mutate(
    packets = packets / 1024, #KP
    bytes = bytes / 1024^3,   #GB
    date = hour2date(thour),
    tod = hour2tod(thour)
    ) %>%
  group_by(date, tod) %>%
  summarize(
    bs = length(unique(bs)),
    pkts = sum(packets, na.rm=TRUE),
    byts = sum(bytes, na.rm=TRUE),
    users = sum(users,  na.rm=TRUE)) %>%
  mutate(
    byts = byts/1024/10^6) %>% #10^6 PB
  data.frame()

# Change data subset into matrix
time.dl <- time.density %>%
  select(date, tod, byts) %>%
  spread(tod, byts)
time.dl[is.na(time.dl)] <- 0
# Format row names
rownames(time.dl) <- format(time.dl$date, "%m.%d")
# skip the first row with wrong stat.
time.dl <- time.dl[2:nrow(time.dl), 2:ncol(time.dl)]
colnames(time.dl) <- seq(0, 23)

# Plot data quality
cellcol <- plotrix::color.scale(data.matrix(time.dl), c(1,0), 1, c(1,0))
# Avoid the name conflict with the same method in fields package
color2D.matplot(time.dl, xlab='Time of Day', ylab="",
                main="Data collection over days (10^6 PB)",
                cellcolors=cellcol, axes=F,
                show.legend=T)
axis(1, at=seq(0,23)+0.5, label=seq(0,23))
axis(2, at=rev(seq(1,length(rownames(time.dl))))-0.5,
     las=1, label=rownames(time.dl), "%m.%d")