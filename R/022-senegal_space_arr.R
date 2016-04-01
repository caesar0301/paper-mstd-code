library(movr)
library(rgeos)
library(maptools)
library(qlcVisualize)
library(RColorBrewer)
library(animation)
library(classInt)

# Read map data
ms <- readShapeSpatial('../maps/senegal/senegal_arr_2014_wgs.shp')

# Read people data
hm <- read.csv('../data/scl_fine_p06_hm_i1h', sep=',', head=F)
colnames(hm) <- c('interval', 'site', 'users')

# Read mobile network topology
mobnet <- read.csv('../data/senegal_site_lonlat.csv', head=T)

# Join with people data
hm.arr <- hm %>% left_join(mobnet, by=c('site'='site_id')) %>%
  dplyr::select(interval, arr_id, users) %>%
  group_by(interval, arr_id) %>%
  summarise(users = sum(users))

# Find colors over global intervals
nclr <- 9
pal <- rev(brewer.pal(nclr, 'RdYlBu'))
class <- classIntervals(hm.arr$users, nclr, style='quantile')
hm.arr$col <- findColours(class, pal)

hm.arr.wide <- hm.arr %>% dplyr::select(-users) %>%
  spread(key=interval, value=col, fill=pal[1])
ms@data <- ms@data %>% left_join(hm.arr.wide, by=c('ARR_ID'='arr_id'))

for(i in seq(0, 23)){
  pdf(paste('figures/senegal_space_arr_h', i, '.pdf', sep=''), width=10, height=8)
  ccol <- ms@data[, paste(378768 + i, sep='')]
  plot(ms, col=ccol)
  title("Population dist. in Senegal, Africa")
  legend(x=-12.5, y=16.5, legend=leglabs(round(class$brks)),  fill=pal, bty='n')
  dev.off()
}
