library(movr)

filter <- dplyr::filter

# Read data set of heatmap
heatmap <- readRDS("rdata/heatmap.senegal.p06.i1h.rds")

# Read network topology
mobnet <- readRDS('rdata/senegal.bsnet.rds')
hm.geo <- heatmap %>% inner_join(mobnet, by=c('site'='site_id'))

hm.geo %>%
  mutate(date = hour2date(interval, tz='Africa/Accra'),
         tod = as.numeric(hour2tod(interval, tz='Africa/Accra'))) %>%
  filter(date >= "2013-03-18" & date <= "2013-03-24") %>%
  # generate hourly summary
  group_by(interval) %>%
  dplyr::summarise(
    date = unique(date),
    users = sum(users)) %>%
  # scale over a day
  group_by(date) %>%
  mutate(users = standardize(users)) %>%
  arrange(interval) %>%
  as.data.frame()