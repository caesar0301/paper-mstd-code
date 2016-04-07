library(dplyr)
library(data.table)

# City Cellular Traffic Map (C2TM):
#
# From 2012-08-19T00:00:00 to 2012-08-26T23:00:00
# 1345305600 - 1345996800
# Total records: 1625680
# Total BS: 13269
# Packets: KP
# Bytes: KB
# Time: GMT+8

# Select fields to open source
heatmap <- readRDS("rdata/heatmap.hz.fd8.rds")
public <- heatmap[,c("bs", "thour", "lon", "lat","packets", "bytes", "users")]

# Format data fields
bs.unique <- unique(public$bs)
bs.df <- data.frame(bs=bs.unique, id=seq(1, length(bs.unique)))

public.new <- public %>%
  left_join(bs.df, by="bs") %>%
  mutate(
    thour = thour * 3600,
    lon = format(lon - 9.1, digits=9),
    lat = format(lat - 17.13, digits=9),
    packets = packets * 1024,
    bytes = bytes * 1024
    )

# Export network topology
topo <- public.new %>%
  select(id, lon, lat) %>%
  unique %>%
  setnames(c("bs", "lon", "lat"))
write.table(topo, "topology.csv", sep=",", row.names=F,
            fileEncoding = "utf-8", quote=F)

# Export network traffic
traffic <- public.new %>%
  select(id, thour, users, packets, bytes) %>%
  setnames(c("bs", "time_hour", "users", "packets_KP", "bytes_KB"))
write.table(traffic, "cellular_traffic.csv", sep=",", row.names=F,
            fileEncoding="utf-8", quote=F)