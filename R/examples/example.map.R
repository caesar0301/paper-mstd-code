library(maptools)
library(PBSmapping)
library(rgdal)
library(classInt)
library(sp)
library(gpclib)
library(splancs)

hongkong=readShapePoly("maps/HK/HKG_adm0.shp")
hangzhou=readShapePoly("maps/hangzhou/landuse.shp")
china=readShapePoly("maps/CHN/CHN_adm0.shp")

plot(china,border="light green",col="light green")
lp=hongkong@polygons[[1]]
for(i in 1:length(slot(lp,"Polygons"))){
  hk=slot(slot(lp,"Polygons")[[i]],"coords")
  polygon(hk[,1]-114.2-73.7,
          hk[,2]-22.25+45.48,
          col="light blue",
          border="black",
          density=25)}
