library(geoR)
ozone<-read.table("http://www.ats.ucla.edu/stat/r/faq/ozone.csv", sep=",", header=T)
head(ozone, n=10)
ozone
head(ozone)
breaks = seq(0, 1.5, l = 11)
v1 <- variog(coords = ozone[,3:4], data = ozone[,2], breaks = breaks)
v1
plot(v1, type = "b", main = "Variogram: Av8top")