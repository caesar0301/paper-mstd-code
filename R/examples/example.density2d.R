library(ggplot2)
library(MASS)

## generate some random data
data <- data.frame(a=abs(rnorm(1000, mean=0.5)), b=abs(rnorm(1000, mean=0.5)))
data$a <- data$a / max(data$a)
data$b <- data$b / max(data$b)


## layout settings for ggplot
t2 <- theme(
  axis.line = element_line(colour = "black"),
  axis.text = element_text(colour = "black"),
  axis.ticks = element_line(colour = "black"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank()
)


## generate the "z" coordinate (density) just for the correct midpoint in the color gradient
z <- kde2d(data$a, data$b)

g <- ggplot(data, aes(x=a, y=b)) +
  stat_density2d(aes(fill=..density..), geom="tile", contour=FALSE) +
  scale_fill_gradient2(low="#44aa00", mid="#ffcc00", high="#502d16", midpoint=mean(range(z$z))) +
  ## limit scale ( density is calculated on the limited scale, all other points are removed from calculation)
  #xlim(0,1) +
  #ylim(0,1) +
  ## limit view area ( density is calcluated on all points, no points removed )
  coord_cartesian(xlim = c(0, 1), ylim=c(0,1)) +
  xlab("x method") +
  ylab("y method") +
  ## add points to the density map (comment it if not desired)
  geom_point(size=0.6, colour="white",shape=1, alpha=0.2) +
  ## make a line from (0,0) - (1,1)
  #geom_segment(aes(x=0, y=0, xend=1, yend=1)) +
  ## or just create a abline with slope 1 and intercept 0
  geom_abline() +
  t2
plot(g)