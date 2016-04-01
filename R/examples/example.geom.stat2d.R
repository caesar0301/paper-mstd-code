data(diamonds)

p <- ggplot(diamonds,aes(carat,price)) +
  stat_summary2d(fun=sum,aes(z=depth))
df <- ggplot_build(p)$data[[1]]
df$x <- with(df,(xmin+xmax)/2)
df$y <- with(df,(ymin+ymax)/2)
ggplot(diamonds,aes(carat,price))+stat_bin2d()+
  geom_point(data=df,aes(x=x,y=y,size=value),color="red",shape=1)