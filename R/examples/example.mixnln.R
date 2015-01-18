# An example to fit a mixture of lognormal and normal distributions 

x.1<-rnorm(6000, 2.4, 0.6)
x.2<-rlnorm(10000, 1.3,0.1)

X<-c(x.1, x.2)
hist(X,100,freq=FALSE, ylim=c(0,1.5))
lines(density(x.1), lty=2, lwd=2)
lines(density(x.2), lty=2, lwd=2)
lines(density(X), lty=4)

fitnormlnorm<-function(par, val) {
  p <- 1/(1+exp(-par[5]))
  return(-sum(log(p*dnorm(val, par[1], abs(par[2]), log = FALSE)+
                    (1-p)*dlnorm(val, par[3], abs(par[4]), log = FALSE))))
}

# Mean 1
m1=2.3; s1=0.5
# Mean 2
m2=1.3; s2=0.1
# proportion of 1 - logit transform
p=0

par<-c(m1, s1, m2, s2, p)

result2<-optim(par, fitnormlnorm, method="BFGS", val=X,
               hessian=FALSE, control=list(trace=1))

lines(seq(from=0, to=5, length=100),
      dnorm(seq(from=0, to=5, length=100),
            result2$par[1], result2$par[2]), col="red")

lines(seq(from=0, to=5, length=100),
      dlnorm(seq(from=0, to=5, length=100),
             result2$par[3], result2$par[4]), col="green")

p <- 1/(1+exp(-result2$par[5]))

paste("Proportion of Gaussian data",  p)

lines(seq(from=0, to=5, length=100),
      p*dnorm(seq(from=0, to=5, length=100),
              result2$par[1], result2$par[2])+
        (1-p)*dlnorm(seq(from=0, to=5, length=100),
                     result2$par[3], result2$par[4]), col="blue")
