# fit the paramters and test the goodness-of-fit of
# mixture of two distributions
library(mixtools)

#######################################
## Two normal distributions
#######################################
# CDF of mixture of two normals
pmnorm <- function(x, mu, sigma, pmix) {
  pmix[1]*pnorm(x,mu[1],sigma[1]) + (1-pmix[1])*pnorm(x,mu[2],sigma[2])
}

# A single-run estimation of ks statistics
x <- c(rnorm(50), rnorm(50,2))
hist(x, 100, freq=FALSE)
foo <- normalmixEM(x)
test <- ks.test(x, pmnorm, mu=foo$mu, sigma=foo$sigma, pmix=foo$lambda)
test

# Bootstrap estimation of ks statistic distribution
N <- length(x)
ks.boot <- rep(0,100)
for (i in 1:100) {
  z <- rbinom(N, 1, foo$lambda[1])
  x.b <- z*rnorm(N, foo$mu[1], foo$sigma[1]) + (1-z)*rnorm(N, foo$mu[2], foo$sigma[2])
  foo.b <- normalmixEM(x.b, maxit=10000, lambda=foo$lambda, mu=foo$mu, sigma=foo$sigma)
  ks.boot[i] <- ks.test(x.b, pmnorm, mu=foo.b$mu, sigma=foo.b$sigma, pmix=foo.b$lambda)$statistic
}

mean(test$statistic <= ks.boot)


#######################################
## Two log-normal distributions
#######################################
pmlnorm <- function(x, meanlog, sdlog, pmix) {
  pmix[1]*plnorm(x,meanlog[1],sdlog[1]) + (1-pmix[1])*plnorm(x,meanlog[2],sdlog[2])
}

x <- c(rlnorm(100, 2, 0.5), rlnorm(100, 4, 0.5))
mixmdl = normalmixEM(log(x), k=2)
plot(mixmdl, which=2)
lines(density(log(x)), lty=2,lwd=2)

test2 <- ks.test(x, pmlnorm, meanlog = mixmdl$mu, sdlog = mixmdl$sigma,
                pmix = mixmdl$lambda)
test2