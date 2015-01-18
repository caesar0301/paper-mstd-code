library(RandomFields)

RFoptions(seed=0)
model <- RMnsst(phi=RMgauss(), psi=RMfbm(alpha=1), delta=2)
x <- seq(0, 10, if (interactive()) 0.25 else 1)
plot(model, dim=2)
plot(RFsimulate(model, x=x, y=x))
