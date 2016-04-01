library(CompRandFld)
library(fields)

################################################################
########### Examples of Spatio-temporal kriging  ###############
################################################################

# Define the spatial-coordinates of the points:
x <- runif(15, 0, 1)
y <- runif(15, 0, 1)
coords<-cbind(x,y)
times<-1:7

#  Define the times to predict
times_to_pred<-8:10

# Define model correlation and associated parameters
corrmodel<-"exp_exp"
param<-list(nugget=0,mean=1,scale_s=1,scale_t=2,sill=2)

# Simulation of the space time Gaussian random field:
set.seed(31)
data<-RFsim(coordx=coords,coordt=times,corrmodel=corrmodel,
           param=param)$data

# Maximum likelihood fitting of the space time random field:
fit <- FitComposite(data, coordx=coords, coordt=times,
                    corrmodel=corrmodel, likelihood='Full',
                    type='Standard',fixed=list(mean=1,
                    nugget=0))

################################################################
###
### Example 3. Temporal ordinary kriging of one location site for
### a Gaussian random fields with estimated double exponential
### correlation.
###
################################################################

param<-as.list(c(fit$param,fit$fixed))
pr<-Kri(loc=c(.5,.5),time=times_to_pred,coordx=coords,coordt=times,
        corrmodel=corrmodel,param=param,data=data,type_krig="ordinary")

pr$pred       #  prediction
pr$varpred    #  prediction variance


################################################################
###
### Example 4. Spatio temporal simple kriging of n locations
### sites and m temporal instants for a Gaussian random fields
### with estimated double exponential correlation.
###
###############################################################

# locations to predict
xx<-seq(0,1,0.02)
loc_to_pred<-as.matrix(expand.grid(xx,xx))

pr<-Kri(loc=loc_to_pred,time=times_to_pred,coordx=coords,coordt=times,
       corrmodel=corrmodel, param=param,data=data)

par(mfrow=c(3,2))

colour <- rainbow(100)

for(i in 1:3){
image.plot(xx, xx, matrix(pr$pred[i,],ncol=length(xx)),col=colour,
           main = paste("Kriging Time=" , i),ylab="")
image.plot(xx, xx, matrix(pr$varpred[i,],ncol=length(xx)),col=colour,
           main = paste("Std error Time=" , i),ylab="")
}