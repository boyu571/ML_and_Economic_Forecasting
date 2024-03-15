##### Time Series Analysis #####

## set your working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#dir()

# ARMAX model
# use packages to find the best model

data = as.matrix(read.table("m-gmsp5008.txt",header=T))
head(data)

gm = cbind(data[,2])   
sp = cbind(data[,3])
n = nrow(gm)

plot(gm~seq(1:n))
plot(sp~seq(1:n))

ar1=arima(gm, order=c(1,0,0))
ar1

# use first differenced data if it is a unit root process
# gm=diff(gm); sp=diff(sp)
# n=nrow(gm)



install.packages("forecast")
library(forecast)

#auto.arima: zero mean
arma=auto.arima(gm)
arma;

mean(gm)

#include mean: (gm-mean) is an ARMA(4,1) process
fit = arima(gm,order = c(4,0,1),include.mean  = TRUE, method = "ML")
fit

#armax includes mean: 
armax=auto.arima(gm, xreg=sp)
armax

# ARIMA(5,1,0)? Does it make sense?

#Actually, this is a linear model (intercept and xreg) with an ARMA error

fit2 = arima(gm,order = c(5,0,0),xreg = sp,include.mean  = TRUE, method = "ML")
fit2
# Note that this model is
# gm_t = b_0 + b_1*sp_t + u_t where u_t is AR(5) 


# Rolling window forecast

nf = 46   # number of forecasts

f1 = matrix(0,nf,1)
f2 = matrix(0,nf,1)


for (i in 1:nf){
  gm2 = gm[i:(660+i)]
  sp2 = sp[i:(660+i)]

  fit1=arima(gm2,order=c(1,0,2), method="ML")
  fit2=arima(gm2,order=c(5,0,0), method="ML")

  f1[i]=forecast(fit1, h=1)$mean
  f2[i]=forecast(fit2, h=1)$mean

}

# When there is a problem in convergence, you can increase the maximum iteration number

for (i in 1:nf){
  gm2 = gm[i:(660+i)]
  sp2 = sp[i:(660+i)]

  fit1=arima(gm2,order=c(1,0,2), method="ML",optim.control=list(maxit=1000))
  fit2=arima(gm2,order=c(5,0,0), method="ML")

  f1[i]=forecast(fit1, h=1)$mean
  f2[i]=forecast(fit2, h=1)$mean

}

cbind(f1,f2)
