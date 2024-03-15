
###########################
# ARMAX forecasting #
# Actually, it is a linear model with an ARMA error.
###########################

## set your working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#dir()
library(forecast)


data = as.matrix(read.csv("ARMAX.csv",header=T))
head(data)

complete.cases(data)
data = na.omit(data)
data2=data[,2:3]

cpi=matrix(as.numeric(data2[,1]))
growth_rate=matrix(as.numeric(data2[,2]))

y=growth_rate
x=cpi
n= nrow(y)        

plot(y~seq(1:n))
plot(x~seq(1:n))

x = diff(log(cpi))
n=nrow(x)
plot(x~seq(1:n))
y = y[2:(n+1)]  #adjust y according to x

######
# model specification
# suppose that this part is done
######


###############################
## Rolling window forecasting, 1-step ahead forecasting 
###############################

nf = 55 # number of forecasts

# Forecasting using ARMA model with mean
ARMA = matrix(0,nf,1)
for (i in 1:nf){
  y2 = y[i:(119+i)] # window size = 156, 120
  x2 = x[i:(119+i)]
  
  y2 = y[(i+1):(119+i)] # adjusting for x_(t-1)
  x2 = x[i:(119+i-1)]

  fit = arima(y2,order = c(1,0,0),include.mean  = TRUE, method = "ML")
  
  coeff=matrix(as.numeric(fit$coef))
  alpha=coeff[1]
  mean =coeff[2]
  
  ARMA[i]= alpha*(y[(119+i)]-mean)+mean
}
ARMA

# Forecasting using ARMAX model With Constant
ARMAX_con = matrix(0,nf,1)
for (i in 1:nf){
  y2 = y[i:(119+i)] # window size = 156
  x2 = x[i:(119+i)]
  
  y2 = y[(i+1):(119+i)] # adjusting to consider x_(t-1)
  x2 = x[i:(119+i-1)]
  
  fit1 = arima(y2,order = c(1,0,0),xreg = x2,include.mean  = TRUE, method = "ML")
  
  coeff=matrix(as.numeric(fit1$coef))
  alpha=coeff[1]
  mean =coeff[2]
  gamma =coeff[3]
  
  ARMAX_con[i]=mean*(1-alpha)+alpha*(y[119+i])+gamma*(x[119+i])-alpha*gamma*x[119+i-1]
}

ARMAX_con

# Forecasting using ARMAX model with Constant(default) using predict command
ARMAX_pred=matrix(0,nf,1)

for(i in 1:nf){ 
  y2 = y[i:(119+i)] # window size = 156
  x2 = x[i:(119+i)]
  
  y2 = y[(i+1):(119+i)] # adjusting for x_(t-1)
  x2 = x[i:(119+i-1)]
  
  fit2 = arima(y2,order = c(1,0,0), xreg = x2, method = "ML")
  ARMAX_pred[i] = predict(fit2,newxreg = x[119+i])$pred
}

ARMAX_pred

# Comparison
comp = cbind(y[121:175],ARMA,ARMAX_con,ARMAX_pred)
comp

yf   = y[121:175]
loss1  = (yf-ARMA)^2
loss2  = (yf-ARMAX_con)^2
loss3  = (yf-ARMAX_pred)^2

MSE  = cbind(mean(loss1),mean(loss2),mean(loss3))
MSE


###########
# DMW test
###########

source("dmwtest.R")
dmw = dmwtest(yf, ARMA, ARMAX_con)
dmw





#####
#####



# Fitting an ARIMA with some coefficients restricted as zero
fit = auto.arima(y)
fit # ARIMA(1,0,1)

M1 = arima(x, order=c(1,0,1), include.mean=TRUE)
M2 = arima(x, c(1,0,5), fixed=c(NA,NA,0,0,0,0,NA))
M3 = arima(x, c(1,0,12), fixed=c(NA,0,0,0,0,0,0,0,0,0,0,0,NA,NA))

# ARMAX model with MA terms
ARMAX_pred_ma=matrix(0,nf,1)
for(i in 1:nf){ 
  y2 = y[i:(155+i)] # window size = 156
  x2 = x[i:(155+i)]
  
  y2 = y[(i+1):(155+i)] # adjusting for x_(t-1)
  x2 = x[i:(155+i-1)]
  
  fit3 = arima(y2,order = c(1,0,1),xreg = x2, method = "ML")
  ARMAX_pred_ma[i] = predict(fit3,newxreg = x[155+i])$pred
}
ARMAX_pred_ma
loss4 = (yf-ARMAX_pred_ma)^2
MSE = cbind(MSE,mean(loss4))
MSE



