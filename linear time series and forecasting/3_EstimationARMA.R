##### Estimation of ARMA models #####


# 1) simulate ARIMA(p,d,q) models

# ARMA(1,1)
sim.arma=arima.sim(list(order=c(1,0,1),ma=0.5,ar=0.5),n=500)
plot(sim.arma)

# AR(2)
sim.ar=arima.sim(list(order=c(2,0,0),ar=c(0.4,0.2)),n=500)
plot(sim.ar)
# MA(2)
sim.ma=arima.sim(list(ma=c(0.6,-0.4)),n=500)
plot(sim.ma)



# 2) estimation

fit1=arima(sim.ar,order=c(0,0,1), method="ML")
fit1
fit1$coef
fit1$sigma
fit1$aic; fit1$loglik
tsdiag(fit1)

AIC(fit1) #AIC
BIC(fit1) #BIC
?BIC


uhat<-fit1$resid

#Ljung-Box Q-test
Box.test(uhat,lag=10,type='Ljung') #reject H0 if p-value<0.05
?Box.test


fit2=arima(sim.ar,order=c(2,0,0), method="ML")
fit2
fit2$aic; fit2$loglik
tsdiag(fit2)

uhat2<-fit2$resid

#Ljung-Box Q-test
Box.test(uhat2,lag=10,type='Ljung') #reject H0 if p-value<0.05



?nottem
nottem
nottem=ts(nottem,start=1920,freq=12)
summary(nottem)
plot(stl(nottem,s.window="periodic"))

# find AR order
m1=ar(nottem,method="mle")
?ar
m1$order
m1

ar1=arima(nottem,order=c(1,0,0),method="ML")
ar1
tsdiag(ar1)

ar2=arima(nottem,order=c(2,0,0),method="ML")
ar2
tsdiag(ar2)

ar12=arima(nottem,order=c(12,0,0),method="ML")
ar12
tsdiag(ar12)

ma3=arima(nottem,order=c(0,0,3),method="ML")
ma3

arma21=arima(nottem,order=c(2,0,1),method="ML")
arma21

likelihood<-cbind(ar1$loglik, ar2$loglik, ar12$loglik, ma3$loglik, arma21$loglik)
criterion <-cbind(ar1$aic, ar2$aic, ar12$aic, ma3$aic, arma21$aic)
model.comparison = rbind(likelihood, criterion)
model.comparison

# model checking
uhat12<-ar12$resid
Box.test(uhat12,lag=10,type='Ljung')


# 3) estimation using packages

install.packages("forecast")
library(forecast)

?auto.arima
best=auto.arima(nottem, seasonal=FALSE)
best




# forecast

?forecast.Arima
forc.arma=forecast(best, h=10)
forc.arma
