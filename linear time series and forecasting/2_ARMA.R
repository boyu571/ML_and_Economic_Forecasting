##### AR, MA, ARMA, ARIMA #####
# ACF, PACF

# simulate ARIMA(p,d,q) processes
??arima.sim

# AR(1)
ar1=arima.sim(list(order=c(1,0,0),ar=c(0.7)),n=500)
#ar2=arima.sim(list(ar=c(0.7)),n=500)

plot(ar1)

# Autocorrelation
acf(ar1,main="ACF of AR(1) process")
?acf
acf(ar1,lag.max = 10,main="ACF of AR(1) process")


# To get the values of acf
acf(ar1,plot=F)
acf1 = acf(ar1,plot=F)
acfTable = data.frame(acf1$lag, acf1$acf)
acfMat = as.matrix(acf1$acf)


# Plotting without lag 0
acf(ar1,lag.max = 10)       # ACF of lag 0 is included
plot(acf(ar1,plot=F)[1:10]) # plot without lag 0

# Partial Autocorrelation
pacf(ar1,lag.max = 10, main="PACF of AR(1) process")
?pacf

# MA(1)
ma1=arima.sim(list(ma=c(0.7)),n=500)
plot(ma1)
acf(ma1,main="ACF of MA(1) process")
pacf(ma1,main="PACF of MA(1) process")


par(mfcol=c(2,2))    # plot multiple graphs(column-wise)

acf(ar1,lag.max=20,main="ACF of AR(1) process")
acf(ma1,lag.max=20,main="ACF of MA(1) process")
pacf(ar1,lag.max=20,main="PACF of AR(1) process")
pacf(ma1,lag.max=20,main="PACF of MA(1) process")


# plots without lag 0
par(mfcol=c(2,2))    

plot(acf(ar1,plot=F)[1:20], main="ACF of AR(1) process")
plot(acf(ma1,plot=F)[1:20], main="ACF of MA(1) process")
pacf(ar1,lag.max=20, main="PACF of AR(1) process")
pacf(ma1,lag.max=20, main="PACF of MA(1) process")


## More ARMA processes

# ARMA(1,1)
sim.arma=arima.sim(list(order=c(1,0,1),ma=0.5,ar=0.5),n=500)
plot(sim.arma)
acf(sim.arma)
pacf(sim.arma)

# AR(2)
sim.ar=arima.sim(list(order=c(2,0,0),ar=c(0.4,0.2)),n=500)
plot(sim.ar)
acf(sim.ar)
pacf(sim.ar)
# MA(2)
sim.ma=arima.sim(list(ma=c(0.6,-0.4)),n=500)
plot(sim.ma)
acf(sim.ma)
pacf(sim.ma)

par(mfcol=c(2,2))    # plot multiple graphs(column-wise)

acf(sim.ar,main="ACF of AR(2) process")
acf(sim.ma,main="ACF of MA(2) process")
pacf(sim.ar,main="PACF of AR(2) process")
pacf(sim.ma,main="PACF of MA(2) process")


## (stationary) ARMA and ARIMA

par(mfrow=c(3,2))    # plot multiple graphs(row-wise)
# ARMA(1,1)
sim.arma=arima.sim(list(order=c(1,0,1),ma=0.5,ar=0.5),n=500)
plot(sim.arma)
acf(sim.arma)

# ARIMA(0,1,0), unit root process
sim.arima=arima.sim(list(order=c(0,1,0)),n=500)
#sim.arima=arima.sim(list(order=c(0,1,0),ar=0.7),n=200)

plot(sim.arima)
acf(sim.arima)

# first difference of the unit root process
f.diff=diff(sim.arima,differences = 1)
plot(f.diff)
acf(f.diff)



# unit root process with drift
n = 500
y=matrix(0,n,1)
eps = sim.arma
for (i in 1:(n-1)){
  y[i+1]=0.7+y[i]+eps[i+1]
}
plot(y)
acf(y)

y.diff=diff(y,differences = 1)
plot(y.diff)
acf(y.diff)

