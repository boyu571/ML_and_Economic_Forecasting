
### Unit Root Test ###

# ADF, PP and KPSS Tests using "urca" package

getwd()
setwd("C:\\Users\\boyu5\\boyu571_github\\00_SKKU_24_Sprig\\MLEF\\ML_and_Economic_Forecasting\\main")
dir()

install.packages("urca")
install.packages("readxl")
library(urca)
library(readxl)

data = read.csv("2016-01.csv",header=T)

data1 = data[-1,]

attach(data1)


# Find tcode of each variable: 1) plot, 2) unit root tests

# 첫번째 스케일 확인(100이상이 있는지)
# 두번째 unitroot 확인

# Industrial Production Index
y1 = INDPRO
y1 = na.omit(y1) # if necessary
y1 = diff(y1)
plot(y1, type = 'l')

### 0. Check Estimates of AR Coefficients 

ar1=arima(y1, order=c(1,0,0))
ar1

ar2=arima(y1, order=c(1,0,0), xreg = 1:length(y1))
ar2
# 2번

# Unemployment Rate
y2 = UNRATE
y2 = diff(y2)
plot(y2, type = 'l')
# 2번
### 0. Check Estimates of AR Coefficients 

ar1=arima(y2, order=c(1,0,0))
ar1

ar2=arima(y2, order=c(1,0,0), xreg = 1:length(y2))
ar2

# CPI: All Items
y3 = CPIAUCSL
plot(y3, type = 'l')

y3 = log(y3)

n = nrow(as.matrix(y3))

ar2=arima(y3, order=c(1,0,0), xreg = 1:length(y3))
ar2
trend = seq(1:(n=1))
lm(y3[2:n]~y3[1:(n-1)]+trend)

y33 = diff(y3)
plot(y33, type = 'l')




# Housing Starts: Total New Privately Owned
y4 = HOUST
plot(y4, type = 'l')

y4 = log(y4)

ar1=arima(y4, order=c(1,0,0))
ar1

daily_adf1=ur.df(y4, type="drift")
summary(daily_adf1)

y44 = diff(y4)
plot(y44, type = 'l')

ar1=arima(y44, order=c(1,0,0))
ar1

daily_adf1=ur.df(y4, type="drift")
summary(daily_adf1)


# 10-year Treasury C Minus FEDFUNDS
y5 = T10YFFM
plot(y5, type = 'l')

ar1=arima(y5, order=c(1,0,0))
ar1

daily_adf1=ur.df(y5, type="drift")
summary(daily_adf1)

