setwd('C:/Users/user/Documents')
getwd()

library(readxl)
library(urca)

data <- read.csv("DataKoreaFrom200408To202306WOtcode.csv")
#Default Premium = DBAA - DAAA(daily credit)
head(data)

data1 = data[-1,]
data1
attach(data1)

y1 = VKOSPI
y1 = na.omit(y1) # if necessary
plot(y1, type = 'l')

y2 = log(y1)
plot(y2, type = 'l')

y3 = diff(y2)
plot(y3, type = 'l')

y4 = diff(y3)
plot(y4, type = 'l')



ar1=arima(y2, order=c(1,0,0))
ar1

ar2=arima(y2, order=c(1,0,0), xreg = 1:length(y2))
ar2

# Model with intercept
adf1=ur.df(y1, type="drift")
summary(adf1)

adf2=ur.df(y1, type="trend")
summary(adf2)

# Model with intercept
kpss1=ur.kpss(y1, type="mu", lags = "long")
kpss1
summary(kpss1)

# Model with intercept and trend
kpss2=ur.kpss(y1, type="tau", lags = "long" )
kpss2
summary(kpss2)
