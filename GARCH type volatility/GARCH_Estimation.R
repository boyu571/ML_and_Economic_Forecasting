## GARCH, GJR-GARCH, GARCH-X model estimation ##

getwd()
setwd("D:/2Teaching/00ForecastingMachineLearning/R-codes/GARCHtype")
dir() 

install.packages("readxl")
install.packages("fBasics")

library("readxl")
library("fBasics")

data=read.csv('S_Pdata.csv',header=T)
head(data)
tail(data)

date=matrix(data$date)
vix=matrix(data$vix)
sp=matrix(data$SPX_r)*100       # percentage form
rk=matrix(data$SPX_rk)*10000    

head(vix)
x=vix^2/252
head(x)

# descriptive statistics for the return series
summary(sp)
kurtosis(sp)
skewness(sp)

# basic plots
ts.plot(sp)


# GARCH(1,1) #

install.packages('fGarch')

library(fGarch)

?garchFit
fit1=garchFit(~garch(1,1),include.mean=T,data=sp,trace=F,cond.dist="QMLE") 
fit1
summary(fit1)
ts.plot(fit1@h.t)

omega=coef(fit1)[2]
alpha=coef(fit1)[3]
beta=coef(fit1)[4]



# GJR-GARCH(1,1) #
install.packages('rugarch')
library(rugarch)

?ugarchspec
gjr.spec = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(0,0),include.mean=T))
?ugarchfit
fit2 = ugarchfit(gjr.spec,sp)
fit2

coef(fit2)
omega=matrix(coef(fit2)[2])
alpha=matrix(coef(fit2)[3])
beta=matrix(coef(fit2)[4])
gamma=matrix(coef(fit2)[5])


# GARCH-X model with vix#
?ugarchspec

# generate lagged variable
sp1=sp[-1,]
sp1=matrix(sp1)
lagx=tslag(x,k=1,trim=TRUE)
lagx=matrix(lagx)


garchx.vix=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=lagx),
                      mean.model=list(armaOrder=c(0,0),include.mean=T),distribution.model="norm")
fit3 = ugarchfit(garchx.vix,sp1,solver="nloptr")
fit3
coef(fit3)



# GARCH-X with realized kernel #


lagrk=matrix(tslag(rk,k=1,trim=T))

garchx.rk=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=lagrk),
                     mean.model=list(armaOrder=c(0,0),include.mean=T),distribution.model="norm")
fit4 = ugarchfit(spec=garchx.rk,sp1, solver = "nloptr")
fit4
coef(fit4)

fit1
fit2
fit3
fit4




