##### FORECASTING GARCH type volatility #####

## GARCH, GJR-GARCH, GARCH-X model forecasting ##
#ARMAX rolling window (specify x,y,z & change the p,q adequately)

# packages for GARCH model estimation
install.packages('fGarch')
library(fGarch)
install.packages('rugarch')
library(rugarch)
library(fBasics)


# set directory
getwd()
setwd("C:/HAN/Teaching/1TimeSeries/R")
dir() 

# load data
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


###########################
  # model 1: GARCH(1,1) #
###########################

nf = 52                                  # number of forecast: 52
VF_garch = matrix(NA,nf,1)                # vector for GARCH(1,1) forecasts

for(i in 1:nf){ 
  
  sp2 = sp[(2200+i):(2200+1007+i)]         # window size:1008
  
  # estimation for i'th window
  fit1=garchFit(~garch(1,1),include.mean=T,data=sp2,trace=F,cond.dist="QMLE") 
  
  ut_2=(sp2-coef(fit1)[1])^2               # (y-mu)^2
  h_t=fit1@h.t                             # conditional variance estimates

  omega=coef(fit1)[2]
  alpha=coef(fit1)[3]
  beta=coef(fit1)[4]
  
  # calculate one-step ahead forecast
  VF_garch[i]=omega+alpha*ut_2[1008]+beta*h_t[1008] 
}

VF_garch


# GARCH model fitting with mean model ARMA(p,q) 
fit1=garchFit(~garch(1,1),mean.model=list(armaOrder=c(p,q),include.mean=T),include.mean=T,data=sp2,trace=F,cond.dist="QMLE")

###########################
# model 2: GJR-GARCH(1,1) #
###########################

VF_GJRgarch = matrix(NA,nf,1)              # vector for GJR-GARCH(1,1) forecasts
gjr.spec = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(0,0),include.mean=T))


for(i in 1:nf){ 
  
  sp2 = sp[(2200+i):(2200+1007+i)]        # window size:1008
  
  # estimation for i'th window
  fit2 = ugarchfit(gjr.spec,sp2,solver="hybrid")  

  ut = sp2-coef(fit2)[1]                 # y-mu
  ut_2= ut^2                             # (y-mu)^2
  I<-ifelse(ut[1008]<0,1,0)              # 1 if u_t<0, 0 otherwise
  
  h_t=matrix(sigma(fit2)^2)      # conditional variance estimates  # h_t=matrix(fitted(fit2)) is fitted value of mean               
  omega=as.numeric(coef(fit2)[2])
  alpha=as.numeric(coef(fit2)[3])
  beta=as.numeric(coef(fit2)[4])
  gamma=as.numeric(coef(fit2)[5])
  
  # calculate one-step ahead forecast
  VF_GJRgarch[i]=omega+(alpha+gamma*I)*ut_2[1008]+beta*h_t[1008] 
} 


VF_GJRgarch



#########################################
# model 3: GARCH-X with realized kernel #
#########################################

VF_garchX = matrix(NA,nf,1)              # vector for GARCH-X forecasts
garchx.rk=ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),external.regressors=lagrk),
                     mean.model=list(armaOrder=c(0,0),include.mean=T),distribution.model="norm")

for(i in 1:nf){ 

   sp2 = sp[(2200+i):(2200+1007+i)]      # window size:1008
   rk2 = rk[(2200+i):(2200+1007+i)]

   sp2=matrix(sp2)
   
   ylast = sp2[1008]
   xlast = rk2[1008]
  
  # matching y_t with x_(t-1)
   sp3 = sp2[-1,]
   lagrk=matrix(tslag(rk2,k=1,trim=T))   # generate lagged covariate
  

  # estimation for i'th window
  fit3 = ugarchfit(spec=garchx.rk,sp3,solver="hybrid")

  ut_2=(ylast-coef(fit3)[1])^2            # (y-mu)^2
  h_t=matrix(sigma(fit3)^2)               # conditional variance estimates
  rk_t=as.numeric(xlast)

  omega=as.numeric(coef(fit3)[2])
  alpha=as.numeric(coef(fit3)[3])
  beta=as.numeric(coef(fit3)[4])
  xtreg=as.numeric(coef(fit3)[5])
  
  # calculate one-step ahead forecast
  VF_garchX[i]=omega+alpha*ut_2+beta*h_t[1007]+xtreg*rk_t 
}
  
  
VF_garchX



