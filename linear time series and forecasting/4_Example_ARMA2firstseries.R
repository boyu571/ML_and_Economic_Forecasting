## set your working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#dir()


install.packages("readxl")
# Use installed package
library("readxl")

# import data from a xls file without header
dataxls = as.matrix(read_excel("ARMA2.xls"))
?read_excel
data = as.matrix(read_excel("ARMA2.xls", col_names=FALSE))
head(data)
tail(data)

complete.cases(data)
data = na.omit(data)
n    = nrow(data)

y = matrix(data[,1])
  
ar1=arima(y,order=c(1,0,0), method="ML")
ar1  

ar2=arima(y,order=c(2,0,0), method="ML")
ar2 

# model checking
uhat<-ar2$resid
Box.test(uhat,lag=10,type='Ljung')



ma1=arima(y,order=c(0,0,1), method="ML")
ma1  

ma2 =arima(y,order=c(0,0,2), method="ML")
ma2  

arma11=arima(y,order=c(1,0,1), method="ML")
arma11  

arma12=arima(y,order=c(1,0,2), method="ML")
arma12  


criterion <-cbind(ar1$aic, ar2$aic, ma1$aic, ma2$aic, arma11$aic, arma12$aic)
criterion
  
  
  
  