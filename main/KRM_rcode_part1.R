library(rstudioapi)
library(devtools)  
library(HDeconometrics)
dir()

data = read.csv("project2024/DataKoreaFrom200408To202306.csv")   

tcode = data[1,]  # first element: Transformation code
tcode

### Transformation 

data = data[-1,]  # Transfrom 라인 제거 

tdata = data[-(1:2),]  #최초 2 observations 제거(이차 차분 고려)

head(tdata[,1:5])



for (i in 2:94){
  
  if(tcode[i] == 1){
    tdata[,i] <- data[-(1:2),i]
  } # no transformation  
  
  if(tcode[i] == 2){
    tdata[,i] <- diff(data[-1,i])
  } # 1차 차분
  
  # tdoce ==3(2차 차분)에 해당하는 데이터는 없음
  
  if(tcode[i] == 4){
    tdata[,i] <- log(data[-(1:2),i])
  } # log
  
  if(tcode[i] == 5){
    tdata[,i] <- diff(log(data[-1,i]))
  } # log differencing
  
  if(tcode[i] == 6){
    tdata[,i] <- diff(diff(log(data[,i])))
  } # log취한 뒤 2차 차분
  
  if(tcode[i] == 7){
    tdata[,i] <- diff(data[-1,i]/data[1:(nrow(data)-1),i])
  } # 증가율의 1차 차분
}

#########
tdata
fdata = tdata
complete.cases(fdata)  # no missing values

# fdata 행렬에서 'CPI' 열의 인덱스 찾기
cpi_index <- which(colnames(fdata) == "CPI")
cpi_index # 65
fdata

Y = cbind(fdata[,65],fdata[,c(-1,-65)]) 
mode(Y)

Y= as.matrix(Y)
mode(Y)

Y
# 첫 번째 열의 이름을 'CPI'로 변경
colnames(Y)[1] <- "CPI"
Y

# Number of Forecasts
npred=105
# 225-120

## Random Walk Model ##
source("main/functions/func-rw.R")

rw1=rw.rolling.window(Y,npred,1,1)
rw3=rw.rolling.window(Y,npred,1,3)
rw6=rw.rolling.window(Y,npred,1,6)
rw12=rw.rolling.window(Y,npred,1,12)

## AR(4) Model ##
source("main/functions/func-ar.R")

ar1=ar.rolling.window(Y,npred,1,1,type="fixed")
ar3=ar.rolling.window(Y,npred,1,3,type="fixed")
ar6=ar.rolling.window(Y,npred,1,6,type="fixed")
ar12=ar.rolling.window(Y,npred,1,12,type="fixed")


## LASSO ##
source("main/functions/func-lasso.R")
alpha=1

lasso1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
lasso3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
lasso6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
lasso12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")

## Adaptive LASSO ##

adalasso1=lasso.rolling.window(Y,npred,1,1,alpha,type="adalasso")
adalasso3=lasso.rolling.window(Y,npred,1,3,alpha,type="adalasso")
adalasso6=lasso.rolling.window(Y,npred,1,6,alpha,type="adalasso")
adalasso12=lasso.rolling.window(Y,npred,1,12,alpha,type="adalasso")


## Elastic Net ##

alpha=0.5

elasticnet1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
elasticnet3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
elasticnet6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
elasticnet12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")


## Adaptive Elastic Net ##

adaelasticnet1=lasso.rolling.window(Y,npred,1,1,alpha,type="adalasso")
adaelasticnet3=lasso.rolling.window(Y,npred,1,3,alpha,type="adalasso")
adaelasticnet6=lasso.rolling.window(Y,npred,1,6,alpha,type="adalasso")
adaelasticnet12=lasso.rolling.window(Y,npred,1,12,alpha,type="adalasso")

## Random Forest (RF) ##
source("main/functions/func-rf.R")
library(randomForest)

rf1=rf.rolling.window(Y,npred,1,1)
rf3=rf.rolling.window(Y,npred,1,3)
rf6=rf.rolling.window(Y,npred,1,6)
rf12=rf.rolling.window(Y,npred,1,12)


######################## Neural Networks(Deep Learning) ##########################
source("main/functions/func-nn.R")
library(dplyr)
library(keras)

library(h2o)
h2o.init()

nn1=nn.rolling.window(Y,npred,1,1)
nn3=nn.rolling.window(Y,npred,1,3)
nn6=nn.rolling.window(Y,npred,1,6)
nn12=nn.rolling.window(Y,npred,1,12)



######################## Xgboost ##########################
# Import the Boosting function
source('main/functions/func-xgb.R')
library(HDeconometrics)
library(xgboost)

xgb1=xgb.rolling.window(Y,npred,1,1)
xgb3=xgb.rolling.window(Y,npred,1,3)
xgb6=xgb.rolling.window(Y,npred,1,6)
xgb12=xgb.rolling.window(Y,npred,1,12)


#install.packages("Boruta")
library(Boruta)
library(randomForest)

Y2 = Y                

# Example for lag = 12 setting
lag = 12  # lag = horizon

aux = embed(Y2,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y2)*lag))]


set.seed(42)
boruta_12 <- Boruta(X, y, maxRuns = 100)

plot = plot(boruta_12)
plot

attstats = attStats(boruta_12)
attstats

#write.csv(attstats,"Boruta_12.csv",sep=";",row.names = FALSE, col.names = FALSE)

order = order(attstats$meanImp, decreasing = T)

order
## Cross Validation for Optimal Number of Variables # (Up to 70 Variables)

Errors = rep(NA,70)          


for (i in 2:70){
  
  selected = order[1:i]
  
  model=randomForest(X[,selected], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:70), Errors, xlab="# of Variables", ylab="Fitted Squared Error")

Errors1 = Errors


varOrder = order(attstats$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors1)                                    # Optimal Number of Variables 
selected12 = varOrder[1:which.min(Errors1)]             # The Set of Optimal Number of Variables
#selected = varOrder[1:16]                 # 꺾이는 부분. 16개까지만 사용 


# Rolling Window with Selected Variables

source("main/functions/func-rf_selected2022.R")

BS_RF1 = rf.rolling.window(Y2,npred,1,1,selected1)
BS_RF3 = rf.rolling.window(Y2,npred,1,3,selected3)
BS_RF6 = rf.rolling.window(Y2,npred,1,6,selected6)
BS_RF12 = rf.rolling.window(Y2,npred,1,12,selected12)


# LSTM Function  
# ==================================================================
# Install packages and Recall Library

#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)

install.packages("tensorflow")  #관리자 권한으로 실행 
library(tensorflow)
install_tensorflow()

library(keras)
library(reticulate)
use_condaenv("r-reticulate", required = TRUE)
print(py_config())

# ==================================================================
# Normalization

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# Inverse Normalization 
denormalize <- function(x, minval, maxval) {
  x*(maxval-minval) + minval
}

indice = 1
horizon = 1
lag = horizon

# FINAL CHECK COMPLETE

source("main/functions/func-lstm.R")

lstm_1 <- mul.lstm.rolling.window(Y,npred,1,1)  
lstm_1$errors 

lstm_3 <- mul.lstm.rolling.window(Y,npred,1,3)  
lstm_3$errors 

lstm_6 <- mul.lstm.rolling.window(Y,npred,1,6)  
lstm_6$errors 

lstm_12 <- mul.lstm.rolling.window(Y,npred,1,12)  
lstm_12$errors 

# saving entire worksapce
save.image("강려명_part1.RData")    



library(stringr)
library(openxlsx)

# load("강려명_part1.RData") 

stack <- NULL

for (i in 1:2) {
  #1.random walk  
  rw = cbind(rw1$errors[i],rw3$errors[i],rw6$errors[i],rw12$errors[i])
  
  #2. ar
  ar = cbind(ar1$errors[i],ar3$errors[i],ar6$errors[i],ar12$errors[i])

  #3. lasso
  lasso = cbind(lasso1$errors[i],lasso3$errors[i],lasso6$errors[i],lasso12$errors[i])
  
  #4. adaptive lasso
  adalasso = cbind(adalasso1$errors[i],adalasso3$errors[i],adalasso6$errors[i],adalasso12$errors[i])
  
  #5. elastic net
  elasticnet = cbind(elasticnet1$errors[i],elasticnet3$errors[i],elasticnet6$errors[i],elasticnet12$errors[i])
  
  #6. adaptive elastic net 
  adaelasticnet = cbind(adaelasticnet1$errors[i],adaelasticnet3$errors[i],adaelasticnet6$errors[i],adaelasticnet12$errors[i])

  #7. random forest (RF)
  rf = cbind(rf1$errors[i],rf3$errors[i],rf6$errors[i],rf12$errors[i])
  
  #8. NN 
  nn = cbind(nn1$errors[i], nn3$errors[i], nn6$errors[i], nn12$errors[i])
  
  #9. XGboost
  xgb = cbind(xgb1$errors[i], xgb3$errors[i], xgb6$errors[i], xgb12$errors[i])
  
  #10. BS_RF
  bs_rf = cbind(BS_RF1$errors[i], BS_RF3$errors[i], BS_RF6$errors[i], BS_RF12$errors[i])
  
  #11. LSTM
  lstm = cbind(lstm_1$errors[i], lstm_3$errors[i], lstm_6$errors[i], lstm_12$errors[i])
  
  
  nn.rmse = cbind(nn1$errors[1], nn3$errors[1], nn6$errors[1], nn9$errors[1], nn12$errors[1])
  
  
  
  df <-  rbind(rw, ar, ridge, lasso, adalasso, elasticnet, adaelasticnet,  csr, tfact, rf) %>% as.data.frame()
  
  
  #df = round(df, digit=4)
  
  stack <- rbind(stack, df)
}

colnames(stack) <- seq(1,12)

nModel = 10 # number of model
error_rmse <- stack[1:nModel,]
error_mae <- stack[(nModel+1):(nModel*2),]

rownames(error_rmse) <- c('rw', 'ar','ridge', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet',  'csr',  'tfact', 'rf')
rownames(error_mae) <- c('rw', 'ar','ridge', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet',  'csr', 'tfact', 'rf')

sheets <- list("error_rmse" = error_rmse, "error_mae" = error_mae)
write.xlsx(sheets, file = "errortable.xlsx", rowNames=TRUE)
#####
