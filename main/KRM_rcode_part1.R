library(rstudioapi)
library(devtools)  
library(HDeconometrics)

dir()

load("강려명_part1.RData") 
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
library(reshape2)

install.packages("tensorflow")  #관리자 권한으로 실행 
library(tensorflow)
# install_tensorflow()

install.packages("keras")

library(keras)
library(reticulate)
tf <- reticulate::import("tensorflow")
print(tf$`__version__`)


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


source("main/functions/func-lstm.R")

lstm_1 <- mul.lstm.rolling.window(Y,npred,1,1)  
lstm_1$errors 

lstm_3 <- mul.lstm.rolling.window(Y,npred,1,3)  
lstm_3$errors 

lstm_6 <- mul.lstm.rolling.window(Y,npred,1,6)  
lstm_6$errors 

lstm_12 <- mul.lstm.rolling.window(Y,npred,1,12)  
lstm_12$errors 

  



library(stringr)
library(openxlsx)



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
  #lstm = cbind(lstm_1$errors[i], lstm_3$errors[i], lstm_6$errors[i], lstm_12$errors[i])
  
  df <-  rbind(rw, ar, lasso, adalasso, elasticnet, adaelasticnet,  rf, nn, xgb, bs_rf) %>% as.data.frame()
  
  
  #df = round(df, digit=4)
  
  stack <- rbind(stack, df)
}
stack
colnames(stack) <- c("1", "3", "6", "12")

nModel = 10 # number of model
error_rmse <- stack[1:nModel,]
error_mae <- stack[(nModel+1):(nModel*2),]

rownames(error_rmse) <- c('rw', 'ar','lasso', 'adalasso', 'elasticnet', 'adaelasticnet', 'rf', 'nn', 'xgb', 'bs_rf')
rownames(error_mae) <- c('rw', 'ar','lasso', 'adalasso', 'elasticnet', 'adaelasticnet', 'rf', 'nn', 'xgb', 'bs_rf')

sheets <- list("error_rmse" = error_rmse, "error_mae" = error_mae)
write.xlsx(sheets, file = "errortable.xlsx", rowNames=TRUE)
#####

library(sandwich)
library(MCS)

source("functions/gwtest.R")

#############################################################
### = Giacomini-White Test for Equal Predictive Ability = ###
#############################################################


rw_pred = matrix(NA,npred,4)
rw_pred[,1] = rw1$pred
rw_pred[-(1:2),2] = rw3$pred
rw_pred[-(1:5),3] = rw6$pred
rw_pred[-(1:11),4] = rw12$pred

ar_pred = matrix(NA,npred,4)
ar_pred[,1] = ar1$pred
ar_pred[-(1:2),2] = ar3$pred
ar_pred[-(1:5),3] = ar6$pred
ar_pred[-(1:11),4] = ar12$pred


lasso_pred = matrix(NA,npred,4)
lasso_pred[,1] = lasso1$pred
lasso_pred[-(1:2),2] = lasso3$pred
lasso_pred[-(1:5),3] = lasso6$pred
lasso_pred[-(1:11),4] = lasso12$pred

adalasso_pred = matrix(NA,npred,4)
adalasso_pred[,1] = adalasso1$pred
adalasso_pred[-(1:2),2] = adalasso3$pred
adalasso_pred[-(1:5),3] = adalasso6$pred
adalasso_pred[-(1:11),4] = adalasso12$pred

elasticnet_pred = matrix(NA,npred,4)
elasticnet_pred[,1] = elasticnet1$pred
elasticnet_pred[-(1:2),2] = elasticnet3$pred
elasticnet_pred[-(1:5),3] = elasticnet6$pred
elasticnet_pred[-(1:11),4] = elasticnet12$pred

adaelasticnet_pred = matrix(NA,npred,4)
adaelasticnet_pred[,1] = adaelasticnet1$pred
adaelasticnet_pred[-(1:2),2] = adaelasticnet3$pred
adaelasticnet_pred[-(1:5),3] = adaelasticnet6$pred
adaelasticnet_pred[-(1:11),4] = adaelasticnet12$pred


rf_pred = matrix(NA,npred,4)
rf_pred[,1] = rf1$pred
rf_pred[-(1:2),2] = rf3$pred
rf_pred[-(1:5),3] = rf6$pred
rf_pred[-(1:11),4] = rf12$pred


nn_pred = matrix(NA,npred,4)
nn_pred[,1] = nn1$pred
nn_pred[-(1:2),2] = nn3$pred
nn_pred[-(1:5),3] = nn6$pred
nn_pred[-(1:11),4] = nn12$pred


xgb_pred = matrix(NA,npred,4)
xgb_pred[,1] = xgb1$pred
xgb_pred[-(1:2),2] = xgb3$pred
xgb_pred[-(1:5),3] = xgb6$pred
xgb_pred[-(1:11),4] = xgb12$pred

bs_rf_pred = matrix(NA,npred,4)
bs_rf_pred[,1] = BS_RF1$pred
bs_rf_pred[-(1:2),2] = BS_RF3$pred
bs_rf_pred[-(1:5),3] = BS_RF6$pred
bs_rf_pred[-(1:11),4] = BS_RF12$pred

#lstm_pred = matrix(NA,npred,4)
#lstm_pred[,1] = lstm_1$pred
#lstm_pred[-(1:2),2] = lstm_3$pred
#lstm_pred[-(1:5),3] = lstm_6$pred
#lstm_pred[-(1:11),4] = lstm_12$pred



gwtest <- function (model){
  
  source("main/functions/gwtest.R")
  npred=105
  real=tail(Y[,1],npred)  
  gwtest_bs_rf_model = matrix(NA,1,4)     
  gwpvalue_bs_rf_model = matrix(NA,1,4) 
  
  
  for(i in 1:1){
    
    gw = gw.test(bs_rf_pred[,i], model[,i], real, tau=i, T=npred, method="NeweyWest")
    
    gwtest_bs_rf_model[i] <- gw$statistic
    gwpvalue_bs_rf_model[i] <- gw$p.value
  }
  
  
  gw = gw.test(bs_rf_pred[-(1:2),2], model[-(1:2),2], real[-(1:2)], tau=3, T=(npred-2), method="NeweyWest")
  
  gwtest_bs_rf_model[2] <- gw$statistic
  gwpvalue_bs_rf_model[2] <- gw$p.value
  
  
  gw = gw.test(bs_rf_pred[-(1:5),3], model[-(1:5),3], real[-(1:5)], tau=6, T=(npred-5), method="NeweyWest")
  
  gwtest_bs_rf_model[3] <- gw$statistic
  gwpvalue_bs_rf_model[3] <- gw$p.value
  
  
  gw = gw.test(bs_rf_pred[-(1:11),4], model[-(1:11),4], real[-(1:11)], tau=12, T=(npred-11), method="NeweyWest")
  
  gwtest_bs_rf_model[4] <- gw$statistic
  gwpvalue_bs_rf_model[4] <- gw$p.value
  
  
  
  return(list(gwtest_bs_rf_model, gwpvalue_bs_rf_model))
}

# 함수 호출
boruta_rw <- gwtest(rw_pred)
boruta_ar <- gwtest(ar_pred)
boruta_lasso <- gwtest(lasso_pred)
boruta_adalasso <- gwtest(adalasso_pred)
boruta_elasticnet <- gwtest(elasticnet_pred)
boruta_adaelasticnet <- gwtest(adaelasticnet_pred)
boruta_rf <- gwtest(rf_pred)
boruta_nn <- gwtest(nn_pred)
# boruta_lstm <- gwtest(lstm_pred)
boruta_xgb <- gwtest(xgb_pred)


part1_test_result1 <- rbind(boruta_rw[[1]],boruta_ar[[1]],boruta_lasso[[1]],boruta_adalasso[[1]],boruta_elasticnet[[1]],boruta_adaelasticnet[[1]],boruta_rf[[1]],boruta_nn[[1]],boruta_xgb[[1]])

part1_test_result2 <- rbind(boruta_rw[[2]],boruta_ar[[2]],boruta_lasso[[2]],boruta_adalasso[[2]],boruta_elasticnet[[2]],boruta_adaelasticnet[[2]],boruta_rf[[2]],boruta_nn[[2]],boruta_xgb[[2]])

write.csv(part1_test_result1 ,file= "part1_test1.csv")
write.csv(part1_test_result2 ,file= "part1_test2.csv")





###########################################
### = Model Confidence Set (MCS) Test = ### 
###########################################

# Superior Set Model에 남아 있는 모형들의 예측력이 우수함을 의미함 
# 남아 있는 모형이 예측력이 좋음 (포함되었다는 사실이 중요)
# 좋은 모델 
# 모든 forecasat horizon 에서 에러가 제일 낮고 
# model confidence set 에 남아있고 
# giocommini 에서 각각 해봤는데 다 유의하고 

real=tail(Y[,1],npred)  # actual value 

for(i in 1:1){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],xgb_pred[i],bs_rf_pred[i])
  
  LOSS=Pred-real
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_1 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}




# forecast horizon 3
for(i in 2:2){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],xgb_pred[i],bs_rf_pred[i]) %>% na.omit()
  
  LOSS=Pred-real[-(1:2)]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_3 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}


# forecast horizon 6
for(i in 3:3){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],xgb_pred[i],bs_rf_pred[i]) %>% na.omit()
  
  LOSS=Pred-real[-(1:5)]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_6 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}


# forecast horizon 12  

for(i in 4:4){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],xgb_pred[i],bs_rf_pred[i]) %>% na.omit()
  
  LOSS=Pred-real[-(1:11)]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_12 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}


# 6번 best model
library(Boruta)
library(randomForest)

Y3 = Y[c(106:225),]

# Example for lag = 12 setting
lag = 6  # lag = horizon

aux = embed(Y3,4+lag)
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
selected6 = varOrder[1:which.min(Errors1)]             # The Set of Optimal Number of Variables
#selected = varOrder[1:16]                 # 꺾이는 부분. 16개까지만 사용 



source("main/functions/func-rf_selected2022.R")

INF_202306 = runrf(Y2,1,3, selected3)
INF_202309 = runrf(Y2,1,6, selected6)
INF_202306
INF_202309




# 변수 중요도 및 오차 계산은 동일하게 유지

# saving entire worksapce
save.image("강려명_part1.RData")  
