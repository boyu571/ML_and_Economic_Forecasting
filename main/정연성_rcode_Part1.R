#install.packages("rstudioapi")
library(rstudioapi)
setwd("C:\\Users\\PC2\\Desktop\\2023-24 SKKU 퀀트응용경제학과\\24-1 머신러닝과 경제예측\\한희준 교수님\\머신러닝과 경제예측_한희준_퀀트응용경제학과\\Project2024\\Rcode")
dir()

#install.packages("devtools")  #github의 package들을 인스톨할 수 있게 함 

# Installing 'fbi' Package from Github
library(devtools)  
#install_github("cykbennie/fbi")

library(fbi)


data <- read.csv("DataKoreaFrom200408To202306.csv")



### Transformation 

tcode = data[1,] 

tcode

data1 <- data[-1,]    # tcode line 제거 
tdata = data1[-(1:2),]   #log후 2차 차분도 있기 때문에 맞추려고 






for (i in 2:94){
  
  if(tcode[i] == 1){
    tdata[,i] <- data1[-(1:2),i]
  } # no transformation  
  
  if(tcode[i] == 2){
    tdata[,i] <- diff(data1[-1,i])
  } # 1차 차분
  
  # tdoce ==3(2차 차분)에 해당하는 데이터는 없음
  
  if(tcode[i] == 4){
    tdata[,i] <- log(data1[-(1:2),i])
  } # log
  
  if(tcode[i] == 5){
    tdata[,i] <- diff(log(data1[-1,i]))
  } # log differencing
  
  if(tcode[i] == 6){
    tdata[,i] <- diff(diff(log(data1[,i])))
  } # log취한 뒤 2차 차분
  
  if(tcode[i] == 7){
    tdata[,i] <- diff(data1[-1,i]/data1[1:(nrow(data1)-1),i])
  } # 증가율의 1차 차분
}



tdata$CPI
CPI <- tdata[,65]

# Y 합치기 (FRED_importing_from201606.R 참고 )
Y <- tdata[,-1]
Y = Y[,-65]
Y <- cbind(CPI, Y)

?as.Date


# rowname 변경
library(zoo)
monthly_dates <- seq(as.Date("2004-10-01"), by = "month", length.out = nrow(Y)+2)
monthly_dates <- as.yearmon(monthly_dates)
row.names(Y) <- monthly_dates


save.image(file = "kor_tdata.RData")
save(Y,file='kor_tdata.rda')


load('kor_tdata.RData')
load('kor_tdata.rda')



#########################################################3


####################
### Forecasting Part
#################### 


library(devtools)  
#install_github("gabrielrvsc/HDeconometrics")

library(HDeconometrics)


# Loading Data
load('kor_tdata.rda')


# Number of Forecasts
npred=105



## Random Walk Model ##
source("functions/func-rw.R")

rw1=rw.rolling.window(Y,npred,1,1)
rw3=rw.rolling.window(Y,npred,1,3)
rw6=rw.rolling.window(Y,npred,1,6)
rw12=rw.rolling.window(Y,npred,1,12)




rw = cbind(rw1$errors[1],rw3$errors[1],rw6$errors[1],rw12$errors[1])
rw2 = cbind(rw1$errors[2],rw3$errors[2],rw6$errors[2],rw12$errors[2])

# erros[1]: RMSE, errors[2]: MAE



## AR(4) Model ##
source("functions/func-ar.R")

ar1=ar.rolling.window(Y,npred,1,1,type="fixed")

ar3=ar.rolling.window(Y,npred,1,3,type="fixed")

ar6=ar.rolling.window(Y,npred,1,6,type="fixed")

ar12=ar.rolling.window(Y,npred,1,12,type="fixed")



ar = cbind(ar1$errors[1],ar3$errors[1],ar6$errors[1],ar12$errors[1])
ar2 = cbind(ar1$errors[2],ar3$errors[2],ar6$errors[2],ar12$errors[2])



## LASSO ##
source("functions/func-lasso.R")
alpha=1   

lasso1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
lasso3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
lasso6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
lasso12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")



lasso = cbind(lasso1$errors[1],lasso3$errors[1],lasso6$errors[1],lasso12$errors[1])
lasso2 = cbind(lasso1$errors[2],lasso3$errors[2],lasso6$errors[2],lasso12$errors[2])



## Adaptive LASSO ##

adalasso1=lasso.rolling.window(Y,npred,1,1,alpha,type="adalasso")
adalasso3=lasso.rolling.window(Y,npred,1,3,alpha,type="adalasso")
adalasso6=lasso.rolling.window(Y,npred,1,6,alpha,type="adalasso")
adalasso12=lasso.rolling.window(Y,npred,1,12,alpha,type="adalasso")


adalasso = cbind(adalasso1$errors[1],adalasso3$errors[1],adalasso6$errors[1],adalasso12$errors[1])
adalasso2 = cbind(adalasso1$errors[2],adalasso3$errors[2],adalasso6$errors[2],adalasso12$errors[2])




## Elastic Net ##

alpha=0.5

elasticnet1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
elasticnet3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
elasticnet6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
elasticnet12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")





elasticnet = cbind(elasticnet1$errors[1],elasticnet3$errors[1],elasticnet6$errors[1],elasticnet12$errors[1])
elasticnet2 = cbind(elasticnet1$errors[2],elasticnet3$errors[2],elasticnet6$errors[2],elasticnet12$errors[2])




## Adaptive Elastic Net ##

adaelasticnet1=lasso.rolling.window(Y,npred,1,1,alpha,type="adalasso")
adaelasticnet3=lasso.rolling.window(Y,npred,1,3,alpha,type="adalasso")
adaelasticnet6=lasso.rolling.window(Y,npred,1,6,alpha,type="adalasso")
adaelasticnet12=lasso.rolling.window(Y,npred,1,12,alpha,type="adalasso")



adaelasticnet = cbind(adaelasticnet1$errors[1],adaelasticnet3$errors[1],adaelasticnet6$errors[1],adaelasticnet12$errors[1])

adaelasticnet2 = cbind(adaelasticnet1$errors[2],adaelasticnet3$errors[2],adaelasticnet6$errors[2],adaelasticnet12$errors[2])





## Random Forest (RF) ##
source("functions/func-rf.R")
library(randomForest)

rf1=rf.rolling.window(Y,npred,1,1)
rf3=rf.rolling.window(Y,npred,1,3)
rf6=rf.rolling.window(Y,npred,1,6)
rf12=rf.rolling.window(Y,npred,1,12)



rf = cbind(rf1$errors[1],rf3$errors[1],rf6$errors[1],rf12$errors[1])

rf2 = cbind(rf1$errors[2],rf3$errors[2],rf6$errors[2],rf12$errors[2])



### Errors and their Relative Ratio to RW Model ###

ERRORS = rbind(rw, ar, lasso, adalasso, elasticnet, adaelasticnet, rf)
ERRORS2 = rbind(rw2, ar2, lasso2, adalasso2, elasticnet2, adaelasticnet2, rf2)
#RATIO = ERRORS / rep(ERRORS[1,], each=nrow(ERRORS))

write.csv(ERRORS2,file="errors2.csv")

# saving entire worksapce
save.image("kor_results_forecasts1.RData")  





######################## Xgboost ##########################
# Import the Boosting function
source('functions/func-xgb.R')
library(HDeconometrics)
library(xgboost)

npred=105

xgb1=xgb.rolling.window(Y,npred,1,1)
xgb3=xgb.rolling.window(Y,npred,1,3)
xgb6=xgb.rolling.window(Y,npred,1,6)
xgb12=xgb.rolling.window(Y,npred,1,12)

# Get the error
xgb.rmse = cbind(xgb1$errors[1], xgb3$errors[1], xgb6$errors[1], xgb12$errors[1])
xgb.mae = cbind(xgb1$errors[2], xgb3$errors[2], xgb6$errors[2], xgb12$errors[2])






######################## Neural Networks(Deep Learning) ##########################
source("functions/func-nn.R")

library(dplyr)
library(keras)

# neural network 
library(h2o)
h2o.init()

nn1=nn.rolling.window(Y,npred,1,1)   # 여기까지 함 
nn3=nn.rolling.window(Y,npred,1,3)
nn6=nn.rolling.window(Y,npred,1,6)
nn12=nn.rolling.window(Y,npred,1,12)

# Get the error
nn.rmse = cbind(nn1$errors[1], nn3$errors[1], nn6$errors[1], nn12$errors[1])
nn.mae = cbind(nn1$errors[2], nn3$errors[2], nn6$errors[2],  nn12$errors[2])

# Get the prediction
# nn.pred = cbind(nn1$pred, xnn3$pred, nn6$pred, nnb9$pred, nn12$pred)



save.image("Part1_kor_results_forecasts.RData") 




# LSTM Function  

# ==================================================================
# Install packages and Recall Library

#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)

library(reticulate)

#install.packages("tensorflow")  #관리자 권한으로 실행 
library(tensorflow)
#install_tensorflow()

reticulate::py_config()

#install.packages("keras")  #관리자 권한으로 실행
library(keras) 
#install_keras()


tf$constant("hello")
tf$version



# ==================================================================
# Normalization

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# Inverse Normalization 
denormalize <- function(x, minval, maxval) {
  x*(maxval-minval) + minval
}

# ==================================================================
# Load Data Set

dir()

load("kor_tdata.rda")


# ==================================================================

nprev=105
npred = nprev
indice = 1
horizon = 1
lag = horizon

# FINAL CHECK COMPLETE

source("functions/func-lstm.R")

lstm_1 <- mul.lstm.rolling.window(Y,npred,1,1)  
lstm_1$errors 


lstm_3 <- mul.lstm.rolling.window(Y,npred,1,3)  
lstm_3$errors 


lstm_6 <- mul.lstm.rolling.window(Y,npred,1,6)  
lstm_6$errors 



lstm_12 <- mul.lstm.rolling.window(Y,npred,1,12)  
lstm_12$errors 


save.image("Part1_kor_results_forecasts.RData")







### Boruta Algorithm

###############################################################################


load("Part1_kor_results_forecasts.RData")  
save(Y,file='Part1_data.rda')
load('Part1_data.rda')

########################################################
### = Ranking Variables using the Boruta Algorithm = ###
########################################################


#install.packages("Boruta")
library(Boruta)
library(randomForest)
library(tidyverse)

Y2 = Y                  ## Using the Whole Sample 
# Y2 = Y[1:359,]        ## Using only the First Window

# 몇 개 윈도우에서 중요변수 뽑아보고 전체랑 비교 
# 

# Example for lag = 12 setting
lag = 1  # lag = horizon
Y2 <- Y2 %>% as.matrix()

aux = embed(Y2,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y2)*lag))]


set.seed(42)
boruta_12 <- Boruta(X, y, maxRuns = 100)

plot = plot(boruta_12)
plot

attstats = attStats(boruta_12)
attstats



order = order(attstats$meanImp, decreasing = T)

order


## Cross Validation for Optimal Number of Variables # (Up to 70 Variables)

Errors = rep(NA,70)          


#load("Boruta_result.RData")  #아래 loop 시간이 오래 걸림

for (i in 2:70){
  
  selected = order[1:i]
  
  model=randomForest(X[,selected], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:70), Errors, xlab="# of Variables", ylab="Fitted Squared Error")



# lag = horizon =1 
Errors1 = Errors
varOrder1 = order(attstats$meanImp, decreasing = T)  
which.min(Errors1)  
selected1 = varOrder1[1:19]
########################


# lag = horizon =3
Errors3 = Errors
varOrder3 <- order(attstats$meanImp, decreasing = T) 
which.min(Errors3) 
selected3 = varOrder3[1:16]    # elbow point 
########################


# lag = horizon =6 
Errors6 = Errors
varOrder6 <- order(attstats$meanImp, decreasing = T) 
which.min(Errors6) 
selected6 = varOrder6[1:19]   # elbow point  
########################


# lag = horizon =12
Errors12 = Errors
varOrder12 <- order(attstats$meanImp, decreasing = T) 
which.min(Errors12) 
selected12 = varOrder12[1:12]   # elbow point  









# Rolling Window with Selected Variables

source("functions/func-rf_selected2022.R")

BS_RF1 = rf.rolling.window(Y2,npred,1,1,selected1)
BS_RF1$errors

BS_RF3 = rf.rolling.window(Y2,npred,1,3,selected3)
BS_RF3$errors

BS_RF6 = rf.rolling.window(Y2,npred,1,6,selected6)
BS_RF6$errors

BS_RF12 = rf.rolling.window(Y2,npred,1,12,selected12)
BS_RF12$errors

#library(foreach)
#install.packages("doSNOW")
#library(doSNOW)
#registerDoSNOW(makeCluster(3, type="SOCK")) # For parallel computing, core 3개인 경우임.

# parallel computing이 된다면, func-rf_selected2022.R 파일의 15번째 줄인 
# model=randomForeast(X[,selected],y,importance=TRUE) 대신
# model <- foreach(ntree = rep(167, 3), .combine=randomForest::combine, .multicombine=TRUE, .packages = "randomForest") %dopar% randomForest(X[,selected],y,ntree=ntree, importance=TRUE) 로 수정해서 돌릴 것.  

save.image("Part1_kor_results_forecasts.RData")













####################
### Testing Part
####################

rm(list=ls())

setwd("C:\\Users\\PC2\\Desktop\\2023-24 SKKU 퀀트응용경제학과\\24-1 머신러닝과 경제예측\\한희준 교수님\\머신러닝과 경제예측_한희준_퀀트응용경제학과\\Project2024\\Rcode")



#install.packages("sandwich")
#install.packages("MCS")
library(sandwich)
library(MCS)




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


lstm_pred = matrix(NA,npred,4)
lstm_pred[,1] = lstm_1$pred
lstm_pred[-(1:2),2] = lstm_3$pred
lstm_pred[-(1:5),3] = lstm_6$pred
lstm_pred[-(1:11),4] = lstm_12$pred


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


source("functions/gwtest.R")


# forecasting 에 따라 예측치가 하나씩 줄어들기 때문 

# Null : 두 모형의 예측력이 동일 

# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함. 
# tau : forecast horizon 
# tstatistics >0 일 때 앞의 모델 rmse > 뒤의 모델 rmse 임  




gwtest <- function (model){
  
  source("functions/gwtest.R")
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
boruta_lstm <- gwtest(lstm_pred)
boruta_xgb <- gwtest(xgb_pred)


part1_test_result1 <- rbind(boruta_rw[[1]],boruta_ar[[1]],boruta_lasso[[1]],boruta_adalasso[[1]],boruta_elasticnet[[1]],boruta_adaelasticnet[[1]],boruta_rf[[1]],boruta_nn[[1]],boruta_lstm[[1]],boruta_xgb[[1]])

part1_test_result2 <- rbind(boruta_rw[[2]],boruta_ar[[2]],boruta_lasso[[2]],boruta_adalasso[[2]],boruta_elasticnet[[2]],boruta_adaelasticnet[[2]],boruta_rf[[2]],boruta_nn[[2]],boruta_lstm[[2]],boruta_xgb[[2]])

write.csv(part1_test_result1 ,file= "part1_test_result1.csv")
write.csv(part1_test_result2 ,file= "part1_test_result2.csv")





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
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],lstm_pred[i],xgb_pred[i],bs_rf_pred[i])
  
  LOSS=Pred-real
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_1 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}




# forecast horizon 3
for(i in 2:2){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],lstm_pred[i],xgb_pred[i],bs_rf_pred[i]) %>% na.omit()
  
  LOSS=Pred-real[-(1:2)]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_3 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}


# forecast horizon 6
for(i in 3:3){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],lstm_pred[i],xgb_pred[i],bs_rf_pred[i]) %>% na.omit()
  
  LOSS=Pred-real[-(1:5)]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_6 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}


# forecast horizon 12  

for(i in 4:4){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], rf_pred[,i],nn_pred[,i],lstm_pred[i],xgb_pred[i],bs_rf_pred[i]) %>% na.omit()
  
  LOSS=Pred-real[-(1:11)]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_6 <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}


?MCSprocedure
# (1-alpha)*100% 수준에서 Model Confidence Set에 포함 (may also try alpha = 0.2)

save.image("Part1_kor_results_forecasts.RData")



