###############################################################################
### Forecasting Inflation in a Data Rich Environment (Medeiros et al, 2019) ###
###############################################################################

### Codes for Replicating TABLE S.12 - Forecasting Errors for CPI (1990-2000), 

####################
### Testing Part
####################

rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


load("results_forecasts.RData")  #forecasts from Inflation_1990-2000_Forecasting.R 

install.packages("sandwich")
install.packages("MCS")
library(sandwich)
library(MCS)


#############################################################
### = Giacomini-White Test for Equal Predictive Ability = ###
#############################################################

source("functions/gwtest.R")

real=tail(Y[,1],npred)  # actual value 

rw_pred = matrix(NA,npred,12)
rw_pred[,1] = rw1$pred
rw_pred[-1,2] = rw2$pred
rw_pred[-(1:2),3] = rw3$pred
rw_pred[-(1:3),4] = rw4$pred
rw_pred[-(1:4),5] = rw5$pred
rw_pred[-(1:5),6] = rw6$pred
rw_pred[-(1:6),7] = rw7$pred
rw_pred[-(1:7),8] = rw8$pred
rw_pred[-(1:8),9] = rw9$pred
rw_pred[-(1:9),10] = rw10$pred
rw_pred[-(1:10),11] = rw11$pred
rw_pred[-(1:11),12] = rw12$pred

rf_pred = matrix(NA,npred,12)
rf_pred[,1] = rf1$pred
rf_pred[-1,2] = rf2$pred
rf_pred[-(1:2),3] = rf3$pred
rf_pred[-(1:3),4] = rf4$pred
rf_pred[-(1:4),5] = rf5$pred
rf_pred[-(1:5),6] = rf6$pred
rf_pred[-(1:6),7] = rf7$pred
rf_pred[-(1:7),8] = rf8$pred
rf_pred[-(1:8),9] = rf9$pred
rf_pred[-(1:9),10] = rf10$pred
rf_pred[-(1:10),11] = rf11$pred
rf_pred[-(1:11),12] = rf12$pred

# RW vs RF
gwtest_rw_rf = matrix(NA,1,12)     # forecast horizon별 test statistic 결과, difference = rw_pred - rf_pred
gwpvalue_rw_rf = matrix(NA,1,12)  # forecast horizon별 p-value
gwtest_rw_rf
# gwtest.R 파일 참조. 귀무가설은 두 모형의 예측력이 동일하다는 것임. 따라서 p-value가 0.05보다 작으면, forecast loss가 작은 모형의 예측력이 통계적으로 유의하게 우월함을 의미함. 

for(i in 1:1){
  
  gw = gw.test(adalasso_pred[,i], rf_pred[,i], real, tau=i, T=npred, method="NeweyWest")
  
  gwtest_rw_rf[i] <- gw$statistic
  gwpvalue_rw_rf[i] <- gw$p.value
}

for(i in 2:12){
  gw = gw.test(adalasso_pred[-(1:(i-1)),i], rf_pred[-(1:(i-1)),i], real[-(1:(i-1))], tau=i, T=(npred-i+1), method="NeweyWest")
  
  gwtest_rw_rf[i] <- gw$statistic
  gwpvalue_rw_rf[i] <- gw$p.value
}

# gwtest와 gwpvalue를 정리할 필요성

##

ar_pred = matrix(NA,npred,12)
ar_pred[,1] = ar1$pred
ar_pred[-1,2] = ar2$pred
ar_pred[-(1:2),3] = ar3$pred
ar_pred[-(1:3),4] = ar4$pred
ar_pred[-(1:4),5] = ar5$pred
ar_pred[-(1:5),6] = ar6$pred
ar_pred[-(1:6),7] = ar7$pred
ar_pred[-(1:7),8] = ar8$pred
ar_pred[-(1:8),9] = ar9$pred
ar_pred[-(1:9),10] = ar10$pred
ar_pred[-(1:10),11] = ar11$pred
ar_pred[-(1:11),12] = ar12$pred

ridge_pred = matrix(NA,npred,12)
ridge_pred[,1] = ridge1$pred
ridge_pred[-1,2] = ridge2$pred
ridge_pred[-(1:2),3] = ridge3$pred
ridge_pred[-(1:3),4] = ridge4$pred
ridge_pred[-(1:4),5] = ridge5$pred
ridge_pred[-(1:5),6] = ridge6$pred
ridge_pred[-(1:6),7] = ridge7$pred
ridge_pred[-(1:7),8] = ridge8$pred
ridge_pred[-(1:8),9] = ridge9$pred
ridge_pred[-(1:9),10] = ridge10$pred
ridge_pred[-(1:10),11] = ridge11$pred
ridge_pred[-(1:11),12] = ridge12$pred

lasso_pred = matrix(NA,npred,12)
lasso_pred[,1] = lasso1$pred
lasso_pred[-1,2] = lasso2$pred
lasso_pred[-(1:2),3] = lasso3$pred
lasso_pred[-(1:3),4] = lasso4$pred
lasso_pred[-(1:4),5] = lasso5$pred
lasso_pred[-(1:5),6] = lasso6$pred
lasso_pred[-(1:6),7] = lasso7$pred
lasso_pred[-(1:7),8] = lasso8$pred
lasso_pred[-(1:8),9] = lasso9$pred
lasso_pred[-(1:9),10] = lasso10$pred
lasso_pred[-(1:10),11] = lasso11$pred
lasso_pred[-(1:11),12] = lasso12$pred

adalasso_pred = matrix(NA,npred,12)
adalasso_pred[,1] = adalasso1$pred
adalasso_pred[-1,2] = adalasso2$pred
adalasso_pred[-(1:2),3] = adalasso3$pred
adalasso_pred[-(1:3),4] = adalasso4$pred
adalasso_pred[-(1:4),5] = adalasso5$pred
adalasso_pred[-(1:5),6] = adalasso6$pred
adalasso_pred[-(1:6),7] = adalasso7$pred
adalasso_pred[-(1:7),8] = adalasso8$pred
adalasso_pred[-(1:8),9] = adalasso9$pred
adalasso_pred[-(1:9),10] = adalasso10$pred
adalasso_pred[-(1:10),11] = adalasso11$pred
adalasso_pred[-(1:11),12] = adalasso12$pred

elasticnet_pred = matrix(NA,npred,12)
elasticnet_pred[,1] = elasticnet1$pred
elasticnet_pred[-1,2] = elasticnet2$pred
elasticnet_pred[-(1:2),3] = elasticnet3$pred
elasticnet_pred[-(1:3),4] = elasticnet4$pred
elasticnet_pred[-(1:4),5] = elasticnet5$pred
elasticnet_pred[-(1:5),6] = elasticnet6$pred
elasticnet_pred[-(1:6),7] = elasticnet7$pred
elasticnet_pred[-(1:7),8] = elasticnet8$pred
elasticnet_pred[-(1:8),9] = elasticnet9$pred
elasticnet_pred[-(1:9),10] = elasticnet10$pred
elasticnet_pred[-(1:10),11] = elasticnet11$pred
elasticnet_pred[-(1:11),12] = elasticnet12$pred

adaelasticnet_pred = matrix(NA,npred,12)
adaelasticnet_pred[,1] = adaelasticnet1$pred
adaelasticnet_pred[-1,2] = adaelasticnet2$pred
adaelasticnet_pred[-(1:2),3] = adaelasticnet3$pred
adaelasticnet_pred[-(1:3),4] = adaelasticnet4$pred
adaelasticnet_pred[-(1:4),5] = adaelasticnet5$pred
adaelasticnet_pred[-(1:5),6] = adaelasticnet6$pred
adaelasticnet_pred[-(1:6),7] = adaelasticnet7$pred
adaelasticnet_pred[-(1:7),8] = adaelasticnet8$pred
adaelasticnet_pred[-(1:8),9] = adaelasticnet9$pred
adaelasticnet_pred[-(1:9),10] = adaelasticnet10$pred
adaelasticnet_pred[-(1:10),11] = adaelasticnet11$pred
adaelasticnet_pred[-(1:11),12] = adaelasticnet12$pred


csr_pred = matrix(NA,npred,12)
csr_pred[,1] = csr1$pred
csr_pred[-1,2] = csr2$pred
csr_pred[-(1:2),3] = csr3$pred
csr_pred[-(1:3),4] = csr4$pred
csr_pred[-(1:4),5] = csr5$pred
csr_pred[-(1:5),6] = csr6$pred
csr_pred[-(1:6),7] = csr7$pred
csr_pred[-(1:7),8] = csr8$pred
csr_pred[-(1:8),9] = csr9$pred
csr_pred[-(1:9),10] = csr10$pred
csr_pred[-(1:10),11] = csr11$pred
csr_pred[-(1:11),12] = csr12$pred


tfact_pred = matrix(NA,npred,12)
tfact_pred[,1] = tfact1$pred
tfact_pred[-1,2] = tfact2$pred
tfact_pred[-(1:2),3] = tfact3$pred
tfact_pred[-(1:3),4] = tfact4$pred
tfact_pred[-(1:4),5] = tfact5$pred
tfact_pred[-(1:5),6] = tfact6$pred
tfact_pred[-(1:6),7] = tfact7$pred
tfact_pred[-(1:7),8] = tfact8$pred
tfact_pred[-(1:8),9] = tfact9$pred
tfact_pred[-(1:9),10] = tfact10$pred
tfact_pred[-(1:10),11] = tfact11$pred
tfact_pred[-(1:11),12] = tfact12$pred

###########################################
### = Model Confidence Set (MCS) Test = ### 
###########################################

# Superior Set Model에 남아 있는 모형들의 예측력이 우수함을 의미함 

# 아래 loop를 다 돌리는 건 시간이 걸림. Try a specific i(forecasting horizon) first. 
for(i in 1:1){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i],ridge_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], csr_pred[,i], tfact_pred[,i], rf_pred[,i])
    
  LOSS=Pred-real
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_i <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}

for(i in 2:12){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i],ridge_pred[,i], lasso_pred[,i], adalasso_pred[,i], elasticnet_pred[,i],  adaelasticnet_pred[,i], csr_pred[,i], tfact_pred[,i], rf_pred[,i]) 
  LOSS=Pred[-(1:(i-1)),]-real[-(1:(i-1))]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_i <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}



?MCSprocedure
# (1-alpha)*100% 수준에서 Model Confidence Set에 포함 (may also try alpha = 0.2)

save.image("results_test.RData")
