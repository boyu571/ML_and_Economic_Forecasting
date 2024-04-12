#install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir()


#install.packages("glmnet")
library(glmnet)

#install.packages("remotes")
remotes::install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)
library(randomForest)

source("functions/func-adalassorf.R")

load("rawdata.rda")
Y=dados   


nprev=132

indice=1                                                           
lag=1
horizon = 1
type="adalasso"
alpha=1


#func-adalassorf.R 설명 

#rf1c=lasso.rolling.window(Y,nprev,1,1,type="adalasso")

i=nprev
Y.window=Y[(1+nprev-i):(nrow(Y)-i),]

#lasso=runlasso(Y.window,indice,lag,alpha,type)

Y = Y.window


#runlasso=function(Y,indice,lag,alpha=1,type="lasso"){
  
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  
  X=embed(as.matrix(Y2),4)
  dim(X)
  # X = {Y2_t, Y2_(t-1), Y2_(t-2), Y2_(t-3)}
  # embed_example.R 참고 
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),] # Xin은 X의 첫번째행(y_4로 시작하는 행)부터 마지막 바로 전 행(y_(T-1)로 시작하는 행)까지 
  dim(Xin)
  Xout=X[nrow(X),] # [Y2_T, Y2_(T-1), Y2_(T-2), Y2_(T-3)]
  
  y=tail(Y2[,1],nrow(Xin)) # y는 앞부분 첫번째 행 제외하고 y_T까지 
  length(y)
  X = Xin 
  X.out = Xout
  
  
  model=ic.glmnet(X,y,alpha = alpha)
  coef=model$coef
  if(type=="adalasso"){
    penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-1)
    model=ic.glmnet(X,y,penalty.factor = penalty,alpha=alpha)
  }
 
  # 아래 if는 필요 없음 
  if(type=="fal"){
    taus=c(seq(0.1,1,0.1),1.25,1.5,2,3,4,5,7,10)
    alphas=seq(0,1,0.1)
    bb=Inf
    for(alpha in alphas){
      m0=ic.glmnet(X,y,alpha = alpha)
      coef=m0$coef
      for(tau in taus){
        penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-tau)
        m=ic.glmnet(X,y,penalty.factor = penalty)
        crit=m$bic
        if(crit<bb){
          model=m
          bb=crit
        }
      }
    }
  }
  
  
  selected=which(model$coef[-1]!=0)   #intercept 제외하고 0이 아닌 coefficient
  if(length(selected)<2){
    selected=1:2
  }
  
  selected
  head(X[,selected])
  
  modelrf=randomForest(X[,selected],y)   # 선택된 변수들만 가지고 random forest
  pred=predict(modelrf,X.out[selected])
  
  return(list("pred"=pred))
#}








## == presente == ##

#rf1c=lasso.rolling.window(Y,nprev,1,1,type="adalasso")
rf1p=lasso.rolling.window(Y,nprev,2,1,type="adalasso")
#rf2c=lasso.rolling.window(Y,nprev,1,2,type="adalasso")
rf2p=lasso.rolling.window(Y,nprev,2,2,type="adalasso")
#rf3c=lasso.rolling.window(Y,nprev,1,3,type="adalasso")
rf3p=lasso.rolling.window(Y,nprev,2,3,type="adalasso")
#rf4c=lasso.rolling.window(Y,nprev,1,4,type="adalasso")
rf4p=lasso.rolling.window(Y,nprev,2,4,type="adalasso")
#rf5c=lasso.rolling.window(Y,nprev,1,5,type="adalasso")
rf5p=lasso.rolling.window(Y,nprev,2,5,type="adalasso")
#rf6c=lasso.rolling.window(Y,nprev,1,6,type="adalasso")
rf6p=lasso.rolling.window(Y,nprev,2,6,type="adalasso")
#rf7c=lasso.rolling.window(Y,nprev,1,7,type="adalasso")
rf7p=lasso.rolling.window(Y,nprev,2,7,type="adalasso")
#rf8c=lasso.rolling.window(Y,nprev,1,8,type="adalasso")
rf8p=lasso.rolling.window(Y,nprev,2,8,type="adalasso")
#rf9c=lasso.rolling.window(Y,nprev,1,9,type="adalasso")
rf9p=lasso.rolling.window(Y,nprev,2,9,type="adalasso")
#rf10c=lasso.rolling.window(Y,nprev,1,10,type="adalasso")
rf10p=lasso.rolling.window(Y,nprev,2,10,type="adalasso")
#rf11c=lasso.rolling.window(Y,nprev,1,11,type="adalasso")
rf11p=lasso.rolling.window(Y,nprev,2,11,type="adalasso")
#rf12c=lasso.rolling.window(Y,nprev,1,12,type="adalasso")
rf12p=lasso.rolling.window(Y,nprev,2,12,type="adalasso")


### == juntando tudo ==  ###

#cpi=cbind(rf1c$pred,rf2c$pred,rf3c$pred,rf4c$pred,
#          rf5c$pred,rf6c$pred,rf7c$pred,rf8c$pred,
#          rf9c$pred,rf10c$pred,rf11c$pred,rf12c$pred)

pce=cbind(rf1p$pred,rf2p$pred,rf3p$pred,rf4p$pred,
          rf5p$pred,rf6p$pred,rf7p$pred,rf8p$pred,
          rf9p$pred,rf10p$pred,rf11p$pred,rf12p$pred)


##
#write.table(cpi,"forecasts/passado2000/adalassorf-cpi.csv",sep=";",row.names = FALSE, col.names = FALSE)
write.table(pce,"forecasts/passado2000/adalassorf-pce.csv",sep=";",row.names = FALSE, col.names = FALSE)

