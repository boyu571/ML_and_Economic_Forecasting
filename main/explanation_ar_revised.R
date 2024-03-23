
#install.packages("rstudioapi")
library(rstudioapi)

setwd("C:\\Users\\boyu5\\boyu571_github\\00_SKKU_24_Sprig\\MLEF\\ML_and_Economic_Forecasting\\main")
dir()

source("functions/func-ar.R")

load("rawdata.rda")
Y=dados   
#1960�� 2������ 2000�� 12������ ������

nprev=132  
#number of prediction, 1990�� 1������ 2000�� 12������ 132���� ����

#########################
## ar.rolling.window ���� 
#########################

# ar1c=ar.rolling.window(Y,nprev,1,1,type="fixed")  ���� 

# ar.rolling.window=function(Y,nprev,indice=1,lag=1,type="fixed"), 

# indice=1�� CPI, indice=2�� PCE�� ��Ÿ��
# lag=1 means that forecasting horizon is one. �� 1-step ahead forecasting 

#########################
## lag=1�� ��� ����  
#########################
indice=1
lag=1
horizon = lag
type="fixed"

# ar.rolling.window=function(Y,nprev,indice=1,lag=1,type="fixed"){

save.coef=matrix(NA,nprev,5)   #AR(4) model, intercept ���� parameter 5��
save.pred=matrix(NA,nprev,1)


i = nprev  #�Ʒ� for loop�� ����, i=nprev�� try�� �� �� loop�� ������ ��   

for(i in nprev:1){
  Y.window=Y[(1+nprev-i):(nrow(Y)-i),]   
  # i=nprev�� ��, window�� 1989�� 12������
  fact=runAR(Y.window,indice,lag)
  # runAR �Ʒ� ���� ���� 
  save.coef[(1+nprev-i),]=fact$coef
  save.pred[(1+nprev-i),]=fact$pred
  cat("iteration",(1+nprev-i),"\n")
  
  ?cat  
}

real=Y[,indice]
plot(real,type="l")
lines(c(rep(NA,length(real)-nprev),save.pred),col="red")  

rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
# tail(real,nprev)  real ������ ������ nprev�� ����ġ 

mae=mean(abs(tail(real,nprev)-save.pred))
errors=c("rmse"=rmse,"mae"=mae)

return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
#}


ar1c=ar.rolling.window(Y,nprev,1,1,type="fixed")  

ar1c$pred
ar1c$coef
ar1c$errors


#########################
#���� runAR=function(Y,indice,lag,type="fixed")
#########################

#runAR=function(Y,indice,lag,type="fixed"){

Y2=cbind(Y[,indice])

# �Ʒ� �κ��� old version���� X.out�� ������ �� horizon>1�� ��� ������ ���� 
#aux=embed(Y2,4+horizon)
#y=aux[,1]
#X=aux[,-c(1:(ncol(Y2)*horizon))]  

#if(horizon==1){
#  X.out=tail(aux,1)[1:ncol(X)]  
#}else{
#  X.out=aux[,-c(1:(ncol(Y2)*(horizon-1)))]
#  X.out=tail(X.out,1)[1:ncol(X)]
#}


X=embed(as.matrix(Y2),4)
# X = {y_t, y_(t-1), y_(t-2), y_(t-3)}
# embed_example.R ���� 

Xin=X[-c((nrow(X)-horizon+1):nrow(X)),] # Xin�� X�� ù��°��(y_4�� �����ϴ� ��)���� ������ �ٷ� �� ��(y_(T-1)�� �����ϴ� ��)���� 
Xout=X[nrow(X),]    # [y_T, y_(T-1), y_(T-2), y_(T-3)]

y=tail(Y2[,1],nrow(Xin))  # y�� �պκ� ù��° �� �����ϰ� y_T���� 
X = Xin 
X.out = Xout  


# lag �Ǵ� horizon�� 1���� ū ���� �Ʒ� ���� 

if(type=="fixed"){
  model=lm(y~X)
  coef=coef(model)
}

# type�� bic�� ���� �Ʒ� ����
if(type=="bic"){
  bb=Inf
  for(i in seq(1,ncol(X),1)){
    m=lm(y~X[,1:i])
    crit=BIC(m)
    if(crit<bb){
      bb=crit
      model=m
      ar.coef=coef(model)
    }
  }
  coef=rep(0,ncol(X)+1)
  coef[1:length(ar.coef)]=ar.coef
}

pred=c(1,X.out)%*%coef

return(list("model"=model,"pred"=pred,"coef"=coef))
#}


#########################
##  lag=2�� ��� ���� 
#########################
lag = 2  # 2-step ahead forecasting
horizon = lag

Y2=cbind(Y[,indice])
X=embed(as.matrix(Y2),4)
# X = {y_t, y_(t-1), y_(t-2), y_(t-3), y_(t-4)}
# embed_example.R ���� 

Xin=X[-c((nrow(X)-horizon+1):nrow(X)),] # Xin�� X�� ù��°��(y_4�η� �����ϴ� ��)���� ������ �ٷ� ���� ��(y_(T-2)�� �����ϴ� ��)���� 
Xout=X[nrow(X),]    # [y_T, y_(T-1), y_(T-2), y_(T-3)]

y=tail(Y2[,1],nrow(Xin))  # y�� �պκ� �ι�° ����� �����ϰ� y_T���� 
X = Xin 
X.out = Xout  

#########################
##  lag=3�� ��� ���� 
#########################
lag = 3  # 3-step ahead forecasting
horizon = lag

X=embed(as.matrix(Y2),4)
# X = {y_t, y_(t-1), y_(t-2), y_(t-3), y_(t-4)}
# embed_example.R ���� 

Xin=X[-c((nrow(X)-horizon+1):nrow(X)),] # Xin�� X�� ù��°��(y_4�� �����ϴ� ��)���� ������ �ٷ� ������ ��(y_(T-3)�� �����ϴ� ��)���� 
Xout=X[nrow(X),]    # [y_T, y_(T-1), y_(T-2), y_(T-3)]

y=tail(Y2[,1],nrow(Xin))  # y�� �պκ� ����° ����� �����ϰ� y_T���� 
X = Xin 
X.out = Xout  

#########################
## type="bic" ���� 
# AR(1), AR(2), AR(3), AR4(4) �߿��� bic�� ���� ���� ä��
#########################
indice = 1
lag=1
horizon = lag
type="bic"

X=embed(as.matrix(Y2),4)
# X = {y_t, y_(t-1), y_(t-2), y_(t-3), y_(t-4)}
# embed_example.R ���� 

Xin=X[-c((nrow(X)-horizon+1):nrow(X)),] # Xin�� X�� ù��°��(y_4�� �����ϴ� ��)���� ������ �ٷ� �� ��(y_(T-1)�� �����ϴ� ��)���� 
Xout=X[nrow(X),]    # [y_T, y_(T-1), y_(T-2), y_(T-3)]

y=tail(Y2[,1],nrow(Xin))  # y�� �պκ� ù��° �� �����ϰ� y_T���� 
X = Xin 
X.out = Xout  

i =2 

if(type=="bic"){
  bb=Inf
  # try i=1, i=2, i=3, i=4
  #for(i in seq(1,ncol(X),1)){
  m=lm(y~X[,1:i])
  crit=BIC(m)
  if(crit<bb){
    bb=crit
    model=m
    ar.coef=coef(model)
  }
  #}
  coef=rep(0,ncol(X)+1)
  coef[1:length(ar.coef)]=ar.coef
}
pred=c(1,X.out)%*%coef



#########################

ar1p=ar.rolling.window(Y,nprev,2,1,type="fixed")
ar2c=ar.rolling.window(Y,nprev,1,2,type="fixed")
ar2p=ar.rolling.window(Y,nprev,2,2,type="fixed")
ar3c=ar.rolling.window(Y,nprev,1,3,type="fixed")
ar3p=ar.rolling.window(Y,nprev,2,3,type="fixed")
ar4c=ar.rolling.window(Y,nprev,1,4,type="fixed")
ar4p=ar.rolling.window(Y,nprev,2,4,type="fixed")
ar5c=ar.rolling.window(Y,nprev,1,5,type="fixed")
ar5p=ar.rolling.window(Y,nprev,2,5,type="fixed")
ar6c=ar.rolling.window(Y,nprev,1,6,type="fixed")
ar6p=ar.rolling.window(Y,nprev,2,6,type="fixed")
ar7c=ar.rolling.window(Y,nprev,1,7,type="fixed")
ar7p=ar.rolling.window(Y,nprev,2,7,type="fixed")
ar8c=ar.rolling.window(Y,nprev,1,8,type="fixed")
ar8p=ar.rolling.window(Y,nprev,2,8,type="fixed")
ar9c=ar.rolling.window(Y,nprev,1,9,type="fixed")
ar9p=ar.rolling.window(Y,nprev,2,9,type="fixed")
ar10c=ar.rolling.window(Y,nprev,1,10,type="fixed")
ar10p=ar.rolling.window(Y,nprev,2,10,type="fixed")
ar11c=ar.rolling.window(Y,nprev,1,11,type="fixed")
ar11p=ar.rolling.window(Y,nprev,2,11,type="fixed")
ar12c=ar.rolling.window(Y,nprev,1,12,type="fixed")
ar12p=ar.rolling.window(Y,nprev,2,12,type="fixed")


bar1c=ar.rolling.window(Y,nprev,1,1,type="bic")
bar1p=ar.rolling.window(Y,nprev,2,1,type="bic")
bar2c=ar.rolling.window(Y,nprev,1,2,type="bic")
bar2p=ar.rolling.window(Y,nprev,2,2,type="bic")
bar3c=ar.rolling.window(Y,nprev,1,3,type="bic")
bar3p=ar.rolling.window(Y,nprev,2,3,type="bic")
bar4c=ar.rolling.window(Y,nprev,1,4,type="bic")
bar4p=ar.rolling.window(Y,nprev,2,4,type="bic")
bar5c=ar.rolling.window(Y,nprev,1,5,type="bic")
bar5p=ar.rolling.window(Y,nprev,2,5,type="bic")
bar6c=ar.rolling.window(Y,nprev,1,6,type="bic")
bar6p=ar.rolling.window(Y,nprev,2,6,type="bic")
bar7c=ar.rolling.window(Y,nprev,1,7,type="bic")
bar7p=ar.rolling.window(Y,nprev,2,7,type="bic")
bar8c=ar.rolling.window(Y,nprev,1,8,type="bic")
bar8p=ar.rolling.window(Y,nprev,2,8,type="bic")
bar9c=ar.rolling.window(Y,nprev,1,9,type="bic")
bar9p=ar.rolling.window(Y,nprev,2,9,type="bic")
bar10c=ar.rolling.window(Y,nprev,1,10,type="bic")
bar10p=ar.rolling.window(Y,nprev,2,10,type="bic")
bar11c=ar.rolling.window(Y,nprev,1,11,type="bic")
bar11p=ar.rolling.window(Y,nprev,2,11,type="bic")
bar12c=ar.rolling.window(Y,nprev,1,12,type="bic")
bar12p=ar.rolling.window(Y,nprev,2,12,type="bic")




### == juntando tudo ==   ����  ###

cpi=cbind(ar1c$pred,ar2c$pred,ar3c$pred,ar4c$pred,
          ar5c$pred,ar6c$pred,ar7c$pred,ar8c$pred,
          ar9c$pred,ar10c$pred,ar11c$pred,ar12c$pred)

pce=cbind(ar1p$pred,ar2p$pred,ar3p$pred,ar4p$pred,
          ar5p$pred,ar6p$pred,ar7p$pred,ar8p$pred,
          ar9p$pred,ar10p$pred,ar11p$pred,ar12p$pred)

bcpi=cbind(bar1c$pred,bar2c$pred,bar3c$pred,bar4c$pred,
           bar5c$pred,bar6c$pred,bar7c$pred,bar8c$pred,
           bar9c$pred,bar10c$pred,bar11c$pred,bar12c$pred)

bpce=cbind(bar1p$pred,bar2p$pred,bar3p$pred,bar4p$pred,
           bar5p$pred,bar6p$pred,bar7p$pred,bar8p$pred,
           ,sep=";",row.names = FALSE, col.names = FALSE)
write.table(pce,"ar-pce.csv",sep=";",row.names = FALSE, col.names = FALSE)

write.table(bcpi,"fbicar-cpi.csv",sep=";",row.names = FALSE, col.names = FALSE)
write.table(bpce,"bicar-pce.csv",sep=";",row.names = FALSE, col.names = FALSE)