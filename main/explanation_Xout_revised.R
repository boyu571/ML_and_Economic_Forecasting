runAR=function(Y,indice,lag,type="fixed"){
  
  Y2=cbind(Y[,indice])
  
  # 아래 부분은 old version으로 X.out을 정의할 때 horizon>1인 경우 오류가 있음 
  #aux=embed(Y2,4+horizon)
  #y=aux[,1]
  #X=aux[,-c(1:(ncol(Y2)*horizon))]  
  
  #if(horizon==1){
  #  X.out=tail(aux,1)[1:ncol(X)]  
  #}else{
  #  X.out=aux[,-c(1:(ncol(Y2)*(horizon-1)))]
  #  X.out=tail(X.out,1)[1:ncol(X)]
  #}
  
  
# Let's say time T=Dec 1989 and T+1=Jan 1990. The given window is until time T.
# To forecast 2-step ahead out-of-sample forecast of y_T+1 (Jan 1990),  X.out=[y_(T-1),y_(T-2),y_(T-3),y_(T-4)] in the code. However, note that the code uses y_T in estimation. 

# When practitioners produce a 2-step ahead forecast for Jan. 1990, they would typically use information up to Nov. 1989 in estimation and forecasting. This means that they do not use y_T (Dec. 1989) in estimation. Similarly, if one produces a 3-step ahead forecast for Jan. 1990, they would use information only up to Oct. 1989 without using y_T (Dec. 1989) and y_T-1 (Nov. 1989) in estimation and forecasting. However, this code  uses  y_T (Dec. 1989) and y_T-1 (Nov. 1989) in estimation even if  X.out=[y_(T-2),y_(T-3),y_(T-4),y_(T-5)]. 

# This may make a substantial difference especially when we consider a longer forecast horizon. That is, if we consider a 12-step ahead out-of-sample forecast, things become more obvious. 
  
  
  # X.out 부분을 수정
  X=embed(as.matrix(Y2),4)
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]
  Xout=X[nrow(X),]
  #Xout=t(as.vector(Xout))  # 이게 필요하나?
  
  y=tail(Y2[,1],nrow(Xin))
  X = Xin 
  X.out = Xout
  ##
  
  
  if(type=="fixed"){
    model=lm(y~X)
    coef=coef(model)
  }
  
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
}
