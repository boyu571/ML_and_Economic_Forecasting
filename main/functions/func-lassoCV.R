runlasso=function(Y,indice,horizon,alpha=1,type="lasso"){
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  
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
  
  
  X=embed(as.matrix(Y2),4)
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]
  Xout=X[nrow(X),]
  #Xout=t(as.vector(Xout))  # 이게 필요하나?
  
  y=tail(Y2[,1],nrow(Xin))
  X = Xin 
  X.out = Xout
  ##
  
  #model=ic.glmnet(X,y,alpha = alpha)
  #coef=model$coef
  #if(type=="adalasso"){
  #  penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-1)
  #  model=ic.glmnet(X,y,penalty.factor = penalty,alpha=alpha)
  #}

  set.seed(4)
  model = cv.glmnet(X,y,family='gaussian',alpha=alpha)  

  if(type=="adalasso"){
    penalty=(abs(coef(model)[-1])+1/sqrt(length(y)))^(-1)    
    set.seed(4)
    model=cv.glmnet(X,y,penalty.factor = penalty,alpha=alpha)
  }

# pred=predict(model,X.out)
  pred=coef(model)[1]+X.out%*%(coef(model)[-1])
  
  return(list("model"=model,"pred"=pred))
}


lasso.rolling.window=function(Y,npred,indice=1,horizon=1,alpha=1,type="lasso"){
  
  save.coef=matrix(NA,npred-horizon+1,21+ncol(Y[,-1])*4)
  save.pred=matrix(NA,npred-horizon+1,1)
  for(i in npred:horizon){
    Y.window=Y[(1+npred-i):(nrow(Y)-i),]
    lasso=runlasso(Y.window,indice,horizon,alpha,type)
    #save.coef[(1+npred-i),]=lasso$model$coef
    save.pred[(1+npred-i),]=lasso$pred
    cat("iteration",(1+npred-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-npred+horizon-1),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,npred-horizon+1)-save.pred)^2))
  mae=mean(abs(tail(real,npred-horizon+1)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
}

