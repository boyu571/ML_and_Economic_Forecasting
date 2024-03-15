runnn=function(Y,indice,horizon){
  
  #dum=Y[,ncol(Y)]
  #Y=Y[,-ncol(Y)]
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+horizon)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*horizon))]  
  
  if(horizon==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(horizon-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  

  trainingframe = as.h2o(cbind(y=y,X))
  
  model = h2o.deeplearning(y = 'y',
                           training_frame = trainingframe,
                           activation = 'Rectifier',
                           hidden = c(32,16,8),
                           epochs = 100,
                           train_samples_per_iteration = -2,
                           seed = 1)
  
  xoutframe = t(c(NA,X.out))
  colnames(xoutframe) = colnames(trainingframe)
  xoutframe = as.h2o(xoutframe)
  
  y_pred = h2o.predict(model, newdata = xoutframe)
  pred = as.vector(y_pred)
  
  return(list("model"=model,"pred"=pred))
}


nn.rolling.window=function(Y,npred,indice=1,horizon=1){
  
  save.pred=matrix(NA,npred,1)
  for(i in npred:1){
    Y.window=Y[(1+npred-i):(nrow(Y)-i),]
    lasso=runnn(Y.window,indice,horizon)
    save.pred[(1+npred-i),]=lasso$pred
    cat("iteration",(1+npred-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-npred),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,npred)-save.pred)^2))
  mae=mean(abs(tail(real,npred)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors))
}

