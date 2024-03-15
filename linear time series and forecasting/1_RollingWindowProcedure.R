##### Importing data and OLS estimation #####

## set your working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#dir()


## Loading data


# Importing a csv file
data = as.matrix(read.csv("DataHousingPrice.csv",header=T))

y = cbind(data[,1])   # or y = matrix(data[,1])
x = cbind(data[,2:4])
head(x)

fit0 = lm(y~x)
summary(fit0)

yhat=fitted(fit0) # fitted value of y
coef(fit0)
?coef
betahat = cbind(coef(fit0)) # coefficient estimates


# Rolling window estimation and saving the last fitted values

# sample size 88, rolling window size 60, 29 rolling estimation 


nf = 29   # number of estimation
olsRol = matrix(0,nf,4)
yhatRol = matrix(0,nf,1)

for (i in 1:nf){
  yi = y[i:(59+i)]  # window size is set to be 60
  xi = x[i:(59+i),]

  fiti = lm(yi~xi)
  yhat = fitted(fiti)     # fitted value of yi
  yhatRol[i]= yhat[59+1]  # last fitted value of y
  
  olsRol[i,]=coef(fiti) 
}
cbind(yhatRol, olsRol)


## save the fitted values
write.csv(cbind(yhatRol, olsRol),"result1.csv")
dir()


####################################
#The same result using matrix
####################################


### Matrix facilities ###
# diag(k) : k x k identity matrix
# entry-wise multiplication : *
# matrix multiplication : %*%
# entry-wise division : /
# transpose : t(A)
# inverse : solve(A) if A is square matrix
# cross product(A'B) : crossprod(A,B)
# outer product(AB') : A%o%B



olsRolMat = matrix(0,nf,4)
yhatRolMat = matrix(0,nf,1)

X = cbind(1,data[,2:4]) # X matrix

for (i in 1:nf){
  yi = y[i:(59+i)]  # window size is set to be 60
  xi = X[i:(59+i),]

  invxi = solve(crossprod(xi))
  olsbi = invxi%*%crossprod(xi,yi)  # OLS estimates

  yhatRolMat[i]=X[(59+i),]%*%olsbi   

  olsRolMat[i,]=t(olsbi)  #t: transpose 
}

cbind(yhatRol, yhatRolMat)
cbind(olsRol, olsRolMat)
