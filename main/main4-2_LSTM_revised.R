
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# LSTM Function  

# ==================================================================
# Install packages and Recall Library

#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)

#install.packages("tensorflow")  #관리자 권한으로 실행 
library(tensorflow)
#install_tensorflow()

#reticulate::py_discover_config()

#install.packages("keras")  #관리자 권한으로 실행
library(keras) 
#install_keras()

#tf$constant("hello")
#tf$version

# use_condaenv("r-tensorflow") if error loading tensorflow


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

load("rawdata.rda")

Y=dados

# ==================================================================
# US inflation forecasting example explanation

nprev=132
npred = nprev
indice = 1
horizon = 1
lag = horizon

# FINAL CHECK COMPLETE

source("functions/func-lstm.R")

lstm_1 <- mul.lstm.rolling.window(Y,npred,1,1)  
lstm_1$errors 

lstm_2 <- mul.lstm.rolling.window(Y,npred,1,2)  
lstm_2$errors 

lstm_3 <- mul.lstm.rolling.window(Y,npred,1,3)  
lstm_3$errors 

lstm_4 <- mul.lstm.rolling.window(Y,npred,1,4)  
lstm_4$errors

lstm_5 <- mul.lstm.rolling.window(Y,npred,1,5)  
lstm_5$errors 

lstm_6 <- mul.lstm.rolling.window(Y,npred,1,6)  
lstm_6$errors 

lstm_7 <- mul.lstm.rolling.window(Y,npred,1,7)  
lstm_7$errors 

lstm_8 <- mul.lstm.rolling.window(Y,npred,1,8)  
lstm_8$errors 

lstm_9 <- mul.lstm.rolling.window(Y,npred,1,9)  
lstm_9$errors 

lstm_10 <- mul.lstm.rolling.window(Y,npred,1,10)  
lstm_10$errors 

lstm_11 <- mul.lstm.rolling.window(Y,npred,1,11)  
lstm_11$errors 

lstm_12 <- mul.lstm.rolling.window(Y,npred,1,12)  
lstm_12$errors 


save.image("results_LSTM.RData")
