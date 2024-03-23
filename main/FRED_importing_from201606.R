#install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir()

# 2021년 3월 현재 FRED-MD vintage 2021-02 사용 가능함 
# FRED-MD 변수의 수가 2016년 6월부터 128개로 변경됨  
# 예전 data들을 update하기 때문에 vintage별로 차이가 발생함(동일한 변수의 과거 동일한 시기 자료도 update될 수 있음) 

data = read.csv("2021-02.csv")   

tcode = data[1,]  # first element: Transformation code
tcode

### Transformation 

data = data[-1,]  # Transfrom 라인 제거 

tdata = data[-(1:2),]  #최초 2 observations 제거(이차 차분 고려)

head(tdata[,1:5])



for (i in 2:129){
  
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

IP = data[,7]   # Industrial Production total
plot(log(IP))

plot(diff(log(IP)))  #산업생산 증가율, tcode와 일치 

plot(tdata[,7])


#1978년 2월부터 2019년 12월까지 자료 사용, missing value 없는 자료를 원함. 

fdata = tdata[228:730,]

complete.cases(fdata[,59])  # missing values in ACOGNO

fdata = fdata[,-59] # ACOGNO 제거 
complete.cases(fdata)


Y = fdata[,2:128]

Y = cbind(fdata[,7],fdata[,c(-1,-7)]) # 산업생산지수 증가율을 y로 취급(첫번째 행)

mode(Y)

Y= as.matrix(Y)
mode(Y)



          