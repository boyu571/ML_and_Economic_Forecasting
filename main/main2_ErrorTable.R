###############################################################################
### Forecasting Korean Inflation, BOK research project by Heejoono Han      ###
###############################################################################

###################################
### 5. Making forecast error tables
###################################

rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(stringr)
library(openxlsx)

load("results_forecasts.RData") 

stack <- NULL

for (i in 1:2) {
#1.random walk  
rw = cbind(rw1$errors[i],rw2$errors[i],rw3$errors[i],rw4$errors[i],rw5$errors[i],rw6$errors[i],
           rw7$errors[i],rw8$errors[i],rw9$errors[i],rw10$errors[i],rw11$errors[i],rw12$errors[i])
#2. ar
ar = cbind(ar1$errors[i],ar2$errors[i],ar3$errors[i],ar4$errors[i],ar5$errors[i],ar6$errors[i],
           ar7$errors[i],ar8$errors[i],ar9$errors[i],ar10$errors[i],ar11$errors[i],ar12$errors[i])

#3. ridge regression
ridge = cbind(ridge1$errors[i],ridge2$errors[i],ridge3$errors[i],ridge4$errors[i],ridge5$errors[i],ridge6$errors[i],
              ridge7$errors[i],ridge8$errors[i],ridge9$errors[i],ridge10$errors[i],ridge11$errors[i],ridge12$errors[i])

#4. lasso
lasso = cbind(lasso1$errors[i],lasso2$errors[i],lasso3$errors[i],lasso4$errors[i],lasso5$errors[i],lasso6$errors[i],lasso7$errors[i],
              lasso8$errors[i]  ,lasso9$errors[i],lasso10$errors[i],lasso11$errors[i],lasso12$errors[i])

#5. adaptive lasso
adalasso = cbind(adalasso1$errors[i],adalasso2$errors[i],adalasso3$errors[i],adalasso4$errors[i],
                 adalasso5$errors[i],adalasso6$errors[i],adalasso7$errors[i],adalasso8$errors[i],
                 adalasso9$errors[i],adalasso10$errors[i],adalasso11$errors[i],adalasso12$errors[i])

#6. elastic net
elasticnet = cbind(elasticnet1$errors[i],elasticnet2$errors[i],elasticnet3$errors[i],elasticnet4$errors[i],
                   elasticnet5$errors[i],elasticnet6$errors[i],elasticnet7$errors[i],elasticnet8$errors[i],
                   elasticnet9$errors[i],elasticnet10$errors[i],elasticnet11$errors[i],elasticnet12$errors[i])

#7. adaptive elastic net 
adaelasticnet = cbind(adaelasticnet1$errors[i],adaelasticnet2$errors[i],adaelasticnet3$errors[i],adaelasticnet4$errors[i],
                      adaelasticnet5$errors[i],adaelasticnet6$errors[i],adaelasticnet7$errors[i],adaelasticnet8$errors[i],
                      adaelasticnet9$errors[i],adaelasticnet10$errors[i],adaelasticnet11$errors[i],adaelasticnet12$errors[i])

#8. complete subset regression (CSR)
csr = cbind(csr1$errors[i],csr2$errors[i],csr3$errors[i],csr4$errors[i],csr5$errors[i],csr6$errors[i],
            csr7$errors[i],csr8$errors[i],csr9$errors[i],csr10$errors[i],csr11$errors[i],csr12$errors[i])

#. random forest (RF)
rf = cbind(rf1$errors[i],rf2$errors[i],rf3$errors[i],rf4$errors[i],rf5$errors[i],rf6$errors[i],
           rf7$errors[i],rf8$errors[i],rf9$errors[i],rf10$errors[i],rf11$errors[i],rf12$errors[i])

#10. random forest (RF)
rf = cbind(rf1$errors[i],rf2$errors[i],rf3$errors[i],rf4$errors[i],rf5$errors[i],rf6$errors[i],
           rf7$errors[i],rf8$errors[i],rf9$errors[i],rf10$errors[i],rf11$errors[i],rf12$errors[i])

df <-  rbind(rw, ar, ridge, lasso, adalasso, elasticnet, adaelasticnet,  csr, tfact, rf) %>% as.data.frame()


#df = round(df, digit=4)

stack <- rbind(stack, df)
}

colnames(stack) <- seq(1,12)

nModel = 10 # number of model
error_rmse <- stack[1:nModel,]
error_mae <- stack[(nModel+1):(nModel*2),]

rownames(error_rmse) <- c('rw', 'ar','ridge', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet',  'csr',  'tfact', 'rf')
rownames(error_mae) <- c('rw', 'ar','ridge', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet',  'csr', 'tfact', 'rf')

sheets <- list("error_rmse" = error_rmse, "error_mae" = error_mae)
write.xlsx(sheets, file = "errortable.xlsx", rowNames=TRUE)
#####








