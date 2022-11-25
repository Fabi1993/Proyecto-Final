library(randomForest)
library(readr)
set.seed(123)

dataframe2 <- read_delim("Universidad/electiva/dataframe3/dataExcel.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

##parto el dataframe cross validacion
train_index <- sample(1:nrow(dataframe2)
                      ,nrow(dataframe2)*0.7
                      ,replace = F)
train.data<-dataframe2[train_index,]
test.data<-dataframe2[-train_index,]

fit.rf<-randomForest(ambiente~sensor1+sensor2+sensor3
                ,data=train.data)
prediccion.rf<-predict(fit.rf,test.data)
output<-data.frame(test.data$ambiente,prediccion.rf)

#Error_cuadratico_medio
RMSE=sqrt(sum((output$test.data.ambiente-output$prediccion.rf)^2)/nrow(output))
RMSE

#------------------#--------------------------#--------------------#


library(randomForest)
library(readr)
set.seed(123)

dataframe2 <- read_delim("Universidad/electiva/dataframe3/dataframe2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
dataframe2$ambiente <-
  as.factor(dataframe2$ambiente)

##parto el dataframe cross validacion
train_index <- sample(1:nrow(dataframe2)
                      ,nrow(dataframe2)*0.7
                      ,replace = F)
train.data<-dataframe2[train_index,]
test.data<-dataframe2[-train_index,]

fit.rf<-randomForest(ambiente~sensor1+sensor2+sensor3
                     ,data=train.data)
prediccion.rf<-predict(fit.rf,test.data)
output<-data.frame(test.data$ambiente,prediccion.rf)

table(test.data$ambiente, prediccion.rf)


prueba<- read_delim("Universidad/electiva/dataframe3/prueba.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
predict(fit.rf,prueba)
