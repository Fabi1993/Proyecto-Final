library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)
library(rpart)

dataframe2 <- read_delim("Universidad/electiva/dataframe3/dataframe2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

##parto el dataframe cross validacion
train_index <- sample(1:nrow(dataframe2)
                      ,nrow(dataframe2)*0.7
                      ,replace = F)
train.data<-dataframe2[train_index,]
test.data<-dataframe2[-train_index,]

fit<-rpart(ambiente~sensor1+sensor2+sensor3
           ,method="class"
           ,data = train.data )
predict(fit,test.data)


plot(fit,uniform=T,margin=0.1)
text(fit,use.n=TRUE, all= TRUE, cex= 0.8)
rpart.plot::rpart.plot(fit)


prueba<- read_delim("Universidad/electiva/dataframe3/prueba.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
predict(fit,prueba)

