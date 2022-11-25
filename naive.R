library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)
library(e1071)

dataframe2 <- read_delim("Universidad/electiva/dataframe3/dataframe2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

##parto el dataframe cross validacion
train_index <- sample(1:nrow(dataframe2)
                      ,nrow(dataframe2)*0.7
                      ,replace = F)

train.data<-dataframe2[train_index,]
test.data<-dataframe2[-train_index,]

nb.model<-naiveBayes(ambiente~.,data=train.data , laplace=1)
prediction.nb<- predict(nb.model, test.data, type= "class")
table(test.data$ambiente, prediction.nb)
caret::confusionMatrix(prediction.nb,as.factor(test.data$ambiente))



prueba<- read_delim("Universidad/electiva/dataframe3/prueba.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
predict(nb.model,prueba)
