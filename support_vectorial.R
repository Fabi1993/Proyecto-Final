library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)
library(e1071)

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

#clasification_mode
#default with factorresponse(cost=1)

model<-svm(ambiente~sensor1+sensor2+sensor3, data=train.data)
print(model)
summary(model)

#test with train data

pred<-predict(model,test.data)
pred
#check accuracy
table(pred, test.data$ambiente)

#increasing C to make the margin narrower
model<-svm(ambiente~sensor1+sensor2+sensor3, data=train.data,cost=100)
print(model)
summary(model)
pred<-predict(model,test.data)
pred
#check accuracy
table(pred, test.data$ambiente)






