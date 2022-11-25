library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)

dataframe2 <- read_delim("Universidad/electiva/dataframe3/dataframe2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

##creo mis dummy var variable clase
dataframe2$iscaja1 <- dataframe2$ambiente=="caja 1"
dataframe2$iscaja2 <- dataframe2$ambiente=="caja 2"
dataframe2$iscaja3 <- dataframe2$ambiente=="caja 3"

##parto el dataframe cross validacion
train_index <- sample(1:nrow(dataframe2)
                      ,nrow(dataframe2)*0.7
                      ,replace = F)
train.data<-dataframe2[train_index,]
test.data<-dataframe2[-train_index,]

#Entrenar los regresores logisticos para caja 1
caja1Classifier<-glm(iscaja1~sensor1+sensor2+sensor3
                     ,data = train.data
                     ,family = "binomial")
#Entrenar los regresores logisticos para caja 2
caja2Classifier<-glm(iscaja2~sensor1+sensor2+sensor3,
                     data=train.data, 
                     family = "binomial")
#Entrenar los regresores logisticos para caja 3

caja3Classifier<-glm(iscaja3~sensor1+sensor2+sensor3,
                     data=train.data, 
                     family = "binomial")


#Predecir la clase de los datos de prueba


aux<- predict(caja1Classifier
              ,test.data
              ,tipe= "response")
prediciones<-data.frame(caja1=aux)

prediciones$caja1<-predict(caja1Classifier
                           ,test.data
                           ,type="response")

prediciones$caja2<-predict(caja2Classifier
                           ,test.data
                           ,type="response")
prediciones$caja3<-predict(caja3Classifier
                           ,test.data
                           ,type="response")


colnames(prediciones)[max.col(prediciones)]


#con Caret


set.seed(123)

fit.control<-trainControl(method = "cv"
                          , number = 10)

fit <- train(ambiente ~ sensor1+sensor2+sensor3, data = dataframe2
             
             , method = "multinom", trControl = fit.control, trace = FALSE)

fit


predicted.class<-predict(fit, test.data, type = "prob")
colnames(predicted.class)[max.col(predicted.class)]



mlogit.prob <- predict(fit, test.data, type = "prob")
predicted.class <- colnames(mlogit.prob)[max.col(mlogit.prob)]
predicted.class


#----Para ingresar nuevos Datos---#
prueba <- read_delim("Universidad/electiva/dataframe3/prueba.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)


colnames(prueba)<-colnames(dataframe2)[1:3]

predict(fit, prueba, type="prob")

