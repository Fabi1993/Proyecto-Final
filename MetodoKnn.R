library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)

#leo mi datframe
dataframe2 <- read_delim("Universidad/electiva/dataframe3/dataframe2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#--Funcion de normalizacion---#
normalise <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#----Cambio a factor mi clase----#
dataframe2$ambiente <-
  as.factor(dataframe2$ambiente)
#---ploteo mi dataframe----#
#plot(dataframe2[1:5])

#---miro las proporciones de mi datframe-#
prop.table(table(dataframe2$ambiente))

#-----#

hist(dataframe2$`sensor1`, breaks = 50)
hist(dataframe2$`sensor2`, breaks = 50)
hist(dataframe2$`sensor3`, breaks = 50)

#--normalizo las variables--#

normData <- dataframe2
standardData <- dataframe2

#--minmax--#
### min-max
normData$sensor1 <-
  normalise(dataframe2$sensor1)

normData$sensor2 <-
  normalise(dataframe2$`sensor2`)

normData$sensor3 <-
  normalise(dataframe2$`sensor3`)


#-- z- score-#
standardData$sensor1 <-
  scale(dataframe2$`sensor1`)
standardData$sensor2 <-
  scale(dataframe2$`sensor2`)
standardData$sensor3 <-
  scale(dataframe2$`sensor3`)
#----Parto mi dataframe----#
sample.index <- sample(1:nrow(dataframe2)
                       ,nrow(dataframe2)*0.7
                       ,replace = F)

#--entrenamiento y test---#
k <- 5
predictors <- c("sensor1","sensor2","sensor3")

# original data
train.data <-dataframe2[sample.index,c(predictors,"ambiente"),drop=F]
test.data <-dataframe2[-sample.index,c(predictors,"ambiente"),drop=F]
#-- cargo mi clase library---#
prediction <- knn(train = train.data[predictors], test = test.data[predictors],cl = train.data$ambiente, k=k)
#---Gmoldes---#

CrossTable(x = test.data$ambiente, y = prediction
           
           , prop.chisq = F)



#--------Normalizado--------#

train.data <-normData[sample.index,c(predictors,"ambiente"),drop=F]
test.data <-normData[-sample.index,c(predictors,"ambiente"),drop=F]
#-- cargo mi clase library---#
prediction <- knn(train = train.data[predictors]
                  
                  , test = test.data[predictors]
                  ,cl = train.data$ambiente, k=k)
#---Gmoldes---#
CrossTable(x = test.data$ambiente, y = prediction
           
           , prop.chisq = F)



#-----------StandarDAta--------#

train.data <-standardData[sample.index,c(predictors,"ambiente"),drop=F]
test.data <-standardData[-sample.index,c(predictors,"ambiente"),drop=F]
#-- cargo mi clase library---#
prediction <- knn(train = train.data[predictors]
                  
                  , test = test.data[predictors]
                  ,cl = train.data$ambiente, k=k)
#---Gmoldes---#
CrossTable(x = test.data$ambiente, y = prediction
           
           , prop.chisq = F)
