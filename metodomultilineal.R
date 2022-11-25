library(tidyverse)
library(caret)
library(readr)
library(class)
library(gmodels)

dataframe <- read_delim("Universidad/electiva/dataframe3/dataExcel.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
plot(dataframe[1:4])

summary(dataframe)

#hold-out cross-validation
sample.index <- sample(1:nrow(dataframe)
                       
                       , nrow(dataframe) * 0.75, replace = FALSE)

training.data <-dataframe[sample.index,, drop=F]
test.data<- dataframe[-sample.index,, drop=F]
#creating a model with 4 predictors
model3 <- lm(ambiente~sensor1+sensor2+sensor3,training.data)

# Make predictions and compute the R2, RMSE and MAE using CARET
predictions <- model3 %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$ambiente),
            RMSE = RMSE(predictions, test.data$ambiente),
            MAE = MAE(predictions, test.data$ambiente))

predict(model3,test.data)
#--------------------------------------------------------------------#
# Define training control
set.seed(1)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model3 <- train(ambiente~sensor1+sensor2+sensor3, data = dataframe, method = "lm",
                trControl = train.control)

# Summarize the results
print(model3)

prueba<- read_delim("Universidad/electiva/dataframe3/prueba.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
predict(model3,prueba)
