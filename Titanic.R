
#rm(list=ls())
#setwd("~/Acad/Projects/Titanic")
library(tidyverse)
library(dplyr)

set.seed(10)

# Importing the dataset
dataset = tbl_df(read.csv('train.csv', stringsAsFactors = FALSE))
test_data = tbl_df(read.csv('test.csv', stringsAsFactors = FALSE))
summary(dataset)
glimpse(dataset)

#Pre-processing
#Function to clean the data
cleaning_df <- function(df){
  df$Title <- gsub("^.*, (.*?)\\..*$", "\\1", df$Name)
  df$PassengerId <- NULL
  df$Name <- NULL
  df$Ticket <- NULL
  df$Cabin <- NULL 
  df$Female <- as.factor(ifelse(df$Sex == 'female',1,0))
  df$Sex <- NULL
  
  #Title
  officer <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')
  royal <- c('Lady', 'the Countess','Sir', 'Jonkheer', 'Dona', 'Don')
  miss <- c('Mlle', 'Ms', 'Mme')
  
  df$Title <- ifelse(df$Title %in% royal, 'Royal', df$Title)
  df$Title <- ifelse(df$Title %in% officer, 'Officer', df$Title)
  df$Title <- ifelse(df$Title %in% miss, 'Miss', df$Title)
  df$Title <- as.factor(df$Title)
  
  #Family Size
  df$Family <- (df$SibSp + df$Parch + 1)
  
  #Embarked
  df$Embarked <- as.factor(ifelse(df$Embarked=="","S",df$Embarked))
  # df$Embarked_S <- as.factor(ifelse(df$Embarked == 'S',1,0))
  # df$Embarked_C <- as.factor(ifelse(df$Embarked == 'C',1,0))
  # df$Embarked_Q <- as.factor(ifelse(df$Embarked == 'Q',1,0))
  # df$Embarked <- NULL
  
  #Impute missing "Age" values
  agemodel <- lm(Age ~ Pclass + Family + Title + Embarked + SibSp + Parch, data = df)
  
  for(i in 1:nrow(df)){
    if(is.na(df[i, "Age"])){
      df[i, "Age"] = predict(agemodel, newdata = df[i, ])
    }
  }
  
  return(df)
}


#Clean the datasets
clean_train_data <- cleaning_df(dataset)
summary(clean_train_data)
glimpse(clean_train_data)

#clean_train_data[clean_train_data$Age < 0,]

clean_test_data <- cleaning_df(test_data)
summary(clean_test_data)
glimpse(clean_test_data)

#-------Random Forest ------
library(randomForest)
#clean_train_data[,-1]
#clean_train_data$pred <- NULL
TitanicTree <- randomForest(x= clean_train_data[,-1], y=as.factor(clean_train_data$Survived), data=clean_train_data)
summary(TitanicTree)
TitanicTree$importance

#Prediction on test data
clean_test_data$Survived <- predict(TitanicTree, clean_test_data)

#summary(test_data)
#summary(clean_train_data)
#creating submission

submission <- data.frame(PassengerId = test_data$PassengerId, Survived = clean_test_data$Survived)
write.csv(submission, file = "submission.csv", row.names = FALSE)


#-----Conditional Inference trees ------

library(party)
c_TitanicTree <- cforest(as.factor(Survived) ~ ., data = clean_train_data, controls = cforest_unbiased(ntree=2000, mtry=3))

#Prediction on test data using our tree
clean_test_data$Survived <- predict(c_TitanicTree, newdata = clean_test_data, OOB= TRUE, type = "response") 

#creating submission
submission <- data.frame(PassengerId = test_data$PassengerId, Survived = clean_test_data$Survived)
write.csv(submission, file = "submission.csv", row.names = FALSE)

