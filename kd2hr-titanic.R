# Name - Kanika Dawar
# Computing ID - kd2hr
# Titanic Kaggle submission assignment 1

library(tidyverse)
library(readr)  
library(dplyr)  

#setwd('C:/Users/Kanika/Downloads/FALL 2018/SYS 6018-001/Titanic')

#------------------------------------------------------------------------

titanic_train = read_csv("train.csv")

#converting variables categorial to factors
titanic_train$Pclass <- factor(titanic_train$Pclass)
titanic_train$Embarked <- factor(titanic_train$Embarked)
titanic_train$Sex <- factor(titanic_train$Sex)

# Let's subset so that we can cross validate
sub <- sample(1:891, size=600) # Select 600 record (row) numbers at random
titanic.train <- titanic_train[sub,]
titanic.valid <- titanic_train[-sub,]  # The set to test the models on

titanic.lg <- glm(Survived ~ Pclass + Sex + Age + SibSp, data=titanic.train, family = "binomial")
summary(titanic.lg)

# Let's test the full model on the training set
# to see how we do predicting.
probs<-as.vector(predict(titanic.lg, newdata = titanic.valid, type="response"))
preds <- rep(0,291)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds,titanic.valid$Survived)

#most suitable model on main train dataset (after running iterations with changing samples and variable combinations)
titanic.lg <- glm(Survived ~ Pclass + Sex + Age + SibSp, data=titanic_train, family = "binomial")
summary(titanic.lg)

#reading in test dataset
titanic_test = read_csv("test.csv")

#converting variables categorial to factors
titanic_test$Pclass <- factor(titanic_test$Pclass)
titanic_test$Embarked <- factor(titanic_test$Embarked)
titanic_test$Sex <- factor(titanic_test$Sex)

#prediction
probs<-as.vector(predict(titanic.lg, newdata = titanic_test, type="response"))
preds <- rep(0,418)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
table(preds)

pred_final <- cbind.data.frame(titanic_test$PassengerId, preds)

#writing final prediction
write.table(pred_final, file = "kd2hr-titanic-submission.csv", row.names=F, col.names=c("PassengerID","Survived"), sep=",")

