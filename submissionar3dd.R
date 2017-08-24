#Homework 1 - SYS 6018-001
#Name - Abhimanyu Roy
#UVa. Computing Id. - ar3dd
#imports the required packages
library(tidyverse)
library(plyr)

#Changing the variables to factor type
cvals<-read.csv("train.csv")
test<-read.csv("test.csv")
cvals$Pclass <- factor(cvals$Pclass)
cvals$Sex <- factor(cvals$Sex)
cvals$Embarked <-factor(cvals$Parch)
cvals$Parch <- factor(cvals$SibSp)
cvals$SibSp <-factor(cvals$Embarked)

test$Pclass <- factor(test$Pclass)
test$Sex <- factor(test$Sex)
test$Embarked <-factor(test$Parch)
test$Parch <- factor(test$SibSp)
test$SibSp <-factor(test$Embarked)
#Runs logistic regression
predictor <- glm(Survived ~ Pclass + Sex + Age , data=cvals, family = "binomial")
summary(predictor)
probs<-as.vector(predict(predictor, type="response"))
preds <- rep(0,nrow(cvals))  # Initialize prediction vector

#Changes values greater than 0.5 to 1
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
#Finds the number of correct predictions
table(preds,cvals$Survived)

#Predictions for the test data
probs2<-as.vector(predict(predictor, test, type="response"))
preds2 <- rep(0,nrow(test))  # Initialize prediction vector
#Changes values greater than 0.5 to 1
preds2[probs2>0.5] <- 1 # p>0.5 -> 1
test$Survived<-preds2
test2<-test[c("PassengerId", "Survived")]
write.csv(test2, "predictions.csv", row.names = FALSE)