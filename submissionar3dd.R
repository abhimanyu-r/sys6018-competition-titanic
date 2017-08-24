#Homework 1 - SYS 6018-001
#Name - Abhimanyu Roy
#UVa. Computing Id. - ar3dd
#imports the required packages
library(tidyverse)
library(plyr)

cvals<-read.csv("test.csv")
cvals$Pclass <- factor(cvals$Pclass)
cvals$Sex <- factor(cvals$Sex)
cvals$Embarked <-factor(cvals$Parch)
cvals$Parch <- factor(cvals$SibSp)
cvals$SibSp <-factor(cvals$Embarked)
#Runs logistic regression
predictor <- glm(Survived ~ Pclass + Sex + Age + Parch, data=cvals, family = "binomial")
summary(predictor)
probs<-as.vector(predict(predictor, type="response"))
preds <- rep(0,nrow(cvals))  # Initialize prediction vector

#Changes values greater than 0.5 to 1
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,cvals$Survived)
