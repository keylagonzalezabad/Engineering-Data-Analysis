#SVM

############ Libraries ############
#install.packages('tidyverse')
library(caTools) #Split
library(caret) #Hyperparameter tuning
library(corrplot) #Correlation plot
library (randomForest)
library(gbm)
library(tidyverse)
library(ggplot2) #Plotting
library(GGally) #Plotting
library(car) #VIF
library(leaps) #Best subset selection
library(e1071)

############ Load data ############
path <- #*** Copy your train path***
mydata <- read.table(path, header=TRUE, sep=",")

summary(mydata) #Summary statistics for mydata data
names(mydata) #Names
is.data.frame(mydata) #Dataframe
dim(mydata) #66 Predictors and 152 Samples

############ Data Split ############
set.seed(1)
sample = sample.split(mydata$Mean.R., SplitRatio = .75) #75% of training
train = subset(mydata, sample == TRUE)
test  = subset(mydata, sample == FALSE)

############ Support Vector Classifier - Linear ############
svm.fit<-svm(Mean.R.~., data=train, kernel="linear",cost=0.01,scale=FALSE)
summary(svm.fit)

#Prediction Train
ypred.train=predict(svm.fit, train)
accu.train<-mean((ypred.train-train$Mean.R.)^2); accu.train

#Prediction Test
ypred.test=predict(svm.fit, test)
accu.test<-mean((ypred.test-test$Mean.R.)^2); accu.test

#Tuning
set.seed(3)
train.control<-tune.control(random = FALSE, nrepeat = 3, sampling = "cross", 
                            cross=5,  best.model = TRUE)

set.seed(5)
tune.lin=tune(svm, Mean.R.~.,data=train, kernel="linear", tunecontrol=train.control,
                 ranges=list(cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.lin)

##Best model
bestmod.lin=tune.lin$best.model
summary(bestmod.lin)

#Prediction Train
ypred.tune.train=predict(bestmod.lin, train)
plot(ypred.tune.train, train$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.svm.train<-mean((ypred.tune.train-train$Mean.R.)^2); MSE.svm.train
#Prediction Test
ypred.tune.test=predict(bestmod.lin, test)
plot(ypred.tune.test, test$Mean.R., xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.svm<-mean((ypred.tune.test-test$Mean.R.)^2); MSE.svm

#Cost vs error
plot(tune.lin$performances[c(1,2)])

############ Support Vector Classifier - Radial Basis ############
#Tuning
set.seed(5)
tune.rad=tune(svm, Mean.R.~.,data=train, kernel="radial", tunecontrol=train.control,
                 ranges=list(gamma=2^(-1:1), cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.rad)

##Best model
bestmod.rad=tune.rad$best.model
summary(bestmod.rad)

#Prediction Train
ypred.tune.train.rad=predict(bestmod.rad, train)
plot(ypred.tune.train.rad, train$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.svm.train.rad<-mean((ypred.tune.train.rad-train$Mean.R.)^2); MSE.svm.train.rad
#Prediction Test
ypred.tune.test.rad=predict(bestmod.rad, test)
plot(ypred.tune.test.rad, test$Mean.R., xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.svm.rad<-mean((ypred.tune.test.rad-test$Mean.R.)^2); MSE.svm.rad

#Cost vs error
plot(tune.rad$performances[c(2,3)])

#Gamma vs error
plot(tune.rad$performances[c(1,3)])

############ Support Vector Classifier - Polynomial ############
#Tuning
set.seed(6)
tune.pol=tune(svm, Mean.R.~.,data=train, kernel="polynomial",
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10), degree=c(2,3,4,5,6)),
              tunecontrol=train.control)

tunecontrol=train.control
summary(tune.pol)

##Best model
bestmod.pol=tune.pol$best.model
summary(bestmod.pol)

#Prediction Train
ypred.tune.train.pol=predict(bestmod.pol, train)
plot(ypred.tune.train.pol, train$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.svm.train.pol<-mean((ypred.tune.train.pol-train$Mean.R.)^2); MSE.svm.train.pol
#Prediction Test
ypred.tune.test.pol=predict(bestmod.pol, test)
plot(ypred.tune.test.pol, test$Mean.R., xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.svm.pol<-mean((ypred.tune.test.pol-test$Mean.R.)^2); MSE.svm.pol

#Cost vs error
plot(tune.pol$performances[c(1,2)])

############ Final Model - SVM ############
library("readxl")
set.seed(3)
path.new <- #*** Copy your unseen test path***
new.test <- read_excel(path.new)
summary(new.test) #Summary statistics for new.test data
names(new.test) #Names

#Trained model
svm.all=svm(Mean.R.~., data=mydata, kernel="polynomial",cost=1, degreee=2,
            gamma=0.01515152, coef.0=0, epsilon=0.1, scale=FALSE)

#Train Predictions
svm.pred.all=predict(svm.all, newdata=mydata)
MSE.svm.all<-mean((svm.pred.all-mydata$Mean.R.)^2); MSE.svm.all
plot(svm.pred.all, mydata$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

#Test Predictions for Unseen data
svm.pred.new=predict(svm.all, newdata=new.test)
MSE.svm.new<-mean((svm.pred.new-new.test$`Mean(R)`)^2); MSE.svm.new

plot(svm.pred.new, new.test$`Mean(R)`, xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console
