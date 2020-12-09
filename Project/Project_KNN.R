#KNN R

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

############ Hyperparameter tuning - Random Forest ############
#Perform 5-fold cross-validation repeated 3 times and to use gridsearch for 
#hyperparameter tuning
set.seed(3)
train.control<-trainControl(method="repeatedcv",
                            number=5,
                            repeats=3,
                            search="grid")

knn_gridsearch = train(Mean.R.~.,
                    data = train,
                    method = "knn",
                    trControl = train.control,
                    tuneGrid = expand.grid(k = seq(1, 31, by = 2)))
                    
print(knn_gridsearch)
summary(knn_gridsearch)
plot(knn_gridsearch)

knn_gridsearch$bestTune #Best hyperparameter

#Train Predictions
knn.pred.train=predict(knn_gridsearch, newdata=train)
plot(knn.pred.train, train$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.knn.train<-mean((knn.pred.train-train$Mean.R.)^2); MSE.knn.train

#Test Predictions
knn.pred = predict(knn_gridsearch, newdata=test); knn.pred
plot(knn.pred, test$Mean.R., xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.knn<-mean((knn.pred-test$Mean.R.)^2); MSE.knn

############ Final Model - Random Forest ############
library("readxl")
set.seed(3)
path.new <- #*** Copy your unseen test path***
new.test <- read_excel(path.new)
summary(new.test) #Summary statistics for new.test data
names(new.test) #Names

#Trained model
knn.all = train(Mean.R.~.,
                data = mydata,
                method = "knn",
                tuneGrid = expand.grid(k = 7))

#Train Predictions
knn.pred.all=predict(knn.all, newdata=mydata)
MSE.knn.all<-mean((knn.pred.all-mydata$Mean.R.)^2); MSE.knn.all
plot(knn.pred.all, mydata$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

#Test Predictions for Unseen data
knn.pred.new=predict(knn.all, newdata=new.test)
MSE.knn.new<-mean((knn.pred.new-new.test$`Mean(R)`)^2); MSE.knn.new

plot(knn.pred.new, new.test$`Mean(R)`, xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console

