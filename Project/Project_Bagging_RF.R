#1. Bagging
#2. Random Forest

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

#expand.grid creates all permutations
tune.grid<-expand.grid(mtry=1:66)

rf_gridsearch <- train(Mean.R. ~., 
                       data = train,
                       method = 'rf',
                       metric = 'rmse',
                       tuneGrid = tune.grid,
                       ntree=100,
                       trControl=train.control)

print(rf_gridsearch)
summary(rf_gridsearch)
plot(rf_gridsearch)

rf_gridsearch$bestTune #Best hyperparameter 

#mtry vs MSE
mtry = rf_gridsearch$results['mtry']
MSE.grid.mtry = (rf_gridsearch$results['RMSE'])^2
plot(data.frame(mtry, MSE.grid.mtry), ylab='MSE', type = "b",col = "dark green")

#Train Predictions
rf.pred.train=predict(rf_gridsearch, newdata=train)
plot(rf.pred.train, train$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.rf.train<-mean((rf.pred.train-train$Mean.R.)^2); MSE.rf.train

#Test Predictions
rf.pred = predict(rf_gridsearch, newdata=test); rf.pred
plot(rf.pred, test$Mean.R., xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)
MSE.rf<-mean((rf.pred-test$Mean.R.)^2); MSE.rf

#Predictors importance
rf.imp<-varImp(rf_gridsearch);rf.imp
plot(rf.imp, top = 5)
plot(rf.imp, top = 10)
plot(rf.imp, top = 20)

############ Range of values for ntree ############
set.seed(3)
my_range <- 1:200 
MSE.rf.tr<- numeric(length = length(my_range))
for (i in my_range){
  rf.tr=randomForest(Mean.R.~., data=train, mtry=11, 
                     ntree=i, importance =TRUE) 
  rf.pred.tr = predict(rf.tr, newdata=test); 
  MSE.rf.tr[i]<-mean((rf.pred.tr-test$Mean.R.)^2); 
}  

plot(my_range, MSE.rf.tr, type = "b", xlab = 'Number of Tree', 
     ylab='MSE',col = "dark green")

############ Final Model - Random Forest ############
library("readxl")
set.seed(3)
path.new <- #*** Copy your unseen test path***
new.test <- read_excel(path.new)
summary(new.test) #Summary statistics for new.test data
names(new.test) #Names

#Trained model
rf.all=randomForest(Mean.R.~., data=mydata, mtry=11, ntree=100, importance =TRUE)

#Train Predictions
rf.pred.all=predict(rf.all, newdata=mydata)
MSE.rf.all<-mean((rf.pred.all-mydata$Mean.R.)^2); MSE.rf.all
plot(rf.pred.all, mydata$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

#Test Predictions for Unseen data
rf.pred.new=predict(rf.all, newdata=new.test)
MSE.rf.new<-mean((rf.pred.new-new.test$`Mean(R)`)^2); MSE.rf.new

plot(rf.pred.new, new.test$`Mean(R)`, xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console
