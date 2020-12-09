# MultiLinear Perceptron Neural Networks

############ Libraries ############
#install.packages('caret')
library(caTools) #Split
library(caret) #Hyperparameter tuning
library(corrplot) #Correlation plot
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

############ Hyperparameter tuning - MLP ############
set.seed(3)
train.control<-trainControl(method="repeatedcv",
                            number=5,
                            repeats=3,
                            search="grid")

set.seed(7)
mlp.Grid<-expand.grid(layer1 = c(1,2,5,8,10,12,15,18,20), 
                      layer2 = c(1,2,5,8,10,12,15,18,20), 
                      layer3 = c(1,2,5,8,10,12,15,18,20))

mlp_gridsearch <- train(Mean.R. ~., 
                        data = train,
                        method = 'mlpML',
                        metric = 'rmse',
                        preProc = c("center", "scale"),
                        tuneGrid = mlp.Grid,
                        trControl=train.control)

print(mlp_gridsearch)
summary(mlp_gridsearch)

mlp.pred = predict(mlp_gridsearch, newdata=test); mlp.pred
plot(mlp.pred, test$Mean.R.)
abline (0,1)
MSE.mlp<-mean((mlp.pred-test$Mean.R.)^2); MSE.mlp

############ Final Model - MLP ############
library("readxl")
set.seed(3)
path.new <- #*** Copy your unseen test path***
new.test <- read_excel(path.new)
summary(new.test) #Summary statistics for new.test data
names(new.test) #Names

scaled.pred <- preProcess(mydata[,c(2:67)], method = c("center","scale"))
scaled.pred.trans <- predict(scaled.pred, mydata[,c(2:67)]); 
Mean.R.<-mydata$Mean.R.; data<-cbind(Mean.R., scaled.pred.trans); data
mlp.Grid.all<-expand.grid(layer1 = 18, 
                          layer2 = 2, 
                          layer3 = 15)

mlp.all<- train(Mean.R. ~., 
                 data = data,
                 method = 'mlpML',
                 metric = 'rmse',
                 tuneGrid = mlp.Grid.all)

#Train Predictions
mlp.pred.all=predict(mlp.all, newdata=data)
MSE.mlp.all<-mean((mlp.pred.all-data$Mean.R.)^2); MSE.mlp.all
plot(mlp.pred.all, data$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

#Test Predictions for Unseen data
preproc_testdata <- predict(scaled.pred, new.test[,c(2:67)])
Mean.R.<-new.test$`Mean(R)`
data.test<-cbind(Mean.R., preproc_testdata); data.test

mlp.pred.new=predict(mlp.all, newdata=data.test)
MSE.mlp.new<-mean((mlp.pred.new-data.test$Mean.R.)^2); MSE.mlp.new
plot(mlp.pred.new, data.test$Mean.R., xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)1

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console
