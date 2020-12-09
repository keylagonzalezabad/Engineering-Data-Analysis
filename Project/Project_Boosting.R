#Boosting

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

############ Hyperparameter tuning - Boosting ############
#Perform 5-fold cross-validation repeated 3 times and to use gridsearch for 
#hyperparameter tuning
set.seed(3)
train.control<-trainControl(method="repeatedcv",
                            number=5,
                            repeats=3,
                            search="grid")
set.seed(6)
gbmGrid<-expand.grid(interaction.depth = c(1, 5, 9), 
                     n.trees = (1:200), 
                     shrinkage = c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.3),
                     n.minobsinnode = c(1:20))

gbm_gridsearch <- train(Mean.R. ~., 
                        data = train,
                        method = 'gbm',
                        metric = 'rmse',
                        tuneGrid = gbmGrid,
                        trControl=train.control)
print(gbm_gridsearch)
summary(gbm_gridsearch)
plot(gbm_gridsearch)

gbm.pred = predict(gbm_gridsearch, newdata=test); gbm.pred
plot(gbm.pred, test$Mean.R.)
abline (0,1)
MSE.gbm<-mean((gbm.pred-test$Mean.R.)^2); MSE.gbm

varImp(gbm_gridsearch)

############ Final Model - Boosting ############
library("readxl")
set.seed(3)
path.new <- #*** Copy your unseen test path***
new.test <- read_excel(path.new)
summary(new.test) #Summary statistics for new.test data
names(new.test) #Names

#Trained model
boost.final=gbm(Mean.R.~., data=mydata, n.trees =89, interaction.depth=9, 
                shrinkage=0.1, n.minobsinnode = 6)

#Train Predictions
boost.pred.all=predict(boost.final, newdata=mydata)
MSE.boost.all<-mean((boost.pred.all-mydata$Mean.R.)^2); MSE.boost.all
plot(boost.pred.all, mydata$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

#Test Predictions for Unseen data
boost.pred.new=predict(boost.final, newdata=new.test)
MSE.boost.new<-mean((boost.pred.new-new.test$`Mean(R)`)^2); MSE.boost.new

plot(boost.pred.new, new.test$`Mean(R)`, xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console