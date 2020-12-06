####### Homework #7 ####### 
#Name: Keyla Gonzalez Abad

#Problem 1 (10pt)
#In the lab, a classification tree was applied to the Carseats data set after 
#converting Sales into a binary response variable. This question will seek to 
#predict Sales using regression trees and related approaches, treating the 
#response as a quantitative variable (that is, without the conversion).
  #(a) Split the data set into a training set and a test set.
  #(b) Fit a regression tree to the training set. Plot the tree, and interpret 
      #the results. Then compute the test MSE.
  #(c) Prune the tree obtained in (b). Use cross validation to determine the 
      #optimal level of tree complexity. Plot the pruned tree and interpret the 
      #results. Compute the test MSE of the pruned tree. Does pruning improve 
      #the test error?
  #(d) Use the bagging approach to analyze the data. What test MSE do you 
      #obtain? Determine which variables are most important.
  #(e) Use random forests to analyze the data. What test MSE do you obtain? 
      #Determine which variables are most important.

############ Libraries ############
#install.packages("randomForest")
library (ISLR)
library (tree)
attach(Carseats)
library(caTools) #Split
library (randomForest)

############ Load data ############
summary(Carseats) #Summary statistics for Carseats data
names(Carseats) #Names
is.data.frame(Carseats) #Dataframe
dim(Carseats) #10 Predictors and 400 Samples

############ Data Split ############
set.seed(1)
sample = sample.split(Carseats$Sales, SplitRatio = .75) #75% of training
train = subset(Carseats, sample == TRUE)
test  = subset(Carseats, sample == FALSE)

############ Regression Tree Fit ############
#Fit
car.tr <- tree(Sales ~., train); car.tr
summary(car.tr)
plot(car.tr);  text(car.tr, cex=0.9, pretty=0)
#Prediction
car.pred.tr=predict(car.tr,test); car.pred.tr
#Test MSE
MSE<-mean((car.pred.tr-test$Sales)^2); MSE

############ Cross-validation ############
set.seed(2)
cv.car =cv.tree(car.tr); cv.car
names(cv.car)

#Visualize results
plot(cv.car$size ,cv.car$dev ,type='b')
min.cv=which.min(cv.car$dev); min.cv
best<-cv.car$size[min.cv]; best
points(min.cv, cv.car$size[min.cv], col="red",cex =2, pch =20)

############ Tree pruning ############
prune.car=prune.tree(car.tr, best=best)
plot(prune.car); text(prune.car, pretty=0)

car.pred.prune=predict(prune.car,test); car.pred.prune
plot(car.pred.prune,test$Sales)
abline (0,1)
MSE.prune<-mean((car.pred.prune-test$Sales)^2); MSE.prune

############ Bagging ############
bag.car=randomForest(Sales~., data=train, mtry=10, importance =TRUE)
bag.car

bag.pred = predict(bag.car, newdata=test); bag.pred
plot(bag.pred, test$Sales)
abline (0,1)
MSE.bag<-mean((bag.pred-test$Sales)^2); MSE.bag

#Importance plot
varImpPlot(bag.car)

############ Random Forest ############
#p/2
set.seed(3)
rf.car=randomForest(Sales~., data=train, mtry=5, importance =TRUE); rf.car
rf.pred = predict(rf.car, newdata=test); rf.pred

plot(rf.pred, test$Sales)
abline (0,1)
MSE.rf<-mean((rf.pred-test$Sales)^2); MSE.rf

importance(rf.car)

#Importance plot
varImpPlot(rf.car)

############ Range of values for mtry ############
MSE.rf.rang <- numeric(length = length(10))
for (i in 1:10){
  rf.car.rang=randomForest(Sales~., data=train, mtry=i, importance =TRUE); 
  rf.pred.rang=predict(rf.car.rang, newdata=test);
  MSE.rf.rang[i]<-mean((rf.pred.rang-test$Sales)^2);
}  

plot(1:10, MSE.rf.rang, type = "b")

############ Range of values for ntree ############
my_range <- 1:200 
MSE.rf.tr<- numeric(length = length(my_range))
for (i in my_range){
  rf.car.tr=randomForest(Sales~., data=train, mtry=5, ntree=i, importance =TRUE) 
  rf.car.tr
  rf.pred.tr = predict(rf.car.tr, newdata=test); rf.pred.tr
  MSE.rf.tr[i]<-mean((rf.pred.tr-test$Sales)^2); MSE.rf.tr
}  

plot(my_range, MSE.rf.tr, type = "b")

range <- seq(5, 200, by=5)
plot(range, MSE.rf.tr[range], type = "b")

#Problem 2 (5pt)
#In the lab, we applied random forests to the Boston data using mtry=6 and ntree=100.
  #(a) Consider a more comprehensive range of values for mtry: 1, 2,.,13. Given 
      #each value of mtry, find the test error resulting from random forests on 
      #the Boston data (using ntree=100). Create a plot displaying the test 
      #error rate vs. the value of mtry. Comment on the results in the plot.
  #(b) Similarly, consider a range of values for ntree (between 5 to 200). 
      #Given each value of ntree, find the test error resulting from random  
      #forests (using mtry=6). Create a plot displaying the test error vs. the 
      #value of ntree. Comment on the results in the plot.

library (MASS)
summary(Boston) #Summary statistics for Carseats data
names(Boston) #Names
is.data.frame(Boston) #Dataframe
dim(Boston) #13 Predictors and 506 Samples

############ Data Split ############
set.seed(4)
sample = sample.split(Boston$lstat, SplitRatio = .75) #75% of training
train = subset(Boston, sample == TRUE)
test  = subset(Boston, sample == FALSE)

############ Range of values for mtry ############
MSE.rf.Boston <- numeric(length = length(13))
for (i in 1:13){
  rf.bost.rang=randomForest(medv~., data=train, mtry=i, importance =TRUE); 
  rf.pred.bost=predict(rf.bost.rang, newdata=test);
  MSE.rf.Boston[i]<-mean((rf.pred.bost-test$medv)^2);
}  

plot(1:13, MSE.rf.Boston, type = "b")

############ Range of values for ntree ############
my_range <- 1:200 
MSE.rf.tr.Boston<- numeric(length = length(my_range))
for (i in my_range){
  rf.bost.tr=randomForest(medv~., data=train, mtry=5, ntree=i, importance =TRUE) 
  rf.pred.tr.bost = predict(rf.bost.tr, newdata=test); 
  MSE.rf.tr.Boston[i]<-mean((rf.pred.tr.bost-test$medv)^2); 
}  

plot(my_range, MSE.rf.tr.Boston, type = "b")

range <- seq(5, 200, by=5)
plot(range, MSE.rf.tr.Boston[range], type = "b")

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console

