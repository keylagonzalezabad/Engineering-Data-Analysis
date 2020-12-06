####### Homework #8 ####### 
#Name: Keyla Gonzalez Abad

#Problem 1 
#In this problem, you will use support vector approaches to predict whether a 
#given car gets high or low gas mileage based on the Auto data set in the ISLR 
#package.
  #(a) Create a binary variable that takes on a 1 for cars with gas mileage 
      #above the median, and a 0 for cars with gas mileage below the median. 
      #Use this variable as response in the following analysis.
  #(b) Fit a support vector classifier to the data with various values of cost, 
      #to predict whether a car gets high or low gas mileage. Report the 
      #cross-validation errors associated with different values of this 
      #parameter. Comment on your results.
  #(c) Now repeat (b), this time using SVMs with radial and polynomial kernels, 
      #with different values of gamma, degree and cost. Comment on your results.

############ Libraries ############
library(ISLR)
library(e1071)
library(caTools) #Split
library(ggplot2) #Plotting
library(GGally) #Plotting

############ Load data ############
head(Auto) #Shows the first six lines of Auto data
summary(Auto) #Summary statistics for Auto data
names(Auto) #Names

med.mpg<-median(Auto$mpg); med.mpg #Median
mpg01<-ifelse(Auto$mpg>med.mpg,1,0); mpg01 #Binary variable
mpg01<-factor(mpg01); is.factor(mpg01) #Categorical variable
pred<-Auto[-c(1)]; head(pred) #Predictors

data<-cbind(mpg01, pred); head(data) #Dataframe with predictors and response
is.data.frame(data)

#Plot of response mpg01 and predictors (correlation, density, scatter plots)
ggpairs(data, columns=1:8)

############ Data Split ############
set.seed(1)
sample = sample.split(data$mpg01, SplitRatio = .75) #75% of training
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

############ Support Vector Classifier - Linear ############
svm.fit<-svm(mpg01~., data=train, kernel="linear",cost =10,scale =FALSE )

set.seed(2)
tune.out.lin=tune(svm, mpg01~.,data=train, kernel="linear",
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out.lin)

##Best model
bestmod.lin=tune.out.lin$best.model
summary(bestmod.lin)

#Prediction
ypred.lin=predict(bestmod.lin, test)
table(predict=ypred.lin,truth=test$mpg01)
accu.lin <- mean(test$mpg01 == ypred.lin); accu.lin

#Cost vs error
plot(tune.out.lin$performances[c(1,2)])

#According to the cross-validation (10-fold) errors associated with different 
#cost values, the best model is the one for a cost-value of 0.1 and a total of  
#88 support vectors. The corresponding cv error was 0.105 with a dispersion of 
#0.046 (summary(tune.out.lin)).

############ Support Vector Classifier - Radial Basis ############
set.seed(3)
tune.out.rad=tune(svm, mpg01~.,data=train, kernel="radial",
              ranges=list(gamma=2^(-1:1), cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out.rad)

##Best model
bestmod.rad=tune.out.rad$best.model
summary(bestmod.rad)

#Prediction
ypred.rad=predict(bestmod.rad, test)
table(predict=ypred.rad,truth=test$mpg01)
accu.rad <- mean(test$mpg01 == ypred.rad); accu.rad

#Cost vs error
plot(tune.out.rad$performances[c(2,3)])

#The best hyperparameters for this dataset were with a gamma-value of 1 and a 
#cost-value of 5. Moreover, a 10-fold cv was applied obtaining an error of 0.088
#with a dispersion of 0.0836

############ Support Vector Classifier - Polynomial ############
set.seed(4)
tune.out.pol=tune(svm, mpg01~.,data=train, kernel="polynomial",
              ranges=list(degree=c(3,4,5,6,7,8,9), gamma=2^(-1:1), 
                          cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out.pol)

##Best model
bestmod.pol=tune.out.pol$best.model
summary(bestmod.pol)

#Prediction
ypred.pol=predict(bestmod.pol, test)
table(predict=ypred.pol,truth=test$mpg01)
accu.pol <- mean(test$mpg01 == ypred.pol); accu.pol

#Cost vs error
plot(tune.out.pol$performances[c(3,4)])

#The best hyperparameters were with a 3rd degree polynomial model, a gamma-value 
#of 1 and a cost-value of 5. Moreover, a 10-fold cv was applied obtaining an 
#error of 0.088 with a dispersion of 0.0836.

#Problem 2
#This problem uses the OJ data set in the ISLR package.
#(a) Create a training set containing a random sample of 800 observations, and 
    #a test set containing the remaining observations.
#(b) Fit a support vector classifier to the training data using cost=0.01, with 
    #Purchase as the response and the other variables as predictors. Use the 
    #summary() function to produce summary statistics, and describe the results 
    #obtained.
#(c) What are the training and test error rates?
#(d) Use the tune() function to select an optimal cost. Consider value in the 
    #range 0.01 to 10.
#(e) Compute the training and test error rates using this new value for cost.
#(f) Repeat parts (b) through (e) using a support vector machine with a radial 
    #kernel. Use the tune() function to select an optimal cost and gamma.
#(g) Repeat parts (b) through (e) using a support vector machine with a 
    #polynomial kernel. Set degree=2. Use the tune() function to select an 
    #optimal cost.
#(h) Overall, which approach seems to give the best results on this data?

############ Load data ############
head(OJ) #Shows the first six lines of Auto data
summary(OJ) #Summary statistics for Auto data
names(OJ) #Names
dim=dim(OJ); dim

#Plot of response Purchase and predictors (correlation, density, scatter plots)
#ggpairs(OJ, columns=1:18)

############ Data Split ############
set.seed(1)
ratio = 800/dim[1]
sample = sample.split(OJ$Purchase, SplitRatio = ratio) 
train = subset(OJ, sample == TRUE)
test  = subset(OJ, sample == FALSE)

############ Support Vector Classifier - Linear ############
svm.fit.oj<-svm(Purchase~., data=train, kernel="linear",cost=0.01,scale=FALSE)
summary(svm.fit.oj)

#According to the summary statistics, there are 620 number of support vectors 
#(311 309) that helps to describe our linear decision boundary. It's important
#to mention that there are only two classes on this dataset.

#Prediction Train
ypred.oj.train=predict(svm.fit.oj, train)
table(predict=ypred.oj.train,truth=train$Purchase)
accu.oj.train <- mean(train$Purchase == ypred.oj.train); accu.oj.train
#Prediction Test
ypred.oj.test=predict(svm.fit.oj, test)
table(predict=ypred.oj.test,truth=test$Purchase)
accu.oj.test <- mean(test$Purchase == ypred.oj.test); accu.oj.test

#Tuning
set.seed(5)
tune.oj.lin=tune(svm, Purchase~.,data=train, kernel="linear",
                 ranges=list(cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.oj.lin)

##Best model
bestmod.lin.oj=tune.oj.lin$best.model
summary(bestmod.lin.oj)

#Prediction Train
ypred.tune.train=predict(bestmod.lin.oj, train)
table(predict=ypred.tune.train,truth=train$Purchase)
accu.tune.train <- mean(train$Purchase == ypred.tune.train); accu.tune.train
#Prediction Test
ypred.tune.test=predict(bestmod.lin.oj, test)
table(predict=ypred.tune.test,truth=test$Purchase)
accu.tune.test <- mean(test$Purchase == ypred.tune.test); accu.tune.test

#Cost vs error
plot(tune.oj.lin$performances[c(1,2)])

############ Support Vector Classifier - Radial Basis ############
svm.fit.oj.rad<-svm(Purchase~.,data=train,kernel="radial",cost=0.01,scale=FALSE)
summary(svm.fit.oj.rad)

#Prediction Train
ypred.oj.train.rad=predict(svm.fit.oj.rad, train)
table(predict=ypred.oj.train.rad,truth=train$Purchase)
accu.rad.train <- mean(train$Purchase == ypred.oj.train.rad); accu.rad.train
#Prediction Test
ypred.oj.test.rad=predict(svm.fit.oj.rad, test)
table(predict=ypred.oj.test.rad,truth=test$Purchase)
accu.rad.test <- mean(test$Purchase == ypred.oj.test.rad); accu.rad.test

#Tuning
set.seed(5)
tune.oj.rad=tune(svm, Purchase~.,data=train, kernel="radial",
                 ranges=list(gamma=2^(-1:1), cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.oj.rad)

##Best model
bestmod.rad.oj=tune.oj.rad$best.model
summary(bestmod.rad.oj)

#Prediction Train
ypred.tune.train.rad=predict(bestmod.rad.oj, train)
table(predict=ypred.tune.train.rad, truth=train$Purchase)
accu.rad.train.tune <- mean(train$Purchase == ypred.tune.train.rad); 
accu.rad.train.tune
#Prediction Test
ypred.tune.test.rad=predict(bestmod.rad.oj, test)
table(predict=ypred.tune.test.rad, truth=test$Purchase)
accu.rad.test.tune <- mean(test$Purchase == ypred.tune.test.rad); 
accu.rad.test.tune

#Cost vs error
plot(tune.oj.rad$performances[c(2,3)])

#Gamma vs error
plot(tune.oj.rad$performances[c(1,3)])

############ Support Vector Classifier - Polynomial ############
svm.fit.oj.pol<-svm(Purchase~., data=train, kernel="polynomial", cost=0.01,
                    degree=2, scale=FALSE)
summary(svm.fit.oj.pol)

#Prediction Train
ypred.oj.train.pol=predict(svm.fit.oj.pol, train)
table(predict=ypred.oj.train.pol, truth=train$Purchase)
accu.pol.train <- mean(train$Purchase == ypred.oj.train.pol); accu.pol.train
#Prediction Test
ypred.oj.test.pol=predict(svm.fit.oj.pol, test)
table(predict=ypred.oj.test.pol, truth=test$Purchase)
accu.pol.test <- mean(test$Purchase == ypred.oj.test.pol); accu.pol.test

#Tuning
set.seed(6)
tune.oj.pol=tune(svm, Purchase~.,data=train, kernel="polynomial",degree=2,
                 ranges=list(cost=c(0.001,0.01,0.1,1,5,10)))
summary(tune.oj.pol)

##Best model
bestmod.pol.oj=tune.oj.pol$best.model
summary(bestmod.pol.oj)

#Prediction Train
ypred.tune.train.pol=predict(bestmod.pol.oj, train)
table(predict=ypred.tune.train.pol, truth=train$Purchase)
accu.pol.train.tune <- mean(train$Purchase == ypred.tune.train.pol); 
accu.pol.train.tune
#Prediction Test
ypred.tune.test.pol=predict(bestmod.pol.oj, test)
table(predict=ypred.tune.test.pol, truth=test$Purchase)
accu.pol.test.tune <- mean(test$Purchase == ypred.tune.test.pol); 
accu.pol.test.tune

#Cost vs error
plot(tune.oj.pol$performances[c(1,2)])

#Overall, the best performance was obtained with the 2nd degree polynomial model
#Moreover, the best model had a cost-value of 5 and 377 support vectors. 

#1. Polynomial: train error=0.8412, test error=0.855555
#2. Linear: train error=0.8375, test error=0.837037
#3. Radial: train error=0.8662, test error=0.822222

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console
