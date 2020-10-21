####### Homework #5 ####### 
#Name: Keyla Gonzalez Abad

#Problem 1 (14pt)
#In this problem, you will develop a model to predict whether a given car gets 
#high or low gas mileage based on the Auto data set.
  #(a) Create a binary variable, mpg01, that contains a 1 if mpg contains a 
      #value above its median, and a 0 if mpg contains a value below its median. 
      #You can compute the median using the median( ) function. Note that you  
      #may find it helpful to use the data.frame( ) function to create a single
      #data set containing both mpg01 and the other Auto variables.
  #(b) Explore the data graphically in order to investigate the association 
      #between mpg01 and the other features. Which of the other features seem  
      #most likely to be useful in predicting mpg01? Scatterplots and Boxplots 
      #may be useful tools to answer this question. Describe your findings.
  #(c) Split the data into a training set and a test set.
  #(d) Perform LDA on the training data in order to predict mpg01 using 
      #the variables that seemed most associated with mpg01 in (b). 
      #What is the test error of the model obtained?
  #(e) Perform QDA on the training data in order to predict mpg01 using the 
      #variables that seemed most associated with mpg01 in (b). 
      #What is the test error of the model obtained?
  #(f) Perform logistic regression on the training data in order to predict 
      #mpg01 using the variables that seemed most associated with mpg01 in (b). 
      #What is the test error of the model obtained?
  #(g) Perform KNN on the training data, with several values of K, in order to 
      #predict mpg01. Use only the variables that seemed most associated with 
      #mpg01 in (b). What test errors do you obtain? Which value of K seems to 
      #perform the best on this data set?

############ Libraries ############
library(ISLR)
library(corrplot)
library(plotly)
library(ggplot2)
library(GGally)
library(reshape2)
library(gridExtra)
library(caTools)
library (MASS)
library (class)
library(pROC)
library(boot)

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

############ Data Visualization ############
#Plot of response (mpg01) and predictors (correlation, density, scatter plots)
ggpairs(data, columns=1:8)

#Histograms
melt.data <- melt(data)

ggplot(data = melt.data, aes(x = value)) + 
  stat_bin() + 
  facet_wrap(~variable, scales = "free")

#Boxplots
p1<-ggplot(data, aes(x=mpg01, y=cylinders, color=mpg01)) + geom_boxplot() 
p2<-ggplot(data, aes(x=mpg01, y=displacement, color=mpg01)) + geom_boxplot()
p3<-ggplot(data, aes(x=mpg01, y=horsepower, color=mpg01)) + geom_boxplot() 
p4<-ggplot(data, aes(x=mpg01, y=weight, color=mpg01)) + geom_boxplot() 
p5<-ggplot(data, aes(x=mpg01, y=year, color=mpg01)) + geom_boxplot() 
p6<-ggplot(data, aes(x=mpg01, y=acceleration, color=mpg01)) + geom_boxplot() 

grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3, nrow = 3)

#Correlation plot
corr_matrix<-cor(data[c(3:6)])
corrplot(corr_matrix, method="circle", type="upper")

#According to the scatter plots, correlation matrix, boxplots and density 
#plot the most useful predictors are horsepower, displacement and weight.
#Specifically, we can observe and clearly distinguish in the boxplots 
#the classification of "0"and "1" on the predictors horsepower, displacement 
#and weight. Hence, we can acknowledge a strong relationship with the response.

#Moreover, we can observe the bimodal distribution for some of the features which
#may help to discriminate our output (mpg01). Finally, it's necessary to analyze 
#potential correlations between features since they may mask and affect our model.

#Other predictors, such as year should be testes in the future since it also 
#displays a bimodal distribution and clear separation in the boxplot. 

############ Data Split ############
sample = sample.split(data$displacement, SplitRatio = .75) #75% of training
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

############ LDA ############
set.seed(12345) #Model training
lda.fits<-lda(mpg01~horsepower+displacement+weight, data=train) ;lda.fits

predicted.lda <- predict(lda.fits, test) #Prediction on test data
names(predicted.lda)

#Estimations, threshold 0.5 (probability<.5 - 0 and 1)
estimate.lda <- predicted.lda$class

#Confusion matrix
table(estimate.lda,test$mpg01)

acc<-mean(estimate.lda==test$mpg01); acc #Predictions Accuracy
test.err<-1-acc; test.err #test error rate

#Test error of 0.088 ~9%

############ QDA ############
#Model training
qda.fits<-qda(mpg01~horsepower+displacement+weight, data=train); lda.fits
predicted.qda <- predict(qda.fits, test) #Prediction on test data
names (predicted.qda)

#Estimations, threshold 0.5 (probability<.5 -> 0 and 1)
estimate.qda <- predicted.qda$class

#Confusion matrix
table(estimate.qda,test$mpg01)

acc<-mean(estimate.qda==test$mpg01); acc #Predictions Accuracy
test.err<-1-acc; test.err #test error rate

#Test error of 0.111 ~11%

############ Logistic Regression ############
glm.fits<-glm(mpg01~horsepower+displacement+weight,
               data=train ,family=binomial)
summary (glm.fits)

predicted.glm <- predict(glm.fits, test, type="response"); 
summary (predicted.glm)

#Estimations, threshold 0.5 (probability<.5 - 0 and 1)
estimate.glm <- round(predicted.glm) 

#Confusion matrix
table(estimate.glm,test$mpg01)

acc<-mean(estimate.glm==test$mpg01);acc #Predictions Accuracy
test.err<-1-acc; test.err #test error rate

#Test error of 0.111 ~11%

############ KNN ############
#Train->horsepower,displacement,weight
train.X=cbind(train$horsepower,train$displacement,train$weight); head(train.X)
#Test->horsepower,displacement,weight
test.X=cbind(test$horsepower,test$displacement,test$weight); head(test.X)
train.y=train$mpg01; head(train.y) #mpg01

#k=1
knn.pred=knn(train.X, test.X, train.y,k=1)
table(knn.pred,test$mpg01)
acc<-mean(knn.pred==test$mpg01);acc
test.err<-1-acc; test.err #test error rate
#Test error of 0.144 ~14%

#k=5
knn.pred=knn(train.X, test.X, train.y,k=7)
table(knn.pred,test$mpg01)
acc<-mean(knn.pred==test$mpg01);acc
test.err<-1-acc; test.err #test error rate
#Test error of 0.111 ~11% #Same as k=7

#k=9
knn.pred=knn(train.X, test.X, train.y,k=9) #Best model
table(knn.pred,test$mpg01)
acc<-mean(knn.pred==test$mpg01);acc
test.err<-1-acc; test.err #test error rate 
#Test error of 0.088 ~9%

#k=11
knn.pred=knn(train.X, test.X, train.y,k=11) 
table(knn.pred,test$mpg01)
acc<-mean(knn.pred==test$mpg01);acc #Same as k=13
test.err<-1-acc; test.err #test error rate 
#Test error of 0.122 ~12%

#k=15
knn.pred=knn(train.X, test.X, train.y,k=15);knn.pred
table(knn.pred,test$mpg01)
acc<-mean(knn.pred==test$mpg01);acc 
test.err<-1-acc; test.err #test error rate 
#Test error of 0.133 ~13%

#The best value is for a k=9, with test error of ~9%

#Problem 2 (6pt)
#Perform ROC analysis and present the results for LDA and KNN for a given K. 
#Use the same model in Problem 1. 

############ LDA ############
pred_post_lda<-predicted.lda$posterior[,2]
ROC_lda <- roc(test$mpg01, pred_post_lda)

# Area Under Curve (AUC) for each ROC curve (higher -> better)
ROC_lda_auc <- auc(ROC_lda); ROC_lda_auc

pROC_obj <- roc(test$mpg01,pred_post_lda,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")

############ KNN ############
#k=9
knn.pred=knn(train.X, test.X, train.y,k=9, prob=TRUE) #Best model
table(knn.pred,test$mpg01)
mean(knn.pred==test$mpg01)

scores.knn <- attr(knn.pred,"prob")
scores.knn[knn.pred=="No"] <- 1-scores.knn[knn.pred=="No"] 

ROC_knn <- roc(test$mpg01, attributes(knn.pred)$prob)

# Area Under Curve (AUC) for each ROC curve (higher -> better)
ROC_knn_auc <- auc(ROC_knn); ROC_knn_auc

pROC_obj <- roc(test$mpg01,scores.knn,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")


#Problem 3 (8pt)
#This question should be answered using the Default data set. In Chapter 4 on 
#classification, we used logistic regression to predict the probability of 
#default using income and balance. Now we will estimate the test error of this 
#logistic regression model using the validation set approach. Do not forget to 
#set a random seed before beginning your analysis.
#(a) Fit a logistic regression model that predicts default using income and balance. 
#(b) Using the validation set approach, estimate the test error of this model. 
#You need to perform the following steps:
  #i. Split the sample set into a training set and a validation set. 
  #ii. Fit a logistic regression model using only the training data set.
  #iii. Obtain a prediction of default status for each individual in the 
  #validation set using a threshold of 0.5.
  #iv. Compute the validation set error, which is the fraction of the 
  #observations in the validation set that are misclassified.
#(c) Repeat the process in (b) three times, using three different splits of the 
    #observations into a training set and a validation set. Comment on the 
    #results obtained.
#(d) Consider another logistic regression model that predicts default using 
     #income, balance and student (qualitative). Estimate the test error for 
     #this model using the validation set approach. Does including the 
     #qualitative variable student lead to a reduction of test error rate?
  
############ Load data ############
head(Default) #Shows the first six lines of Auto data
summary(Default) #Summary statistics for Auto data
names(Default) #Names

#Question "3.A"
############ Logistic Regression ############
#Logistic regression model that predicts default using income and balance
glm.fits<-glm(default~balance+income,
              data=Default ,family=binomial)
summary (glm.fits)

#Question "3.B"
############ Data Split ############
sample = sample.split(Default$income, SplitRatio = .5)
train = subset(Default, sample == TRUE); dim(train)
val = subset(Default, sample == FALSE); dim(val)

############ Logistic Regression with validation ############
glm.fits.val<-glm(default~balance+income, #Training the model
              data=train ,family=binomial)
summary (glm.fits.val)

#Prediction on test data
predicted.glm.val <- predict(glm.fits.val, val, type="response"); 
summary (predicted.glm.val)

#Estimations, threshold 0.5 Validation
estimate.glm.val <- round(predicted.glm.val) 
#Confusion matrix
t<-table(estimate.glm.val,val$default); t
###Validation set error
val.error<-(t[1,2]+t[2,1])/sum(t); val.error

#Question "3.C"
############ Data Split valid=0.25 ############
#Validation = 0.25
valid=0.25
sample = sample.split(Default$income, SplitRatio =1-valid)
train = subset(Default, sample == TRUE); dim(train)
val = subset(Default, sample == FALSE); dim(val)

#LR model
glm.fits.val<-glm(default~balance+income,
                  data=train ,family=binomial)
summary (glm.fits.val)

predicted.glm.val <- predict(glm.fits.val, val, type="response"); 
summary (predicted.glm.val)

#Estimations, threshold 0.75 Validation
estimate.glm.val <- round(predicted.glm.val) 
#Confusion matrix
t<-table(estimate.glm.val,val$default); t
###Validation set error
val.error<-(t[1,2]+t[2,1])/sum(t); val.error

#Validation set error of 0.0268 (~2.68%)

############ Data Split valid=0.75 ############
#Validation = 0.75
valid=0.75
sample = sample.split(Default$income, SplitRatio =1-valid)
train = subset(Default, sample == TRUE); dim(train)
val = subset(Default, sample == FALSE); dim(val)

glm.fits.val<-glm(default~balance+income,
                  data=train ,family=binomial)
summary (glm.fits.val)

predicted.glm.val <- predict(glm.fits.val, val, type="response"); 
summary (predicted.glm.val)

#Estimations, threshold 0.75 Validation
estimate.glm.val <- round(predicted.glm.val) 
#Confusion matrix
t<-table(estimate.glm.val,val$default); t
###Validation set error
val.error<-(t[1,2]+t[2,1])/sum(t); val.error

#Validation set error of 0.0269 (~2.69%)

############ Data Split valid=0.35 ############
#Validation = 0.35
valid=0.35
sample = sample.split(Default$income, SplitRatio =1-valid)
train = subset(Default, sample == TRUE); dim(train)
val = subset(Default, sample == FALSE); dim(val)

glm.fits.val<-glm(default~balance+income,
                  data=train ,family=binomial)
summary (glm.fits.val)

predicted.glm.val <- predict(glm.fits.val, val, type="response"); 
summary (predicted.glm.val)

#Estimations, threshold 0.35 Validation
estimate.glm.val <- round(predicted.glm.val) 
#Confusion matrix
t<-table(estimate.glm.val,val$default); t
###Validation set error
val.error<-(t[1,2]+t[2,1])/sum(t); val.error

#Validation set error of 0.0254 (~2.54%)

#Conclusions:
#Three splits were performed: 0.25, 0.75 and 0.35. We observe that the lowest 
#validation error was for the split of 0.35 (3500/10000); however, there 
#wasn't a significant difference In errors between all of them. 

#Question "3.D"
############ Data Split ############
sample = sample.split(Default$income, SplitRatio = .5)
train = subset(Default, sample == TRUE); dim(train)
val = subset(Default, sample == FALSE); dim(val)

############ Logistic Regression with validation ############
is.factor(Default$student)

glm.fits.val<-glm(default~balance+income+student,
                  data=train ,family=binomial)
summary (glm.fits.val)

predicted.glm.val <- predict(glm.fits.val, val, type="response"); 
summary (predicted.glm.val)

#Estimations, threshold 0.5 Validation
estimate.glm.val <- round(predicted.glm.val) 
#Confusion matrix
t<-table(estimate.glm.val,val$default); t
###Validation set error
val.error<-(t[1,2]+t[2,1])/sum(t); val.error

#Validation set error of 0.025 (~2.5%). 
#It seems that the student predictor has an effect on the model since it reduces
#the test error rate

#Problem 4 (14pt)
#This question requires performing cross validation on a simulated data set.
#(a) Generate a simulated data set as follows:
    #set.seed(1)
    #x=rnorm(200)
    #y=x-2*x^2+rnorm(200)
#In this data set, what is n and what is p? Write out the model used to 
#generate the data in equation form (i.e., the true model of the data).
#(b) Create a scatter plot of Y vs X. Comment on what you find.
#(c) Consider the following four models for the data set:
    #i. Y=??_0+??_1 X+??
    #ii. Y=??_0+??_1 X+??_2 X^2+?? 
    #iii. Y=??_0+??_1 X+??_2 X^2+??_3 X^3+??
    #iv. Y=??_0+??_1 X+??_2 X^2+??_3 X^3+??_4 X^4+??
#Compute the LOOCV errors that result from fitting these models. 
#(d) Repeat (c) using another random seed, and report your results. 
    #Are your results the same as what you got in (c)? Why?
#(e) Which of the models in (c) had the smallest LOOCV error? Is this what you 
    #expected? Explain your answer.
#(f) Now we use 5-fold CV for the model selection. Compute the CV errors that 
    #result from fitting the four models. Which model has the smallest CV error? 
    #Are the results consistent with LOOCV?
#(g) Repeat (f) using 10-fold CV. Are the results the same as 5-fold CV?
  
#Question "4.A"
set.seed(1)
x=rnorm(200); length(x)
y=x-2*x^2+rnorm(200); length(y)

#This is a quadratic model with only two predictors and 200 observations. 
#The predictors seem to be highly correlated since the second predictor is 
#derived by the first one (X, X^2).

error<-rnorm(200) #error

#Coefficients
B0<-0 #B0
B1<-1 #B1
B2<-2 #B2

#Equation form
y_e=B0+B1*x-B2*x^2+error

#Question "4.B"
#Scatter plot
dat=cbind.data.frame(x,y)
ggplot(dat, aes(x=x, y=y)) + geom_point()

#According to the scatter plot, the model and data seems to display a non-linear
#behavior and potential cubic form. Furthermore, it seems that the plot doesn't
#display outliers or high leverage points. 

#Question "4.C"
#Models
cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(y~poly(x,i),data=dat)
  cv.error[i] = cv.glm(dat,glm.fit)$delta[1]
}
cv.error

cv.error[4] #Polynomial of degree=4

#Question "4.D"
set.seed(123)
x=rnorm(200); length(x)
y=x-2*x^2+rnorm(200); length(y)

#Models
cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(y~poly(x,i),data=dat)
  cv.error[i] = cv.glm(dat,glm.fit)$delta[1]
}
cv.error

cv.error[4] #Polynomial of degree=4

#Using a different seed, we can recognize the same CV error. The main reason for 
#this is because the LOOCV splits the dataset n times; thus, all the models  
#are going to be very similar because we are doing CV on every sample (n).

#Scatter plot
dat=cbind.data.frame(x,y)
ggplot(dat, aes(x=x, y=y)) + geom_point()

#Question "4.E"
#Y=??_0+??_1 X+??_2 X^2+??_3 X^3+?? 

#The smallest LOOCV error was with the fourth degree polynomial equation (~1.02); 
#however, all of the models displayed a similar CV error. 

#Finally, this result was expected since we can distinguish a non-linear 
#behavior in the x-y scatter plot.

#Question "4.F" 5-fold
cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(y~poly(x,i),data=dat)
  cv.error[i] = cv.glm(dat,glm.fit,K=5)$delta[1]
}
cv.error

cv.error[3] #Cubic form

#The smallest CV error was for the cubic model with a value of ~1.005. Also, 
#there is a reduction in the CV error in comparison to LOOCV; nevertheless, 
#both models and CV errors display a very close error. 

#Finally, the K-fold and LOOCV display different models; however, both of them
#present a close CV error; hence, we can use the less complex one. 

#Question "4.E" 10-fold
cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(y~poly(x,i),data=dat)
  cv.error[i] = cv.glm(dat,glm.fit,K=10)$delta[1]
}
cv.error

#The results are not exactly the same for 5-fold but they exhibit a very similar
#behavior

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console
