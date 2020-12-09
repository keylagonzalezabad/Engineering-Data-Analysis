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
mydata <- read.table(path, header=TRUE, sep=","); cor.data<-mydata

summary(mydata) #Summary statistics for mydata data
names(mydata) #Names
is.data.frame(mydata) #Dataframe
dim(mydata) #66 Predictors and 152 Samples

############ Data Visualization ############
#Correlation plot
corr.data<-mydata[,c(1:13)]; corr.data
names(corr.data)

corr.data<-corr.data %>% rename(
  Retail.Rec.B11 = retail_and_recreation_percent_change_from_baseline_11,
  Grocery.Pharm.B11 = grocery_and_pharmacy_percent_change_from_baseline_11,
  Parks.B11 = parks_percent_change_from_baseline_11,
  Transit.B11 = transit_stations_percent_change_from_baseline_11,
  Workplaces.B11 = workplaces_percent_change_from_baseline_11,
  Residencial.B11 = residential_percent_change_from_baseline_11,
  Retail.Rec.B10 = retail_and_recreation_percent_change_from_baseline_10,
  Grocery.Pharm.B10 = grocery_and_pharmacy_percent_change_from_baseline_10,
  Parks.B10 = parks_percent_change_from_baseline_10,
  Transit.B10 = transit_stations_percent_change_from_baseline_10,
  Workplaces.B10 = workplaces_percent_change_from_baseline_10,
  Residencial.B10 = residential_percent_change_from_baseline_10
)

corr_matrix<-cor(corr.data); corr_matrix
corrplot(corr_matrix, type="upper", tl.cex=0.6, tl.col='black')

#Plot of response Rt and predictors (correlation, density, scatter plots)
ggpairs(corr.data, columns=1:13)

#With a Transformation (Log(Mean.R.))
log.mean.R.<-log(corr.data$Mean.R.); log.mean.R.
corr.W.mean<-subset(corr.data, select = -c(Mean.R.)); corr.W.mean
corr.data.2<-cbind(log.mean.R. = log.mean.R., corr.W.mean); corr.data.2

corr_matrix.2<-cor(corr.data.2); corr_matrix.2
corrplot(corr_matrix.2, type="upper", tl.cex=0.6, tl.col='black')

#Plot of response Rt and predictors (correlation, density, scatter plots)
ggpairs(corr.data.2, columns=1:13)

mydata<-mydata %>% rename(
  Retail.Rec.B11 = retail_and_recreation_percent_change_from_baseline_11,
  Grocery.Pharm.B11 = grocery_and_pharmacy_percent_change_from_baseline_11,
  Parks.B11 = parks_percent_change_from_baseline_11,
  Transit.B11 = transit_stations_percent_change_from_baseline_11,
  Workplaces.B11 = workplaces_percent_change_from_baseline_11,
  Residencial.B11 = residential_percent_change_from_baseline_11,
  
  Retail.Rec.B10 = retail_and_recreation_percent_change_from_baseline_10,
  Grocery.Pharm.B10 = grocery_and_pharmacy_percent_change_from_baseline_10,
  Parks.B10 = parks_percent_change_from_baseline_10,
  Transit.B10 = transit_stations_percent_change_from_baseline_10,
  Workplaces.B10 = workplaces_percent_change_from_baseline_10,
  Residencial.B10 = residential_percent_change_from_baseline_10,
  
  Retail.Rec.B9 = retail_and_recreation_percent_change_from_baseline_9,
  Grocery.Pharm.B9 = grocery_and_pharmacy_percent_change_from_baseline_9,
  Parks.B9 = parks_percent_change_from_baseline_9,
  Transit.B9 = transit_stations_percent_change_from_baseline_9,
  Workplaces.B9 = workplaces_percent_change_from_baseline_9,
  Residencial.B9 = residential_percent_change_from_baseline_9,
  
  Retail.Rec.B8 = retail_and_recreation_percent_change_from_baseline_8,
  Grocery.Pharm.B8 = grocery_and_pharmacy_percent_change_from_baseline_8,
  Parks.B8 = parks_percent_change_from_baseline_8,
  Transit.B8 = transit_stations_percent_change_from_baseline_8,
  Workplaces.B8 = workplaces_percent_change_from_baseline_8,
  Residencial.B8 = residential_percent_change_from_baseline_8,
  
  Retail.Rec.B7 = retail_and_recreation_percent_change_from_baseline_7,
  Grocery.Pharm.B7 = grocery_and_pharmacy_percent_change_from_baseline_7,
  Parks.B7 = parks_percent_change_from_baseline_7,
  Transit.B7 = transit_stations_percent_change_from_baseline_7,
  Workplaces.B7 = workplaces_percent_change_from_baseline_7,
  Residencial.B7 = residential_percent_change_from_baseline_7,
  
  Retail.Rec.B6 = retail_and_recreation_percent_change_from_baseline_6,
  Grocery.Pharm.B6 = grocery_and_pharmacy_percent_change_from_baseline_6,
  Parks.B6 = parks_percent_change_from_baseline_6,
  Transit.B6 = transit_stations_percent_change_from_baseline_6,
  Workplaces.B6 = workplaces_percent_change_from_baseline_6,
  Residencial.B6 = residential_percent_change_from_baseline_6,
  
  Retail.Rec.B5 = retail_and_recreation_percent_change_from_baseline_5,
  Grocery.Pharm.B5 = grocery_and_pharmacy_percent_change_from_baseline_5,
  Parks.B5 = parks_percent_change_from_baseline_5,
  Transit.B5 = transit_stations_percent_change_from_baseline_5,
  Workplaces.B5 = workplaces_percent_change_from_baseline_5,
  Residencial.B5 = residential_percent_change_from_baseline_5,
  
  Retail.Rec.B4 = retail_and_recreation_percent_change_from_baseline_4,
  Grocery.Pharm.B4 = grocery_and_pharmacy_percent_change_from_baseline_4,
  Parks.B4 = parks_percent_change_from_baseline_4,
  Transit.B4 = transit_stations_percent_change_from_baseline_4,
  Workplaces.B4 = workplaces_percent_change_from_baseline_4,
  Residencial.B4 = residential_percent_change_from_baseline_4,
  
  Retail.Rec.B3 = retail_and_recreation_percent_change_from_baseline_3,
  Grocery.Pharm.B3 = grocery_and_pharmacy_percent_change_from_baseline_3,
  Parks.B3 = parks_percent_change_from_baseline_3,
  Transit.B3 = transit_stations_percent_change_from_baseline_3,
  Workplaces.B3 = workplaces_percent_change_from_baseline_3,
  Residencial.B3 = residential_percent_change_from_baseline_3,
  
  Retail.Rec.B2 = retail_and_recreation_percent_change_from_baseline_2,
  Grocery.Pharm.B2 = grocery_and_pharmacy_percent_change_from_baseline_2,
  Parks.B2 = parks_percent_change_from_baseline_2,
  Transit.B2 = transit_stations_percent_change_from_baseline_2,
  Workplaces.B2 = workplaces_percent_change_from_baseline_2,
  Residencial.B2 = residential_percent_change_from_baseline_2,
  
  Retail.Rec.B1 = retail_and_recreation_percent_change_from_baseline_1,
  Grocery.Pharm.B1 = grocery_and_pharmacy_percent_change_from_baseline_1,
  Parks.B1 = parks_percent_change_from_baseline_1,
  Transit.B1 = transit_stations_percent_change_from_baseline_1,
  Workplaces.B1 = workplaces_percent_change_from_baseline_1,
  Residencial.B1 = residential_percent_change_from_baseline_1
)

############ Data Split ############
set.seed(1)
sample = sample.split(mydata$Mean.R., SplitRatio = .75) #75% of training
train = subset(mydata, sample == TRUE)
test  = subset(mydata, sample == FALSE)

############ Multilinear Regression Model ############
set.seed(2)
data.lm <- lm(Mean.R. ~., data=train)
summary(data.lm)

lm.pred <- predict(data.lm, newdata=test); lm.pred
MSE.lm<-mean((lm.pred-test$Mean.R.)^2); MSE.lm
plot(lm.pred, test$Mean.R.)
abline (0,1)

#Variance inflation factor (VIF)
vif(data.lm)
#-VIF=1, no collinearity.
#-VIF>5, serious collinearity.
vif.data<-vif(data.lm); vif.data

## Forward stepwise selection
set.seed(3)
regfit.fwd=regsubsets(Mean.R.~.,data=train, nvmax=66, method="forward")
reg.summary.fwd=summary(regfit.fwd); reg.summary.fwd

## Choosing optimal model (Cp, BIC. Adjr2)
par(mfrow=c(2,2))

#RSS
plot(reg.summary.fwd$rss,xlab="Number of Variables",ylab="RSS",type="l")
min.rss.fwd=which.min(reg.summary.fwd$rss); min.rss.fwd
points(min.rss.fwd, reg.summary.fwd$rss[min.rss.fwd], col="red",cex =2, pch =20)
#Adjusted R2
plot(reg.summary.fwd$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
max.adjr2.fwd=which.max (reg.summary.fwd$adjr2); max.adjr2.fwd
points (max.adjr2.fwd, reg.summary.fwd$adjr2[max.adjr2.fwd], col ="red",cex =2, pch =20)
#Cp
plot(reg.summary.fwd$cp ,xlab =" Number of Variables ",ylab="Cp",type='l')
min.cp.fwd=which.min(reg.summary.fwd$cp); min.cp.fwd
points (min.cp.fwd, reg.summary.fwd$cp [min.cp.fwd], col="red",cex =2, pch =20)
#BIC
plot(reg.summary.fwd$bic ,xlab=" Number of Variables ",ylab=" BIC", type='l')
min.bic.fwd=which.min(reg.summary.fwd$bic); min.bic.fwd
points(min.bic.fwd, reg.summary.fwd$bic[min.bic.fwd], col="red",cex =2, pch =20)
par(mfrow=c(1,1))

#Coefficients adjr2 (32 predictors)
coef(regfit.fwd,max.adjr2.fwd)
#Coefficients Cp (9 predictors)
coef(regfit.fwd,min.cp.fwd)
#Coefficients BIC (7 predictors)
coef(regfit.fwd,min.bic.fwd)

data.lm.fwd <- lm(Mean.R. ~Workplaces.B11+Workplaces.B8+
                    Grocery.Pharm.B7 + Transit.B7+Parks.B3+
                    Workplaces.B3+Parks.B1, data=train)
summary(data.lm.fwd)

lm.pred.fwd <- predict(data.lm.fwd, newdata=test); lm.pred.fwd
MSE.lm.fwd<-mean((lm.pred.fwd-test$Mean.R.)^2); MSE.lm.fwd
plot(lm.pred.fwd, test$Mean.R.)
abline (0,1)

######### Backwards stepwise selection ######### 
set.seed(4)
regfit.bwd=regsubsets(Mean.R.~.,data=train, nvmax=66, method="backward")
reg.summary.bwd=summary(regfit.bwd); reg.summary.bwd

## Choosing optimal model (Cp, BIC. Adjr2)
par(mfrow=c(2,2))

#RSS
plot(reg.summary.bwd$rss,xlab="Number of Variables",ylab="RSS",type="l")
min.rss.bwd=which.min(reg.summary.bwd$rss); min.rss.bwd
points(min.rss.bwd, reg.summary.bwd$rss[min.rss.bwd], col="red",cex =2, pch =20)
#Adjusted R2
plot(reg.summary.bwd$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
max.adjr2.bwd=which.max (reg.summary.bwd$adjr2); max.adjr2.bwd
points (max.adjr2.bwd, reg.summary.bwd$adjr2[max.adjr2.bwd], col ="red",cex =2, pch =20)
#Cp
plot(reg.summary.bwd$cp ,xlab =" Number of Variables ",ylab="Cp",type='l')
min.cp.bwd=which.min(reg.summary.bwd$cp); min.cp.bwd
points (min.cp.bwd, reg.summary.bwd$cp [min.cp.bwd], col="red",cex =2, pch =20)
#BIC
plot(reg.summary.bwd$bic ,xlab=" Number of Variables ",ylab=" BIC", type='l')
min.bic.bwd=which.min(reg.summary.bwd$bic); min.bic.bwd
points(min.bic.bwd, reg.summary.bwd$bic[min.bic.bwd], col="red",cex =2, pch =20)
par(mfrow=c(1,1))

#Coefficients adjr2 (22 predictors)
coef(regfit.bwd,max.adjr2.bwd)
#Coefficients Cp (12 predictors)
coef(regfit.bwd,min.cp.bwd)
#Coefficients BIC (10 predictors)
coef(regfit.bwd,min.bic.bwd)

data.lm.bwd <- lm(Mean.R. ~Transit.B11+Workplaces.B11+Transit.B6+Retail.Rec.B5+
                    Grocery.Pharm.B5 + Residencial.B4+Retail.Rec.B3 +Parks.B3+
                    Transit.B3 +Retail.Rec.B1, data=train)
summary(data.lm.bwd)

lm.pred.bwd <- predict(data.lm.bwd, newdata=test); lm.pred.bwd
MSE.lm.bwd<-mean((lm.pred.bwd-test$Mean.R.)^2); MSE.lm.bwd
plot(lm.pred.bwd, test$Mean.R.)
abline (0,1)

############ Final Model - SVM ############
library("readxl")
set.seed(3)
path.new <- #*** Copy your unseen test path***
new.test <- read_excel(path.new)
summary(new.test) #Summary statistics for new.test data
names(new.test) #Names

#Trained model
lm.all=lm(Mean.R. ~ workplaces_percent_change_from_baseline_11 +
            workplaces_percent_change_from_baseline_8 +
            grocery_and_pharmacy_percent_change_from_baseline_7 + 
            transit_stations_percent_change_from_baseline_7 + 
            parks_percent_change_from_baseline_3 +
            workplaces_percent_change_from_baseline_3 +
            parks_percent_change_from_baseline_1, data=cor.data)

#Train Predictions
lm.pred.all=predict(lm.all, newdata=cor.data)
MSE.lm.all<-mean((lm.pred.all-cor.data$Mean.R.)^2); MSE.lm.all
plot(lm.pred.all, cor.data$Mean.R., xlab='Train Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

#Test Predictions for Unseen data
lm.pred.new=predict(lm.all, newdata=new.test)
MSE.lm.new<-mean((lm.pred.new-new.test$`Mean(R)`)^2); MSE.lm.new

plot(lm.pred.new, new.test$`Mean(R)`, xlab='Test Rt', 
     ylab='Predicted Rt', col = "dark blue")
abline (0,1)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console

