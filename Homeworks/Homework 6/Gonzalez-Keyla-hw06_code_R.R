####### Homework #6 ####### 
#Name: Keyla Gonzalez Abad

#Problem 1 (10pt)
#In this question, we will predict the number of applications received (Apps) 
#using the other variables in the College data set (ISLR package).
 #(a) Perform best subset selection to the data. What is the best model obtained 
 #according to Cp, BIC and adjusted R2? Show some plots to provide evidence for 
 #your answer, and report the coefficients of the best model. 
 #(b) Repeat (a) using forward stepwise selection and backwards stepwise 
 #selection. How does your answer compare to the results in (a)?
 #(c) Fit a lasso model on the data. Use cross-validation to select the optimal 
 #value of lambda. Create plots of the cross-validation error as a function of 
 #lambda.Report the resulting coefficient estimates.
 #(d) Fit a ridge regression model on the data. Use cross-validation to select 
 #the optimal value of lambda. Create plots of the cross-validation error as a 
 #function of lambda. Report the resulting coefficient estimates.
 #(e) Now split the data set into a training set and a test set.
    #i. Fit the best models obtained in the best subset selection (according to 
       #Cp, BIC or adjusted R2) to the training set, and report the test error 
       #obtained.
    #ii. Fit a lasso model to the training set, with lambda chosen by cross 
       #validation. Report the test error obtained.
    #iii. Fit a ridge regression model to the training set, with lambda chosen 
       #by cross validation. Report the test error obtained.
    #iv. Compare the test errors obtained in the above analysis (i-iii) and 
       #determine the optimal model.

############ Libraries ############
#install.packages("glmnet")
library(leaps)
library(ISLR)
library(glmnet)
library(caTools)

############ Load data ############
summary(College) #Summary statistics for College data
names(College) #Names
is.data.frame(College) #Dataframe
dim(College) #18 Predictors and 777 Samples

sum(is.na(College$Apps)) #Number of missing values for response
is.factor(College$Private) #Categorical Predictor

############ Best Subset Selection ############
regfit.full=regsubsets(Apps~., College, nvmax =18)
reg.summary=summary(regfit.full); reg.summary
names(reg.summary)

###### Choosing optimal model (Cp, BIC. Adjr2)
par(mfrow=c(2,2))

#RSS
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
min.rss.full=which.min(reg.summary$rss); min.rss.full
points(min.rss.full, reg.summary$rss[min.rss.full], col="red",cex =2, pch =20)
#Adjusted R2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
max.adjr2.full=which.max (reg.summary$adjr2); max.adjr2.full
points (max.adjr2.full, reg.summary$adjr2[max.adjr2.full], col ="red",cex =2, pch =20)
#Cp
plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",type='l')
min.cp.full=which.min(reg.summary$cp); min.cp.full
points (min.cp.full, reg.summary$cp [min.cp.full], col="red",cex =2, pch =20)
#BIC
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC", type='l')
min.bic.full=which.min(reg.summary$bic); min.bic.full
points(min.bic.full, reg.summary$bic[min.bic.full], col="red",cex =2, pch =20)
par(mfrow=c(1,1))

#According to the plots of indirect approaches (Cp, BIC, Adjusted R2), the best 
#models (best predictions) include 10 to 13 predictors.

#For Cp, the best prediction is with 12 predictors, for BIC is 10 and for 
#adjusted R2 is 13.

par(mfrow=c(1,3))
#Plot of selected variables for the best model
plot(regfit.full, scale = "adjr2")
#adjr2->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad, 
        #Outstate,RoomBoard,PhD,S.F.Ratio,Expend,Grad.Rate)
plot(regfit.full, scale ="Cp") 
#Cp->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad, 
     #Outstate,RoomBoard,PhD,Expend,Grad.Rate)
plot(regfit.full, scale ="bic")
#BIC->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,Outstate,RoomBoard,PhD
      #Expend,Grad.Rate)

#Coefficients adjr2 (13 predictors)
coef(regfit.full,max.adjr2.full)
#Coefficients Cp (12 predictors)
coef(regfit.full,min.cp.full)
#Coefficients BIC (10 predictors)
coef(regfit.full,min.bic.full)
dev.off()

############ Forward stepwise selection ############
regfit.fwd=regsubsets(Apps~.,data=College,nvmax =18,method="forward")
reg.summary.fwd=summary(regfit.fwd); reg.summary.fwd

###### Choosing optimal model (Cp, BIC. Adjr2)
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

#According to the plots of indirect approaches (Cp, BIC, Adjusted R2), the best 
#models (best predictions) include 10 to 13 predictors.

#For Cp, the best prediction is with 12 predictors, for BIC is 10 and for 
#adjusted R2 is 13.

#Plot of selected variables for the best model
plot(regfit.fwd, scale = "adjr2")
#adjr2->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad, 
#Outstate,RoomBoard,PhD,S.F.Ratio,Expend,Grad.Rate)
plot(regfit.fwd, scale ="Cp") 
#Cp->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad, 
#Outstate,RoomBoard,PhD,Expend,Grad.Rate)
plot(regfit.fwd, scale ="bic")
#BIC->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,Outstate,RoomBoard,PhD
#Expend,Grad.Rate)

#Coefficients adjr2 (13 predictors)
coef(regfit.fwd,max.adjr2.fwd)
#Coefficients Cp (12 predictors)
coef(regfit.fwd,min.cp.fwd)
#Coefficients BIC (10 predictors)
coef(regfit.fwd,min.bic.fwd)
dev.off()

############ Backwards stepwise selection ############
regfit.bwd=regsubsets(Apps~.,data=College,nvmax =18, method="backward")
reg.summary.bwd=summary(regfit.bwd); reg.summary.bwd

###### Choosing optimal model (Cp, BIC. Adjr2)
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
dev.off()

#According to the plots of indirect approaches (Cp, BIC, Adjusted R2), the best 
#models (best predictions) include 10 to 13 predictors.

#For Cp, the best prediction is with 12 predictors, for BIC is 10 and for 
#adjusted R2 is 13.

#Plot of selected variables for the best model
plot(regfit.bwd, scale = "adjr2")
#adjr2->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad, 
#Outstate,RoomBoard,PhD,S.F.Ratio,Expend,Grad.Rate)
plot(regfit.bwd, scale ="Cp") 
#Cp->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,F.Undergrad,P.Undergrad, 
#Outstate,RoomBoard,PhD,Expend,Grad.Rate)
plot(regfit.bwd, scale ="bic")
#BIC->(PrivateYes,Accept,Enroll,Top10perc,Top25perc,Outstate,RoomBoard,PhD
#Expend,Grad.Rate)

#Coefficients adjr2 (13 predictors)
coef(regfit.bwd,max.adjr2.bwd)
#Coefficients Cp (12 predictors)
coef(regfit.bwd,min.cp.bwd)
#Coefficients BIC (10 predictors)
coef(regfit.bwd,min.bic.bwd)
dev.off()

############ Lasso model (alpha=1) ############
x=model.matrix(Apps~.,College)[,-1]; #Predictors Matrix 
is.matrix(x); dim(x)

y=College$Apps #Response Vector 
is.vector(y); length(y)

set.seed(5)
cv.out.5=cv.glmnet(x, y, alpha=1, nfolds=5)
plot(cv.out.5)
lasso.bestlam.5=cv.out.5$lambda.min; lasso.bestlam.5

#cv.out.10=cv.glmnet(x, y, alpha=1, nfolds=10)
#plot(cv.out.10)
#bestlam.10=cv.out.10$lambda.min; bestlam.10

lasso.mod=glmnet(x,y,alpha =1,lambda=lasso.bestlam.5); lasso.mod
lasso.pred=predict(lasso.mod,s=lasso.bestlam.5,type="coefficients"); lasso.pred

############ Ridge model (alpha=0) ############
cv.out.5=cv.glmnet(x, y, alpha=0, nfolds=5)
plot(cv.out.5)
ridge.bestlam.5=cv.out.5$lambda.min; ridge.bestlam.5

#cv.out.10=cv.glmnet(x, y, alpha=1, nfolds=10)
#plot(cv.out.10)
#bestlam.10=cv.out.10$lambda.min; bestlam.10

ridge.mod=glmnet(x,y,alpha=0,lambda=ridge.bestlam.5); ridge.mod
ridge.pred=predict(ridge.mod,s=ridge.bestlam.5,type="coefficients"); ridge.pred

############ Data Split ############
sample = sample.split(College$PhD, SplitRatio = .75) #75% of training
train = subset(College, sample == TRUE)
test  = subset(College, sample == FALSE)

#Train
x.train=model.matrix(Apps~.,train)[,-1]; #Predictors Matrix 
is.matrix(x.train); dim(x.train)
y.train=train$Apps #Response Vector 
is.vector(y.train); length(y.train)

#Test
x.test=model.matrix(Apps~.,test)[,-1]; #Predictors Matrix 
is.matrix(x.test); dim(x.test)
y.test=test$Apps #Response Vector 
is.vector(y.test); length(y.test)

##### Best subset selection
regfit.full.train=regsubsets(Apps~., train, nvmax =18)
reg.summary.train=summary(regfit.full.train); reg.summary.train
names(reg.summary.train)

par(mfrow=c(2,2))
#RSS
plot(reg.summary.train$rss,xlab="Number of Variables",ylab="RSS",type="l")
min.rss.full=which.min(reg.summary.train$rss); min.rss.full
points(min.rss.full, reg.summary.train$rss[min.rss.full], col="red",cex =2, pch =20)
#Adjusted R2
plot(reg.summary.train$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
max.adjr2.full=which.max (reg.summary.train$adjr2); max.adjr2.full
points (max.adjr2.full, reg.summary.train$adjr2[max.adjr2.full], col ="red",cex =2, pch =20)
#Cp
plot(reg.summary.train$cp ,xlab =" Number of Variables ",ylab="Cp",type='l')
min.cp.full=which.min(reg.summary.train$cp); min.cp.full
points (min.cp.full, reg.summary.train$cp [min.cp.full], col="red",cex =2, pch =20)
#BIC
plot(reg.summary.train$bic ,xlab=" Number of Variables ",ylab=" BIC", type='l')
min.bic.full=which.min(reg.summary.train$bic); min.bic.full
points(min.bic.full, reg.summary.train$bic[min.bic.full], col="red",cex =2, pch =20)
par(mfrow=c(1,1))
dev.off()

#According to the plots of indirect approaches (Cp, BIC, Adjusted R2), the best 
#models (best predictions) include 9 to 13 predictors. For Cp, the best 
#prediction is with 11 predictors, for BIC is 9 and for adjusted R2 is 13.

#Coefficients adjr2 (13 predictors)
coef(regfit.full.train,max.adjr2.full)
#Coefficients Cp (11 predictors)
coef(regfit.full.train,min.cp.full)
#Coefficients BIC (9 predictors)
coef(regfit.full.train,min.bic.full)

#Prediction function
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
} # prediction function

k=5 #number of folds in the cross validation
set.seed(123)
folds=sample(1:k,nrow(train),replace=TRUE) #Split data
cv.errors=matrix(NA,k,17,dimnames=list(NULL, paste(1:17))) #Empty matrix 
for(j in 1:k){
  best.fit=regsubsets(Apps~.,data=train[folds!=j,],nvmax=17) 
  for(i in 1:17){ 
    pred=predict(best.fit,train[folds==j,],id=i) #Prediction on training data
    cv.errors[j,i]=mean((train$Apps[folds==j]-pred)^2)
  }
}

#Prediction on training data
mean.cv.error=apply(cv.errors,2,mean) #average errors over folds
plot(mean.cv.error, type='b')
min.cv.error=which.min(mean.cv.error); min.cv.error
points(min.cv.error, mean.cv.error[min.cv.error], col="red",cex =2, pch =20)
mean.cv.error[min.cv.error] 

##### Lasso model (alpha=1)
cv.out.lass=cv.glmnet(x.train, y.train, alpha=1, nfolds=5)
plot(cv.out.lass)
lass.bestlam=cv.out.lass$lambda.min; lass.bestlam

lass.mod=glmnet(x.train, y.train,alpha=1,lambda=lass.bestlam); lass.mod
lass.pred=predict(lass.mod,s=lass.bestlam,newx=x.test); lass.pred

mse.lass=mean((lass.pred-y.test)**2); mse.lass

##### Ridge model (alpha=0) 
cv.out.rid=cv.glmnet(x.train, y.train, alpha=0, nfolds=5)
plot(cv.out.rid)
rid.bestlam=cv.out.rid$lambda.min; rid.bestlam

rid.mod=glmnet(x.train,y.train,alpha=0,lambda=rid.bestlam); rid.mod
rid.pred=predict(rid.mod,s=rid.bestlam,newx=x.test); ridge.pred

mse.rid=mean((rid.pred-y.test)**2); mse.rid


#### #Problem 2 (5pt)
x=model.matrix(Apps~.,College)[,-1]; #Predictors Matrix 
is.matrix(x); dim(x)
y=College$Apps #Response Vector 
is.vector(y); length(y)

## Behavior of the coefficients in terms of ??
#??=(X^T*y)/(X*X^T+??*I)

par(mfrow=c(1,2))
grid = 10^seq(10, -2, length = 100)
res <- glmnet(x, y, alpha = 0, lambda = grid, standardize = TRUE)
plot(res, xvar = "lambda")
res.cv <- cv.glmnet(x, y, alpha = 0, lambda = grid, standardize = TRUE)
plot(res.cv)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console

