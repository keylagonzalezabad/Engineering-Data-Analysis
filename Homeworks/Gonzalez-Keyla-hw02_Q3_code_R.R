####### Homework #2 ####### 
#Name: Keyla Gonzalez Abad

# Use the Auto data set to answer the following questions:
# 
#   (a) Perform a simple linear regression with mpg as the response and horsepower as the predictor. Comment on the output. For example
#       i.      Is there a relationship between the predictor and the response?
#       ii.     How strong is the relationship between the predictor and the response?
#       iii.    Is the relationship between the predictor and the response positive or negative?
#       iv.    How to interpret the estimate of the slope?
#       v.     What is the predicted mpg associated with a horsepower of 98? What are the associated 95% confidence and prediction intervals?
#   (b) Plot the response and the predictor. Display the least squares regression line in the plot.
#   (c) Produce the diagnostic plots of the least squares regression fit. Comment on each plot.
#   (d) Try a few different transformations of the predictor, such as log???(X),???X,X^2, and repeat (a)-(c). Comment on your findings.

############ Linear Regression ############
library(ISLR)
head(Auto) #Shows the first six lines of Auto data
summary(Auto) #Summary statistics for Auto data
names(Auto) #Names

Quant=Auto[c("mpg","horsepower")]
head(Quant)
cor(Quant) #Correlation between response and predictor
           #We can clearly observe a strong negative correlation between 
           #response and predictor (approx -0.778)

#Plot of response and predictor  
pairs(Quant, pch = 19, cex=0.9,
      col = "darkolivegreen4",
      lower.panel=NULL,
      main = "Scatter plots of Data")

#By plotting my data, I can observe a negative trend (correlation) 
#between response and predictor, and it seems that the trend is non-linear.

#Fitting Linear Model
?lm #Information of linear models
x<-Auto[,4]
y<-Auto[,1]

#Both of them are the same
mod <- lm(y~x) #Model
mod <- lm(Quant) #Model
summary(mod) #Summary of model (Std. Error, t value, Pr(>|t|), sqrt(MSE))

# *** ->Indicates values that are below the standard probability cutoff of 0.05.
#And this indicates that we can reject the null hypothesis which allows us to 
#conclude that there is a relationship between response and predictor.

#For the t-value, we would like to obtain a large value in order to reject the 
#null hypothesis. Since we obtained a large value we can conclude that the 
#response and predictor display a strong correlation

attributes(mod) #Attributes of my linear model
mod$coef #Coefficients (slope and intercept). The slope is negative; hence, a
         #large mgp <- low hoursepower and a large hoursepower <- low mpg
confint(mod, level=0.95) #Confidence Interval of my model

#Predicted mpg associated with a horsepower of 98
dat_98<-data.frame(horsepower = 98);dat_98
pred_int<-predict(mod, dat_98, interval="prediction");pred_int
conf_int<-predict(mod, dat_98, interval="confidence");conf_int

############ Plotting ############
#install.packages("ggExtra")
library(ggplot2)
library(ggExtra)

#Using ggplot2 + geom_smooth for linear model 
mpg_hwp=ggplot(Auto, aes(x = horsepower, y = mpg))

mpg_hwp + 
  geom_point(color="blue") +
  geom_rug() +
  geom_smooth(method = lm, color = "red")+ ggtitle("Mpg vs Horsepower") +
  theme(plot.title = element_text(hjust = 0.5))

#Using plot + abline 
plot(Quant$horsepower, Quant$mpg,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "mpg vs horsepower",
     xlab = "horsepower",
     ylab = "mpg")

abline(mod, col="red", lwd=3) #Model (regression line)

############ Diagnostic Plots ############  

par(mfrow=c(2,2))
plot(mod, pch = 19, cex=0.9,lwd=2)

#Four plots are displayed:

#1. Residuals (errors) vs fitted: There is linearity if the red line is flat.
#We can observe that there is no linearity (linearity assumption is not met). 
#Moreover, we can observe a variation in the model 

#2.Normal Q-Q (Quantile-Quantile): This plot can help us to observe if the errors
#are normally distributed
#It seems the errors are not normally distributed, specifically at the extremes of it.

#3. Scale-location: Shows whether residuals are spread equally along the ranges 
#of input variables (predictor)
#4.Residuals vs Leverage:Identifies influential data points on your model, 
#such as Outliers.

#Both plots 3 and 4 can help us to understand the non-linear behavior 

############ Transformations ############
#x
plot(x, y,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "x",
     xlab = "mpg",
     ylab = "horsepower")
abline(mod, col="red", lwd=3) #Model (regression line)

#log???(X)
x_log<-log(x)
mod_log <- lm(y~x_log) #Model

plot(x_log, y,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "log(x)",
     xlab = "mpg",
     ylab = "horsepower")
abline(mod_log, col="red", lwd=3) #Model (regression line)

#???X
x_sqrt<-sqrt(x)
mod_sqrt <- lm(y~x_sqrt) #Model

plot(x_sqrt, y,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "sqrt(x)",
     xlab = "mpg",
     ylab = "horsepower")
abline(mod_sqrt, col="red", lwd=3) #Model (regression line)

#X^2
x_sq<-x**2
mod_sq <- lm(y~x_sq) #Model

plot(x_sq, y,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "(x)^2",
     xlab = "mpg",
     ylab = "horsepower")
abline(mod_sq, col="red", lwd=3) #Model (regression line)

par(mfrow=c(1,1))

#Summary of all models
summary(mod)
summary(mod_log)
summary(mod_sqrt)
summary(mod_sq)

#Diagnostic plots
par(mfrow=c(2,2))

#log(x)
plot(mod_log, pch = 19, cex=0.9,lwd=2)
#???X
plot(mod_sqrt, pch = 19, cex=0.9,lwd=2)
#X^2
plot(mod_sq, pch = 19, cex=0.9,lwd=2)

par(mfrow=c(1,1))

#According to the "Residual standard error", the best model for a linear 
#regression is with log(x) and the worst one with x^2.

#Best to worst models:
#1.log(x) (RSE: 4.501)
#2.???X (RSE: 4.665)
#3.x (RSE: 4.906)
#4.x^2 (RSE: 5.485)

#Moreover, the response vs predictor plots can help us to observe the behavior 
#of each model, where we can observe that the best models are log(x) and ???X.

#Finally, the diagnostic plots can help us to confirm that log(x) and ???X display
#linearity (linearity assumption is met)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console

