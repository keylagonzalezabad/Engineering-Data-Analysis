####### Homework #3 ####### 
#Name: Keyla Gonzalez Abad

#Problem 1 (5pt)
# Use the Auto data set to answer the following questions:
#   (a) Produce a scatterplot matrix which includes all of the variables in the data set. Which predictors appear to have an association with the response?
#   (b) Compute the matrix of correlations between the variables (using the function cor()). You will need to exclude the name variable, which is qualitative. 
#   (c) Perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. Comment on the output. For example,
#       i.      Is there a relationship between the predictors and the response?
#       ii.     Which predictors have a statistically significant relationship to the response?
#       iii.    What does the coefficient for the year variable suggest?
#   (d) Produce diagnostic plots of the linear regression fit. Comment on each plot.
#   (e) Is there serious collinearity problem in the model? Which predictors are collinear?
#   (f) Fit linear regression models with interactions. Are any interactions statistically significant?
  
############ Load data ############
#install.packages("car")
library(ISLR)
library (car)
head(Auto) #Shows the first six lines of Auto data
summary(Auto) #Summary statistics for Auto data
names(Auto) #Names
is.data.frame(Auto) 


############ Plot of data and Correlation matrix ############
#Plot of response (mpg) and predictors  
pairs(Auto, pch = 19, cex=0.9,
      col = "darkolivegreen4",
      lower.panel=NULL,
      main = "Scatter plots of Data")

#We can clearly observe a strong negative trend between:
# 1) mpg vs displacement
# 2) mpg vs horsepower
# 3) mpg vs weight

#And a positive trend for:
# 1) mpg vs acceleration
# 2) mpg vs year

#Cylinders, Name and Origin don't display a clear association or 
#trend with the response

data = subset(Auto, select = -c(name)) #Drop name column

cor(data) #Correlation between response and predictors without name

#We can clearly observe a strong negative correlation between:
# 1) mpg vs displacement (-0.8051269)
# 2) mpg vs weight (-0.8322442)
# 3) mpg vs horsepower (-0.7784268)
# 4) mpg vs cylinders  (-0.7776175)

#And a positive correlation between (Not that strong):
# 1) mpg vs year (0.5805410)
# 2) mpg vs origin (0.5652088)
# 3) mpg vs acceleration (0.4233285)

############ Multilinear Regression ############
data.lm <- lm(mpg ~ ., data=data)
summary(data.lm)
summary(data.lm)$r.sq #R-Square

# P-values with *** ->Indicates values that are below the standard probability cutoff of 0.05.
#This implies that we can reject the null hypothesis for values below the 
#standard probability cutoff, which can allow us to conclude that there is a 
#relationship between the response and predictors.
#- Displacement, weight, year and origin are the predictors that display a relationship
#with the response

#Moreover, it seems that the standard errors for these predictors are very low
#(Displacement, weight, year and origin); hence we can acknowledge a good model 
#fit. Furthermore, large t-values can be used to observe the relationship between 
#the response and predictors; and the results indicate again the relationship of 
#displacement, weight, year and origin with the response .

#On the other hand, since this is a multilinear regression, the F-statistics 
#(overall effect of predictors), RSE and R square (model fitting) can give us 
#more information on the overall performance of our model. We can acknowledge that: 

# - F-statistic has a large value (252.4) -> Indicates that there is some relationship
# between response and predictors (Reject Null Hypothesis)
# - Residual standard error (RSE) has a relatively small value (3.328). However, the 
# accuracy of this depends on the "y" units.
# - Multiple R-squared has a large value (0.8215) which indicates a strong association 
# with the response. 

#For the year variable, we can observe:
# - A large t-value which helps us to reject the null hypothesis and conclude 
# that there is a strong relationship between the response (mpg) and my variable year
# - Our p-value (Small value) indicates that we can reject the null hypothesis since 
#the value is below the standard probability cutoff; hence, it is extremely unlikely 
#to observe such values if H0 is true.


############ Diagnostic Plots ############  
par(mfrow=c(2,2))
plot(data.lm, pch = 19, cex=0.9,lwd=2)

#1. Residuals (errors) vs fitted: There is linearity if the red line is flat.
#We can observe that there is a strong linearity behavior between the response 
#and inputs; however, we can observe a small deviation of the linear behavior at
#the tails of the plot. 

#Moreover, we can analyze the variance of the error terms. On this case, the plot
#displays a funnel shape that can help us to conclude that there is a non-constant
#variance behavior. 

#2.Normal Q-Q (Quantile-Quantile): This plot can help us to observe if the errors
#are normally distributed
#It seems the errors are not normally distributed, specifically at the end of my plot.

#3. Scale-location: Shows whether residuals are spread equally along with the  
#ranges of input variables (unusual yi for given xi).
#This can help us to check potential outliers. For these regression models, there 
#are no clear outliers.

#4.Residuals vs Leverage (unusual value for xi): Identifies influential data 
#points on your model. Those points can be identified when they are outside the 
#red dashed Cook's distance line. 
#Since we don't observe any value outside the Cook's line we can conclude that 
#there aren't unusual values for xi.


############ Collinearity ############  
#1. Variance inflation factor (VIF)
vif(data.lm)

#-VIF=1, no collinearity.
#-VIF>5, serious collinearity.

#Cylinders, displacement, horsepower and weight are the predictor that display a
#high correlation with the other predictors (VIF>5)

#2. Plot of response (mpg) and predictors  
pairs(data, pch = 19, cex=0.9,
      col = "turquoise3",
      lower.panel=NULL,
      main = "Scatter plots of Data")
#The scatter plots can also help us to observe collinearity between predictors.
#It seems that cylinders, displacement, horsepower and weigh display high 
#correlation (e.g. displacement vs horsepower)

#3. Correlation between response and predictors without "name"
cor(data)
#The top 5 highest correlations are: 
  #1) cylinders vs displacement (0.9508233)
  #2) weight vs displacement (0.9329944)
  #3) weight vs cylinders (0.8975273)
  #4) weight vs horsepower (0.8645377)
  #5) cylinders vs horsepower (0.8429834)


############ Interaction Terms ############ 
#All features + interactions 
#Case 1. All features + cylinders*displacement
summary(lm(mpg~cylinders + displacement + horsepower + weight + acceleration + 
             year + origin + cylinders*displacement, data=data))

#Case 2. All features + weight*displacement
summary(lm(mpg~cylinders + displacement + horsepower + weight + acceleration + 
             year + origin + weight*displacement, data=data))

#Case 3. All features + weight*cylinders
summary(lm(mpg~cylinders + displacement + horsepower + weight + acceleration + 
             year + origin + weight*cylinders, data=data))

#Case 4. All features + weight*horsepower
summary(lm(mpg~cylinders + displacement + horsepower + weight + acceleration + 
             year + origin + weight*horsepower, data=data))

#Case 5. All features + cylinders*horsepower
summary(lm(mpg~cylinders + displacement + horsepower + weight + acceleration + 
             year + origin + cylinders*horsepower, data=data))

#Case 6. All features + interactions: (cylinders*displacement + weight*displacement + 
#weight*cylinders + weight*horsepower + cylinders*horsepower)
summary(lm(mpg~cylinders + displacement + horsepower + weight + acceleration + 
        year + origin + cylinders*displacement + weight*displacement + 
        weight*cylinders + weight*horsepower + cylinders*horsepower, data=data))

#All of these interactions improve my original linear model (R-Square=0.8215), 
#specifically:
  #1. Case 6 (R-Square=0.8669)
  #2. Case 5 (R-Square=0.8621)
  #3. Case 4 (R-Square=0.8618)


#Problem 2 (5pt)
# Use the Carseats data set to answer the following questions:
#   (a) Fit a multiple regression model to predict Sales using Price, Urban, and US.
#   (b) Provide an interpretation of each coefficient in the model (note: some of the variables are qualitative).
#   (c) Write out the model in equation form.
#   (d) For which of the predictors can you reject the null hypothesis H_0: ??_j=0 ?
#   (e) On the basis of your answer to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the response.
#   (f) How well do the models in (a) and (e) fit the data?
#   (g) Is there evidence of outliers or high leverage observations in the model from (e)?

############ Load data ############
#install.packages("car")
head(Carseats) #Shows the first six lines of Auto data
summary(Carseats) #Summary statistics for Auto data
names(Carseats) #Names
is.data.frame(Carseats) 


############ Multilinear Regression ############
data_car.lm <- lm(Sales ~ Price+Urban+US, data=Carseats)
summary(data_car.lm)
summary(data_car.lm)$r.sq

#Using plot + abline 
par(mfrow=c(1,1))
plot(Carseats$Price, Carseats$Sales,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "Sales vs Price",
     xlab = "Price",
     ylab = "Sales")

abline(data_car.lm, col="red", lwd=3) #Model (regression line)

#Coding of the dummy variables (Qualitative variables)
contrasts(Urban)
contrasts(US)

############ Interpretation ############
#The coefficients for:
    #1) Price in the regression output is negative which indicates that there is not
    #a strong association with the prince and high sales.
    #2) Urban in the regression output is negative which indicates that there is not
    #a strong association with Urban locations and high sales.
    #3) US in the regression output is positive which indicates that there is a
    #strong association with US stores and high sales.
    #4) Intercept (B0): This is the expected mean value of the response when all 
    #predictors are equal to zero (X=0). 

#Diagnostic Plots
par(mfrow=c(2,2))
plot(data_car.lm, pch = 19, cex=0.9,lwd=2)


############ Equation form ############
# Y = Bo + B1*Price + B2*Urban + B3*US + error
# Yhat ??? Bohat + B1hat*Price + B2hat*Urban + B3hat*US
# Coefficients Estimation:  B = (X^T X)^{-1} X^T y, (B <- solve(t(X) %*% X, t(X) %*% y))

coeffs <- data_car.lm; coeffs
B0 <- summary(data_car.lm)$coefficients[1,1]; B0 #Intercept (Bo)
B1 <- summary(data_car.lm)$coefficients[2,1]; B1 #B1 (Price)
B2 <- summary(data_car.lm)$coefficients[3,1]; B2 #B2 (US)
B3 <- summary(data_car.lm)$coefficients[4,1]; B3 #B3 (Urban)

y <- B0 + Carseats["Price"]*B1+Carseats["Urban"]*B2+Carseats["US"]*B3

############ Null hypothesis ############
#According to the p-values and t- values (from the summary) we can reject the   
#null hypothesis (H_0: ??_j=0) for predictors Price and US. Thus, there is an 
#association between both predictors and response. 

#On the other hand, Urban (urban or rural locations) predictor display a    
#relatively "small" t-value and "large" p-value.

############ Multilinear Regression without URBAN ############
data_car.lm <- lm(Sales ~ Price+US, data=Carseats)
summary(data_car.lm)
summary(data_car.lm)$r.sq

#Using plot + abline 
par(mfrow=c(1,1))
plot(Carseats$Price, Carseats$Sales,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "Sales vs Price",
     xlab = "Price",
     ylab = "Sales")

abline(data_car.lm, col="red", lwd=3) #Model (regression line)

#Multilinear Regression without URBAN
    # Multiple R-squared:  0.2393
    # Adjusted R-squared:  0.2354 
    # F-statistic: 62.43

#Multilinear Regression with URBAN
    # Multiple R-squared:  0.2393
    # Adjusted R-squared:  0.2335 
    # F-statistic: 41.52 

#It seems that both models don't fit well the data since we observe a low 
#Multiple R-squared and adjusted R-squared. There is some relationship between 
#predictors and response since F-statistic is much greater than 1.

#Finally, the model without URBAN displays a better fit.

############ Diagnostic Plots ############   
par(mfrow=c(2,2))
plot(data_car.lm, pch = 19, cex=0.9,lwd=2)

#According to the "Residuals vs Fitted" and "Scale-location vs Fitted" plots, 
#the residuals appear to be well spread and no potential outliers are observed 

#Moreover, by using the "Residuals vs Leverage" plot we can conclude that 
#there is no influential case or cases of high leverage points (We don't even 
#see Cook's distance lines)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console
