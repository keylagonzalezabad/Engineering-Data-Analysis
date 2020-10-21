####### Homework #4 ####### 
#Name: Keyla Gonzalez Abad

############ Libraries ############
library(MASS)
library (car)
library(corrplot)
library(plotly)
library(ggplot2)

############ Load data ############

?Boston #This data sets contain housing values in suburbs of Boston
data(Boston)
head(Boston )#Shows the first six lines of Boston data
summary(Boston) #Summary statistics for Auto data
names(Boston) #Names
is.data.frame(Boston) 

log_medv = log(Boston$medv);log_medv
pred = subset(Boston, select = -c(medv));pred
data = cbind(pred, log_medv = log_medv); data
head(data)
is.data.frame(data) 

############ Data Visualization ############

#Plot of response (log_medv) and predictors  
pairs(data, pch = 19, cex=0.9,
      col = "darkolivegreen4",
      lower.panel=NULL,
      main = "Scatter plots of Data")

#Plot of response (log_medv) and rm 
plot(data$rm, data$log_medv,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "log(medv) vs rm",
     xlab = "rm (avg number of rooms per house)",
     ylab = "log(medv)")

#Correlation plot
corr_matrix<-cor(data)
corrplot(corr_matrix, type="upper")

#Histograms
p<-ggplot(Boston, aes(x=Boston$medv)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="medv histogram",x="medv", y = "Count");p

  #To obtain a Gaussian Distribution
p<-ggplot(data, aes(x=data$log_medv)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="log_medv histogram",x="log_medv", y = "Count");p

p<-ggplot(Boston, aes(x=Boston$crim)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="Crim histogram",x="Crim", y = "Count");p

p<-ggplot(Boston, aes(x=chas)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="Chas histogram",x="Chas", y = "Count");p

p<-ggplot(Boston, aes(x=Boston$rm)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="rm histogram",x="rm", y = "Count");p

p<-ggplot(Boston, aes(x=Boston$age)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="age histogram",x="age", y = "Count");p

p<-ggplot(Boston, aes(x=Boston$zn)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="zn histogram",x="zn", y = "Count");p

p<-ggplot(Boston, aes(x=Boston$dis)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="dis histogram",x="dis", y = "Count");p

p<-ggplot(Boston, aes(x=Boston$rad)) + 
  geom_histogram(color="black", fill="white")+
  labs(title="rad histogram",x="rad", y = "Count");p

#Quantiles
quantile(Boston$crim, .90) #High Crime Rate
quantile(Boston$crim, .10) #Low Crime Rate

#For a High Crime Rate
hcrim<-subset(Boston, crim>quantile(Boston$crim, .90))
summary(hcrim)

#For a Young house
lage<-subset(Boston, age>quantile(Boston$age, .10))
summary(lage)

#For high rm
hrm<-subset(Boston, rm>quantile(Boston$rm, .90))
summary(hrm)

#For high dis
hdis<-subset(Boston, dis>quantile(Boston$dis, .90))
summary(hdis)

table(Boston$chas) #Count

#- Purpose: The main purpose of this research is to analyze and predict 
#housing values by using 7 predictors such as crim (per capita crime rate 
#by town), rm (average number of rooms per house) and rad (index of 
#accessibility to radial highway). 

#- Summarize the predictors and response:
#Our dataset contains:
#- A mean crime rate of ~4 with a high crime rate (90th percentile) of ~11 
#and a low (10th percentile) of 0.04. 
#- The number of houses nearby the Charles River is 35 out of 506; hence, 
#the majority houses are not by the Charles River.
#- An average number of rooms per house is ~6 with a maximum value of ~9  
#and a minimum of ~4.
#- The average of proportion of residential land zoned for lots over 
#25,000 sq.ft is ~11 with a minimum proportion of 0 and maximum of 100.
#- The average of proportion of houses built prior to 1940 (age) is ~69 with 
#a high age (90th percentile) ~99 of and low age of ~ 27 (10th percentile)
#- The mean distance to major employment centers is ~3.8 with a maximum  
#value of ~1 and a minimum of ~12.
#- The average accessibility to radial highways is around 9.6 with a maximum  
#value of ~24 and a minimum of ~1.
#- Finally, we can analyze towns with high crime rate and how the other
#predictors are associated with it. Thus, for a high crime rate, we can observe
#an average age of ~94, distance employment centers of ~1.7 and houses over 
#25,000 sq.ft of 0. 


############ Multilinear Regression Model ############

#Structural properties of a house, Accessibility, Neighborhood 
data.lm <- lm(log_medv ~ crim + chas + rm + age + zn + dis + rad, data=data)
summary(data.lm)
summary(data.lm)$r.sq #R-Square
coef(data.lm)
exp(coef(data.lm))

#Imaginary Mean
pred_lm<-data[c("crim","chas","rm","age","zn","dis","rad")]
new_data_m<-apply(pred_lm, 2, mean, na.rm=T) #2 if COLUMNS, na.rm (Missing Data)
new_data_m

############ Predictions ############

######1. Prediction model on low age
data.refy <- predict(data.lm, newdata=data.frame
                     (crim=4, chas=0, zn=8, rm=6, age=75, 
                       dis=3.5, rad=10, log_medv=NA))
data.refy
exp(data.refy) #Exponential in order to interpret our prediction

#Prediction Interval
pi<-predict(data.lm, newdata=data.frame
            (crim=4, chas=0, zn=8, rm=6, age=75, 
              dis=3.5, rad=10, log_medv=NA), interval="predict");pi

exp(pi) #Exponential in order to interpret our prediction

#Confidence Interval
ci<-predict(data.lm, newdata=data.frame
            (crim=4, chas=0, zn=8, rm=6, age=75, 
              dis=3.5, rad=10, log_medv=NA), interval="confidence");ci

exp(ci) #Exponential in order to interpret our prediction

#Quantile-based non-parametric PI:
non_pi.resid<-data.refy + quantile(resid(data.lm), prob=c(.025,.975))
non_pi.resid
exp(non_pi.resid) #Exponential in order to interpret our prediction

non_pi.stdres<-data.refy + quantile(stdres(data.lm), prob=c(.025,.975)) 
non_pi.stdres
exp(non_pi.stdres) #Exponential in order to interpret our prediction


######2. Prediction model on high crime rate 
data.refy <- predict(data.lm, newdata=data.frame
                     (crim=23, chas=0, zn=0, rm=6, age=94, 
                       dis=1.7, rad=24, log_medv=NA))
data.refy
exp(data.refy) #Exponential in order to interpret our prediction

#Confidence Interval
pi<-predict(data.lm, newdata=data.frame
            (crim=23, chas=0, zn=0, rm=6, age=94, 
              dis=1.7, rad=24, log_medv=NA), interval="predict");pi

exp(pi) #Exponential in order to interpret our prediction

#Prediction Interval
ci<-predict(data.lm, newdata=data.frame
            (crim=23, chas=0, zn=0, rm=6, age=94, 
              dis=1.7, rad=24, log_medv=NA), interval="confidence");ci

exp(ci) #Exponential in order to interpret our prediction

#Quantile-based non-parametric PI:
non_pi.resid<-data.refy + quantile(resid(data.lm), prob=c(.025,.975))
non_pi.resid
exp(non_pi.resid) #Exponential in order to interpret our prediction

non_pi.stdres<-data.refy + quantile(stdres(data.lm), prob=c(.025,.975)) 
non_pi.stdres
exp(non_pi.stdres) #Exponential in order to interpret our prediction

#3. Prediction model on high number of rooms 
data.refy <- predict(data.lm, newdata=data.frame
                     (crim=1, chas=0, zn=25, rm=8, age=63, 
                       dis=4, rad=6, log_medv=NA))
data.refy
exp(data.refy) #Exponential in order to interpret our prediction

#Prediction Interval
pi<-predict(data.lm, newdata=data.frame
            (crim=1, chas=0, zn=25, rm=8, age=63, 
              dis=4, rad=6, log_medv=NA), interval="predict");pi

exp(pi) #Exponential in order to interpret our prediction

#Confidence Interval
ci<-predict(data.lm, newdata=data.frame
            (crim=1, chas=0, zn=25, rm=8, age=63, 
              dis=4, rad=6, log_medv=NA), interval="confidence");ci

exp(ci) #Exponential in order to interpret our prediction

#Quantile-based non-parametric PI:
non_pi.resid<-data.refy + quantile(resid(data.lm), prob=c(.025,.975))
non_pi.resid
exp(non_pi.resid) #Exponential in order to interpret our prediction

non_pi.stdres<-data.refy + quantile(stdres(data.lm), prob=c(.025,.975)) 
non_pi.stdres
exp(non_pi.stdres) #Exponential in order to interpret our prediction

#4. Prediction model on high distance to employment centers
data.refy <- predict(data.lm, newdata=data.frame
                     (crim=0, chas=0, zn=52, rm=6, age=36, 
                       dis=8, rad=5, log_medv=NA))
data.refy
exp(data.refy) #Exponential in order to interpret our prediction

#Prediction Interval
pi<-predict(data.lm, newdata=data.frame
            (crim=0, chas=0, zn=52, rm=6, age=36, 
              dis=8, rad=5, log_medv=NA), interval="predict");pi

exp(pi) #Exponential in order to interpret our prediction

#Confidence Interval
ci<-predict(data.lm, newdata=data.frame
            (crim=0, chas=0, zn=52, rm=6, age=36, 
              dis=8, rad=5, log_medv=NA), interval="confidence");ci

exp(ci) #Exponential in order to interpret our prediction

#Quantile-based non-parametric PI:
non_pi.resid<-data.refy + quantile(resid(data.lm), prob=c(.025,.975))
non_pi.resid
exp(non_pi.resid) #Exponential in order to interpret our prediction

non_pi.stdres<-data.refy + quantile(stdres(data.lm), prob=c(.025,.975)) 
non_pi.stdres
exp(non_pi.stdres) #Exponential in order to interpret our prediction

#5. Prediction model on reference point (Imaginary case close to mean)
data.refy <- predict(data.lm, newdata=data.frame
                     (crim=4, chas=0, rm=6, age=69, zn=12,
                       dis=8, rad=10, log_medv=NA))
data.refy
exp(data.refy) #Exponential in order to interpret our prediction

#Prediction Interval
pi<-predict(data.lm, newdata=data.frame
            (crim=4, chas=0, rm=6, age=69, zn=12,
              dis=8, rad=10, log_medv=NA), interval="predict");pi

exp(pi) #Exponential in order to interpret our prediction

#Confidence Interval
ci<-predict(data.lm, newdata=data.frame
            (crim=4, chas=0, rm=6, age=69, zn=12,
              dis=8, rad=10, log_medv=NA), interval="confidence");ci

exp(ci) #Exponential in order to interpret our prediction

#Quantile-based non-parametric PI:
non_pi.resid<-data.refy + quantile(resid(data.lm), prob=c(.025,.975))
non_pi.resid
exp(non_pi.resid) #Exponential in order to interpret our prediction

non_pi.stdres<-data.refy + quantile(stdres(data.lm), prob=c(.025,.975)) 
non_pi.stdres
exp(non_pi.stdres) #Exponential in order to interpret our prediction

############ Analysis ############

#Confidence Interval Predictors (log_medv)
confint (data.lm)

#Coefficients 
coef(data.lm)
coef<-exp(coef(data.lm));coef
#Percent increase in the response for every one-unit increase in the 

#independent variables.
(coef[1]-1)*100 #Intercept
(coef[2]-1)*100 #for every one-unit increase in crim, medv decreases by about 1.45%
(coef[3]-1)*100 #for every one-unit increase in chas, medv increases by about 18%
(coef[4]-1)*100 #for every one-unit increase in rm, medv increases by about 32%
(coef[5]-1)*100 #for every one-unit increase in age, medv decreases by about 0.4%
(coef[6]-1)*100 #for every one-unit increase in zn, medv increases by about 0.15%
(coef[7]-1)*100 #for every one-unit increase in dis, medv decreases by about 3.5%
(coef[8]-1)*100 #for every one-unit increase in rad, medv decreases by about 0.7%
#

#Random data set for predictions
set.seed(20)
crim_test=sample(0:89, size=20, replace=TRUE); crim_test
zn_test=sample(0:100, size=20, replace=TRUE); zn_test
chas_test=sample(0:1, size=20, replace=TRUE); chas_test
rm_test=sample(3:9, size=20, replace=TRUE); rm_test
age_test=sample(3:100, size=20, replace=TRUE); age_test
dis_test=sample(1:12, size=20, replace=TRUE); dis_test
rad_test=sample(1:24, size=20, replace=TRUE); rad_test

#Prediction model 
data.refy <- predict(data.lm, newdata=data.frame
                     (crim=crim_test, chas=chas_test, rm=rm_test, age=age_test,
                       zn=zn_test, dis=dis_test, rad=rad_test, log_medv=NA))
data.refy
exp(data.refy) #Exponential in order to interpret our prediction
pred_mdve<-exp(data.refy)*1000; pred_mdve
summary(pred_mdve)

top<-sort(pred_mdve, decreasing = TRUE)  #Sort descending order 
head(top)

#Prediction Interval
pi<-predict(data.lm, newdata=data.frame
            (crim=crim_test, chas=chas_test, rm=rm_test, age=age_test,
              zn=zn_test, dis=dis_test, rad=rad_test, log_medv=NA), 
            interval="predict");pi

exp(pi) #Exponential in order to interpret our prediction

#Confidence Interval
ci<-predict(data.lm, newdata=data.frame
            (crim=crim_test, chas=chas_test, rm=rm_test, age=age_test,
              zn=zn_test, dis=dis_test, rad=rad_test, log_medv=NA), 
            interval="confidence");ci

exp(ci) #Exponential in order to interpret our prediction

#Quantile-based non-parametric PI:
non_pi.resid<-data.refy + quantile(resid(data.lm), prob=c(.025,.975))
non_pi.resid
exp(non_pi.resid) #Exponential in order to interpret our prediction

non_pi.stdres<-data.refy + quantile(stdres(data.lm), prob=c(.025,.975)) 
non_pi.stdres
exp(non_pi.stdres) #Exponential in order to interpret our prediction

############ Revision ############

data.refy <- predict(data.lm, newdata=data.frame
                     (crim=60, chas=1, zn=71, rm=9, age=42, 
                       dis=1, rad=4, log_medv=NA))
data.refy
exp(data.refy) #Exponential in order to interpret our prediction

############ Diagnostic Plots ############  
par(mfrow=c(2,2))
plot(data.lm, pch = 19, cex=0.9,lwd=2)

############ Collinearity ############  
#1. Variance inflation factor (VIF)
vif(data.lm)

#-VIF=1, no collinearity.
#-VIF>5, serious collinearity.

############ Suggestion of Multilinear Regression Model ############
data.lm <- lm(log_medv ~ ., data=data) #All predictors
summary(data.lm)
summary(data.lm)$r.sq #R-Square

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console

