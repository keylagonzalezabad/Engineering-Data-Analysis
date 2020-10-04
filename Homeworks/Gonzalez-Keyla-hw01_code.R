####### Homework #1 ####### 
#Name: Keyla Gonzalez Abad

# PROBLEM 1: (1pt)
# What happens in this expression?
1:10 - 0:1
# ANSWER: 
#That's the subtraction of first vector and second vector and since the vector "0:1" 
#only has two values, the vector repeats and  performs the subtraction.
c(1-0,2-1,3-0,4-1,5-0,6-1,7-0,8-1,9-0,10-1)

# PROBLEM 2: (1pt)
# Create a vector consisting of the numbers 1 to N, where 1 appears
# once, 2 appears twice, 3 appears 3 times,...
# Show results for N=5.
# ANSWER:
N=5; vect=rep(1:N, rep(1:N)); vect


# PROBLEM 3: (2pts)
# Data generation and matrix indexing
# (a) Generate a vector with 25 elements and each element independently follows a normal 
# distribution (with mean =0 and sd=1);
# ANSWER:

N=25
norm<-rnorm(N,0,1);norm
mean(norm)
sd(norm)
plot(norm)

# (b) Reshape this vector into a 5 by 5 matrix in two ways (arranged by row and column);
# ANSWER:
N=5
matrix(norm,nrow=N) #Arranged by row
matrix(norm,ncol=N) #Arranged by column
#All of them display the same result 

mat=matrix(norm,nrow=N)
dim(mat)
is.matrix(mat)

# (c) Similarly, generate another vector with 100 elements and plot its histogram.
# ANSWER:
#Normal distribution of 100 elements
N=100
norm_100<-rnorm(N,0,1); norm_100
mean(norm_100)
sd(norm_100)

#Frequency Histogram
hist(norm_100,
     main="Histogram of rnorm(100)",
     col="#009999",
     xlab="Normal distribution of 100 numbers")

#Projection of norm_100 points
rug(norm_100,lwd=2,col="blue")

#Density Histogram
hist(norm_100,
     freq=FALSE,
     main="Histogram of rnorm(100)",
     col="#009999",
     xlab="Normal distribution of 100 numbers")

#Kernal density curve for distribution of a variable
lines(density(norm_100),col="red",lwd=2)

#Projection of norm_100 points
rug(norm_100,lwd=2,col="blue")

# PROBLEM 4: (2pts)
# (a) Upload the Auto data set, which is in the ISLR library. Understand information about this data 
# set by one of the ways we introduced in class (like "help(Auto)" and names(Auto))
# ANSWER:

library(ISLR)
?Auto #Help Auto for detailed information

head(Auto) #Show the first six lines of Auto data
summary(Auto) #Summary statistics for Auto data
names(Auto) #Names
typeof(Auto) #List

#Entire Features
plot(Auto,
     cex=0.5, #Magnitude
     col = "darkslategray", #Color
     pch = 19, # Use solid circles for points
     main = "Features Plot") 

# (b) Make a scatterplot between every pair of the following variables (try to plot all scatterplots
# in one figure; hint: use pairs() command): "mpg", "displacement", "horsepower", "weight", 
# "acceleration". By observing the plots, do you think the two variables in each scatterplot are 
# correlated? If so, how?
# ANSWER:

Quant=Auto[c("mpg", "displacement", "horsepower", "weight","acceleration")]
head(Quant)

pairs(Quant, pch = 19, cex=0.9,
      col = Auto[["cylinders"]],
      lower.panel=NULL,
      main = "Scatter plots of Data")

#It seems that most of the variables are correlated with each other since 
#they display a clear trend (linear or polynomial) between them.  
#However, "Weight vs acceleration", "displacement vs acceleration" and 
#"mpg vs acceleration" don't show a clear trend between them. Hence, they are 
#probably not correlated.

# (c) Draw a line on the scatterplot of mpg vs. horsepower to represent relationship between the two 
# variables.
# ANSWER:

#install.packages("ggExtra")
library(ggplot2)
library(ggExtra)

Auto[["cylinders"]]=as.factor(Auto[["cylinders"]]) #Convert cylinders as a grouping variable

mpg_hwp=ggplot(Auto, aes(x = horsepower, y = mpg))

mpg_hwp + 
  geom_point(aes(shape = cylinders, color = cylinders)) +
  geom_smooth(method = lm)+ ggtitle("Mpg vs Horsepower") +
  theme(plot.title = element_text(hjust = 0.5))

# (d) Is there a better way to represent their relationship rather than the linear model you just  
# drew? (No need to use a mathematical formula. Just draw something on the figure) 
# ANSWER:

#We can use histograms, polynomial trend and points projection.

g=mpg_hwp + 
  geom_point(aes(shape = cylinders, color = cylinders)) + #Scatter points by cylinders
  geom_rug(aes(color = cylinders)) + #Projection of "Mpg vs Horsepower" points
  geom_smooth(color = "red",se=F) + #Local Polynomial Regression Fitting
  geom_smooth(method = lm,color = "black",se=F) + #Linear Regression
  ggtitle("Mpg vs Horsepower") + #Title
  theme(plot.title = element_text(hjust = 0.5))

ggMarginal(g,type = "histogram", fill="transparent") #Histogram of each variable

# PROBLEM 5: (2pts)
# Create a 6x3 matrix named 'dat' with
# row names "Adam", "Anna", "Bill", "Berta", "Chris", "Cindy".
# and column names "Age", "Gender", "Height".
# Fill the columns with realistically looking random numbers
# so someone could actually believe they are real data.
# Use numeric codes 0 and 1 for gender.
# Assume the ages to be uniformly distributed over 18:24.
# Assume men's mean height is 68in and women's 4in less, 
# but both heights have the same sdev.
# Finally assume normal distributions for heights,
# except the numbers should be whole inches to look realistic.
# ANSWER:

N=6
Age=floor(runif(N, min=18, max=24)); Age #Random uniformly distributed ages
Gender=rep(0:1,N/2); Gender #Men (code 0), Women (code 1)
Height=replace(Gender, Gender==0, round(rnorm(N/2, 68, 2))) #Men mean height=68in and sdev=2
Height=replace(Height, Height==1, round(rnorm(N/2, 64, 2)));Height #Women mean height=64in and sdev=2

dat=cbind(Age,Gender,Height)

rownames(dat) <- c("Adam", "Anna", "Bill", "Berta", "Chris", "Cindy")
colnames(dat) <- c("Age", "Gender", "Height")
dat

dim(dat)
is.matrix(dat)

# BONUS QUESTION: (1 extra pt)
# Create an artificial data matrix with 100000 rows and
# columns "Gender", "Age", "Graduated".
# Assume P(female)=0.57 (code 1) and P(male)=0.43 (code 0).
# Assume Age is uniformly distributed over 18:30 (integers).
# Assume Graduated is a binary variable with codes 0/1 (no/yes).
# Assume further the probabilities of having graduated are
# a function of Age as follows:  0,.01,.1,.2,.7,.95,.99,.99,...
# for ages 18 and up.
# (Hint: To fill 'Graduated', create first a column 'prob' that for
# each individual contains the probability of having graduated
# dependent on 'Age'.  This is the tricky part.
# Then set 'Graduated' to  runif(100000)<prob.)
# Finally, using the function 'table()', form frequency tables of
# - Graduated,
# - Gender x Age,
# - Gender x Age x Graduated,  and
# - Age, but only for the Graduates.
# Check whether the output of the 'table' function is of type 'array'.
# ANSWER:

nrow=100000 #Number of rows

#Gender based on P(male)=0.43 (code 0) and P(female)=0.57 (code 1)
Gender=sample(rep(0:1,nrow), size = nrow, prob=rep(c(0.43, 0.57),nrow))

#Age uniformly distributed over 18:30 (integers)
Age=floor(runif(nrow, min=18, max=30))

#Probability of Graduated based on age 
y <- seq(0, 1, by=1/12); y 
Age.lvl <- 18:30; Age.lvl
prob<-y[match(Age, Age.lvl)]; prob  #match: vector of the positions of (first) matches of its 
                                    #first argument in its second.

#Matrix of Age,Gender,Graduated
dat_prob=cbind(Age,Gender,prob); dat_prob
Graduated <-runif(nrow)<prob
dat=cbind(Age,Gender,Graduated); dat
dim(dat)
is.matrix(dat)

#Frequency table of Graduated
tab_grad=table(Gender); tab_grad
is.array(tab_grad)

#Frequency table of Gender x Age
tab_GA=table(Gender,Age); tab_GA
is.array(tab_GA)

#Frequency table of Gender x Age x Graduated
tab_GAG=table(Gender,Age,Graduated); tab_GAG

is.array(tab_GAG)

#Frequency table of Age, but only for the Graduates.
tab_AG=table(Age[Graduated==0],dnn="Ages for the Graduates"); tab_AG
is.array(tab_AG)

# BONUS QUESTION #2: (1 extra pt)

# Create the following matrix elegantly:
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    0    1    0    1    0    1    0    1    0     1
# [2,]    1    0    1    0    1    0    1    0    1     0
# [3,]    0    1    0    1    0    1    0    1    0     1
# [4,]    1    0    1    0    1    0    1    0    1     0
# [5,]    0    1    0    1    0    1    0    1    0     1
# [6,]    1    0    1    0    1    0    1    0    1     0
# [7,]    0    1    0    1    0    1    0    1    0     1
# [8,]    1    0    1    0    1    0    1    0    1     0
# [9,]    0    1    0    1    0    1    0    1    0     1
# [10,]   1    0    1    0    1    0    1    0    1     0

# ANSWER:
mat=matrix(c(rep(0:1,5),rep(1:0,5)),nrow=10,ncol=10); mat
dim(mat)
is.matrix(mat)

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console