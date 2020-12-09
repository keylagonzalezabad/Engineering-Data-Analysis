#Data Visualization

############ Libraries ############
#install.packages('tidyverse')
library(corrplot) #Correlation plot
library(tidyverse)
library(ggplot2) #Plotting
library(GGally) #Plotting

############ Load data ############
path <- #*** Copy your train path***
mydata <- read.table(path, header=TRUE, sep=",")

summary(mydata) #Summary statistics for mydata data
names(mydata) #Names
is.data.frame(mydata) #Dataframe
dim(mydata) #66 Predictors and 152 Samples

############ Data Visualization ############
#Plot of response Rt  
plot(mydata$Mean.R.,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "Rt vs Days",
     xlab = "Days",
     ylab = "Rt")

#Plot of Percent Change Workplaces (B-11) 
plot(mydata$workplaces_percent_change_from_baseline_11,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "Percent Change Workplaces (B-11) vs Days",
     xlab = "Days",
     ylab = "Percent Change Workplaces (B-11)")

#Plot of response Rt and Percent Change Workplaces (B-11) 
plot(mydata$Mean.R., mydata$workplaces_percent_change_from_baseline_11,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "Rt vs Percent Change Workplaces (B-11)",
     xlab = "Percent Change Workplaces (B-11)",
     ylab = "Rt")

#Plot of response Rt and Percent Change Transit Stations (B-11) 
plot(mydata$Mean.R., mydata$transit_stations_percent_change_from_baseline_11,
     pch = 19, cex=0.9,
     col = "darkolivegreen4",
     main = "Rt vs Percent Change Transit Stations (B-11)",
     xlab = "Percent Change Transit Stations (B-11)",
     ylab = "Rt")

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

############ CLEAN UP ############
rm(list = ls()) # Clear environment
dev.off()  # Clear plots
cat("\014")  # Clear console




