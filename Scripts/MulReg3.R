# Year: 2016
# Module: R Stats
# Title: Multiple linear regression with evaluation and visualisation
# Author: Jacob Cybulski 
#
# Data: http://archive.ics.uci.edu/ml/datasets/Automobile
# Data consist of the following entities: 
# (a) the specification of an auto in terms of various characteristics, 
# (b) its assigned insurance risk rating, i.e.
#     degree to which the auto is more risky than its price indicates
# (c) its normalized losses in use as compared to other cars, i.e.
#     the relative average loss payment per insured vehicle year.
# We will be predicting price based on car specifications.

##### Activate libraries

# install.packages("Hmisc", dependencies = TRUE)
# install.packages("psych", dependencies = TRUE)
# install.packages("car", dependencies = TRUE)
library(Hmisc)
library(psych)
library(car)

library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)

##### Read and clean the data and select variables

# Set a working directory
setwd("Data sets")
getwd()
# Read the Automobile.csv data set and eliminate missing values
data <- read.csv(file = "FinaldatasetFinal.csv", header=TRUE, na.strings="?")
summary(data)
data$Country.Name<-NULL
data$MeanTemperature<-NULL
#data$AverageTemperature<-NULL
summary(data)
head(data)
data_IND <- data %>% filter(Country == 'IND')
summary(data_IND)
head(data_IND)
# Select a subset of numeric variables for regression modelling
data_IND.sel <- subset(data_IND, select = c(Year,Population,GDP,Emissions,AverageTemperature))
data.sel <- subset(data, select = c(Population,GDP,Emissions,anamoly))


##### Analyse variables for
#     - Normality of distribution
#     - Multiple collinearity
#     - Extreme values 
#     - Homoscedasticity (even distribution of residuals)
##### All such problems should have been fixed here

# Here we'll only make a brief visual inspection of vars
pairs.panels(data_IND.sel, col="red")
pairs.panels(data.sel, col="red")


##### Develop a linear model
#     The model will be built using the training sample of the data
#     The model will be validated using the validation sample of the data

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(data$anamoly), round(length(data.sel$anamoly) * train.size))
train.sample <- data.sel[train.index,]
valid.sample <- data.sel[-train.index,]


### Multiple regression model utilises a simple formula:
#    Price = B0 + B1 x Horsepower + B2 x Curb.weight + B3 x City.mpg
#
# We will perform additional tests on the trainig data

# We will use a stepwise selection of variables by backwards elimination
# We will consider all candidate variables and eliminate one at the time

fit <- lm(anamoly ~ Population+GDP+Emissions, data=train.sample)
summary(fit) # R2=73%

plot(fit)

# Observe that R-Sq almost did not change and all Ps are good

# Note however that we found some extreme values, which could be removed, here they are
#train.sample[which(rownames(train.sample) %in% c("50", "12", "24")),]

##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.Temp <- predict(fit, 
                                   newdata = subset(train.sample, select=c(AverageTemperature,Year,Population,GDP,Emissions)))
valid.sample$Pred.Temp <- predict(fit, 
                                   newdata = subset(valid.sample, select=c(AverageTemperature,Year,Population,GDP,Emissions)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Temp, train.sample$Temp), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.Temp - train.sample$Temp)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.Temp - train.sample$Temp)))
c(train.corr^2, train.RMSE, train.MAE)
# 0.7225 3997.0000 2676.0000

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Temp, valid.sample$Temp), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.Temp - valid.sample$Temp)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.Temp - valid.sample$Temp)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# 0.6889 4927.0000 3208.0000

# This results could be improved when eliminating extreme values and normalising vars

# Thank you