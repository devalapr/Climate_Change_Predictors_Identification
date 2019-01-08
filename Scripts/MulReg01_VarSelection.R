# Year: 2019
# Module: R Stats
# Title: Multiple linear regression with evaluation and visualisation

##### Activate libraries
library(Hmisc)
library(psych)
library(car)

library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)

##### Read the data and select variables

# Set a working directory
setwd("Data sets")
getwd()
# Read the FinalDataSet.csv data set and eliminate missing values
data <- read.csv(file = "MergedDataSet.csv", header=TRUE, na.strings="?")
#data$Population <- as.numeric(data$Population)
#head(data$Population)
#summary(data)
data$MeanTemperature<-NULL
data$Year<-NULL
data$CountryName<-NULL
data$Country<-NULL
data$MinTemperature<-NULL
data$MaxTemperature<-NULL

#data$AverageTemperature<-NULL
summary(data)
head(data)
#data_IND <- data %>% filter(Country == 'IND')
#summary(data_IND)
#head(data_IND)
# Select a subset of numeric variables for regression modelling
#data_IND.sel <- subset(data_IND, select = c(Year,Population,GDP,Emissions,AverageTemperature))
data.sel <- subset(data, select = c(Population,GDP,Emissions,anamoly))


##### Analyse variables for
#     - Normality of distribution
#     - Multiple collinearity
#     - Extreme values 
#     - Homoscedasticity (even distribution of residuals)
##### All such problems should have been fixed here

# Here we'll only make a brief visual inspection of vars
#pairs.panels(data_IND.sel, col="red")
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
                                   newdata = subset(train.sample, select=c(anamoly,Population,GDP,Emissions)))
valid.sample$Pred.Temp <- predict(fit, 
                                   newdata = subset(valid.sample, select=c(anamoly,Population,GDP,Emissions)))

# The theoretical model performance is defined here as R-Squared
summary(fit)
head(train.sample)
# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Temp, train.sample$anamoly), 2)
train.RMSE <- round(sqrt(mean((train.sample$Pred.Temp - train.sample$anamoly)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.Temp - train.sample$anamoly)))
c(train.corr^2, train.RMSE, train.MAE)
# 0.1225 0.0000 0.0000

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Temp, valid.sample$anamoly), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.Temp - valid.sample$anamoly)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.Temp - valid.sample$anamoly)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# 0.0676 1.0000 0.0000

# This results could be improved when eliminating extreme values and normalising vars


# First check for non-linearity properly if good go further
# This can only be done after the model was created
crPlots(fit)

# Eliminate extreme values
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("885", "904", "942")),]     

### Refit the model (2)
fit <- lm(anamoly ~ Population+GDP+Emissions, data=train.sample)
summary(fit)
#Multiple R-squared:  0.1473,	Adjusted R-squared:  0.144 
#F-statistic: 43.89 on 3 and 762 DF,  p-value: < 2.2e-16

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("923", "961", "883")),]     

### Refit the model (3)
fit <- lm(anamoly ~ Population+GDP+Emissions, data=train.sample)
summary(fit)
#Multiple R-squared:  0.1506,	Adjusted R-squared:  0.1472 
#F-statistic: 44.85 on 3 and 759 DF,  p-value: < 2.2e-16

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("866", "847", "833")),]     

### Refit the model (4)
fit <- lm(anamoly ~ Population+GDP+Emissions, data=train.sample)
summary(fit)
#Multiple R-squared:  0.1533,	Adjusted R-squared:  0.1499 
#F-statistic: 45.62 on 3 and 756 DF,  p-value: < 2.2e-16

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!

# Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
vif(fit)

### Refit the model (5) - drop Horsepower due to multiple collinearity
fit <- lm(anamoly ~ Population+GDP+Emissions, data=train.sample)
summary(fit)
#Multiple R-squared:  0.1533,	Adjusted R-squared:  0.1499 
#F-statistic: 45.62 on 3 and 756 DF,  p-value: < 2.2e-16
vif(fit)

### Refit the model (6) - drop Num.of.doors due to p-value
fit <- lm(anamoly ~ Population+GDP+Emissions, data=train.sample)
summary(fit) # R2=87.7%, F=542.5
vif(fit)

### VIF, F-ratio and p-values say it is good, so no need to do anything else

##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.Price <- predict(fit, 
                                   newdata = subset(train.sample, select=c(Price, Peak.rpm, Curb.weight)))
valid.sample$Pred.Price <- predict(fit, 
                                   newdata = subset(valid.sample, select=c(Price, Peak.rpm, Curb.weight)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Price, train.sample$Price), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Price - 10 ^ train.sample$Price)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Price - 10 ^ train.sample$Price)))
c(train.corr^2, train.RMSE, train.MAE)
# With all prep is: 0.8836 2670.0000 1759.0000 / As above
# Do nothing was:   0.7225 3997.0000 2676.0000 / See previous lesson

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$Price), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$Price)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$Price)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# With all prep is: 0.7396 5723.0000 3334.0000 / As above
# Do nothing was:   0.6889 4927.0000 3208.0000 / See previous lesson

# Small data set - Cross-validation should be used, but vars selection needs to be auto!
# These results and the model should now be interpreted

# Thank you