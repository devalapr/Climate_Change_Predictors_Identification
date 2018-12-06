library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(mice)
#print(gdpPercent)
emissionData<- melt(emissions, id=c("Country Name","Country Code","Indicator Name","Indicator Code"))


colnames(emissionData)[5]<-"Year"
colnames(emissionData)[6]<-"Emissions"
#print(emissionData)
write.csv(emissionData,file="EmissionDataSet.csv")