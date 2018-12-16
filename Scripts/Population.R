library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)

PopulationDataset_02_Comments_Removed <- read_csv("Data sets/PopulationDataset_02_Comments_Removed.csv")
#View(PopulationDataset_Raw_Comments_Removed)
#colnames(PopulationDataset_02_Comments_Removed)
TemperaturDataSet <- read_csv("Data sets/TemperatureDatasetFinal.csv")
head(TemperaturDataSet)

#Deleting unnecessary columns
PopulationDataset_02_Comments_Removed$`Country Name`<-NULL
PopulationDataset_02_Comments_Removed$`Indicator Name`<-NULL
PopulationDataset_02_Comments_Removed$`Indicator Code`<-NULL
PopulationDataset_02_Comments_Removed<-PopulationDataset_02_Comments_Removed[!(PopulationDataset_02_Comments_Removed$`Country Code`=="INX"),]
TemperaturDataSet$MinTemperature<-NULL
TemperaturDataSet$MaxTemperature<-NULL
#colnames(PopulationDataset_02_Comments_Removed)
#head(PopulationDataset_02_Comments_Removed)
Population_Data<- melt(PopulationDataset_02_Comments_Removed, id=c("Country Code"))

#Rename Columns as per this Project Standards
colnames(Population_Data)[1]="Country"
colnames(Population_Data)[2]="Year"
colnames(Population_Data)[3]="Population"
Population_Data <- Population_Data[order(Population_Data$Country),]
#Population_Data[is.na(Population_Data)] <- 0
# Merged <- merge(Population_Data,TemperaturDataSet,by=c("Country","Year"))
# head(Merged)

Country_List <- unique(Population_Data$Country)
#Country_List <- c("ERI")

Population_final = Population_Data[FALSE,]
for (Country1 in Country_List) {
  cat("Current Country is ", Country1)
  Population_Country <- Population_Data %>% filter(Country == Country1)
  #cat("Rows in Population_Country",nrow(Population_Country),"\n")
  #print(Population_Country)
  temp_country <- na.omit(Population_Country)
  #cat("Data after ommiting null values","\n")
  # print(temp_country)
  # transform(temp_country, Year = as.numeric(Year))
  temp_country$Year <- as.numeric(as.character(temp_country$Year))
  Population_Country$Year <- as.numeric(as.character(Population_Country$Year))
  Population_Data$Year <- as.numeric(as.character(Population_Data$Year))
  #cat("Check for factor ",is.factor(temp_country$Year))
  #print(temp_country$Year)
  linearMod <- lm(Population ~ Year, data=temp_country)
  print(summary(linearMod))
  cat("Liner Model Coeffs",linearMod$coefficients,"\n")
  for(i in 1:nrow(Population_Country))
  {
    if(is.na(Population_Country$Population[i]))
    {
      cat("Yep Yep Johnny Depp",i,"\n")
      Population_Country$Population[i] = as.numeric(linearMod$coefficients[1]) + as.numeric(linearMod$coefficients[2])*(Population_Data$Year[i])
      #cat(as.numeric(as.numeric(Population_Country$Population[i]),"=",linearMod$coefficients[1]),as.numeric(linearMod$coefficients[2]),as.numeric(Population_Data$Year[i]))
      #cat("Year",Population_Data$Year[i])
      cat("\n")
      }
  }
  Population_final <- rbind(Population_final,Population_Country)
}
#head(Population_final)
write.csv(Population_final,file="Data sets/PopulationDataset_03_Preprocessed.csv",row.names=FALSE)
#head(Population_Data)
#write.csv(Population_Data,file="Data sets/PopulationDataset.csv",row.names=FALSE)
#write.csv(Population_Data,file="Data sets/PopulationTest.csv")
#Population_Data %>% filter(Country == "IND")
#Population_Data

#cor(Population_Data$TotalDeaths,Population_Data$TotalAffected,method="spearman")