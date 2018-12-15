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
 Merged <- merge(Population_Data,TemperaturDataSet,by=c("Country","Year"))
# head(Merged)

Country_List <- unique(Merged$Country)
#Country_List <- c("ERI")

Population_final = Population_Data[FALSE,]
for (Country1 in Country_List) {
  Population_Country <- Population_Data %>% filter(Country == Country1)
  temp_country <- na.omit(Population_Country)
  values <- lm(temp_country$Population ~ temp_country$Year, temp_country)
  print(summary(values))
  for(i in 1:nrow(Population_Country))
  {
    if(is.na(Population_Country$Population[i]))
    {
      Population_Country$Population[i] = as.numeric(values$coef[1]) + as.numeric(values$coef[2])*as.numeric(Population_Data$Year[i])
    }
  }
  Population_final <- rbind(Population_final,Population_Country)
}
head(Population_final)
write.csv(Population_final,file="Data sets/PopulationDataset_03_Preprocessed.csv",row.names=FALSE)
#head(Population_Data)
#write.csv(Population_Data,file="Data sets/PopulationDataset.csv",row.names=FALSE)
#write.csv(Population_Data,file="Data sets/PopulationTest.csv")
#Population_Data %>% filter(Country == "IND")
#Population_Data

#cor(Population_Data$TotalDeaths,Population_Data$TotalAffected,method="spearman")