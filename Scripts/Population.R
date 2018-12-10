library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)

PopulationDataset_Raw_Comments_Removed <- read_csv("Data sets/PopulationDataset_Raw_Comments_Removed.csv")
#View(PopulationDataset_Raw_Comments_Removed)
#colnames(PopulationDataset_Raw_Comments_Removed)

#Deleting unnecessary columns
PopulationDataset_Raw_Comments_Removed$`Country Name`<-NULL
PopulationDataset_Raw_Comments_Removed$`Indicator Name`<-NULL
PopulationDataset_Raw_Comments_Removed$`Indicator Code`<-NULL
#colnames(PopulationDataset_Raw_Comments_Removed)
#head(PopulationDataset_Raw_Comments_Removed)
Population_Data<- melt(PopulationDataset_Raw_Comments_Removed, id=c("Country Code"))

#Rename Columns as per this Project Standards
colnames(Population_Data)[1]="Country"
colnames(Population_Data)[2]="Year"
colnames(Population_Data)[3]="Population"
#head(Population_Data)
write.csv(Population_Data,file="Data sets/PopulationDataset.csv",row.names=FALSE)
#write.csv(Population_Data,file="Data sets/PopulationTest.csv")
#Population_Data %>% filter(Country == "IND")
#Population_Data

#cor(Population_Data$TotalDeaths,Population_Data$TotalAffected,method="spearman")