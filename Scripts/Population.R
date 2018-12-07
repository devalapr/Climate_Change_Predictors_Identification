library(reshape2)
library(ggplot2)
library(dplyr)
library(readr)
PopulationDataset_Raw <- read_delim("Data sets/PopulationDataset_Raw.csv", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
View(PopulationDataset_Raw)
Population_Data<- melt(PopulationDataset_Raw, id=c("Country"))
write.csv(Population_Data,file="Data sets/PopulationTest.csv")

Population_Data %>% filter(Country == "IND")
Population_Data

cor(Population_Data$TotalDeaths,Population_Data$TotalAffected,method="spearman")