library(readr)
library(dplyr)
library(tidyr)
library(sqldf)

#CO2DS <- read_csv("emissions.csv")
POPDS <- read_csv("PopulationDataset_03_Preprocessed.csv")
TMPDS <- read_csv("TemperatureDatasetFinal.csv")
MRGDS <- merge(POPDS,TMPDS)
GDPDS <- read_csv("GDPDataset.csv")
GDPDS <- select(GDPDS, "Country Name", "Country Code", "Year","GDP")
#Rename the country code to country
colnames(GDPDS)[2]<-"Country"
colnames(GDPDS)[1]<-"CountryName"
MRGDS <- merge(MRGDS,GDPDS)

CO2DS <- read_csv("Co2EmissionsDataSet.csv")
CO2DS <- select(CO2DS, "Country", "Year","Emissions")
colnames(CO2DS)[1]<-"CountryName"
CO2DS$CountryName[CO2DS$CountryName == "United States of America"] <- "United States"
CO2DS$CountryName[CO2DS$CountryName == "South Korea"] <- "Korea, Rep."

MRGDS <- merge(MRGDS,CO2DS)
MRGDS <- MRGDS %>% filter((format(MRGDS$Year,format="%Y") <= 2012))

# Since all the datasets were ending in the year 2012, we considered the year 2012 for selecting something
# Filtered Dataset
FLTDS = MRGDS %>% filter((format(MRGDS$Year,format="%Y") == 2012))
FLTDS = sqldf('select Country from FLTDS WHERE GDP>1000000000000 OR Population > 100000000')
# In the year 2012, select countries with GDP over a trillion and Population over 100 million
# Now, consider data for only these countries from all the data sources
FNLDS <- merge(MRGDS, FLTDS, by = "Country")

DISDS <- read_csv("Deaths_by_Year_Country.csv");
DISDS <- select(DISDS, -c("country_name"))
FNDDS <- merge(x = FNLDS, y = DISDS, by = c("Year","Country"), all.x=TRUE)
#FNDDS[is.na(FNDDS)] <- 0

# Code to calculate the anamoly for the temperature and countries
by_cou <- FNDDS %>% group_by(Country) %>% summarise(mean_Temperature = mean(AverageTemperature,na.rm = TRUE))
l = c()
p = c()
for(i in 1:nrow(by_cou))
{ 
  print(by_cou$mean_Temperature[[i]])
  for( k in 1:(nrow(FNDDS))) {
    if(by_cou$Country[[i]] == FNDDS$Country[[k]]) {
      l[[k]] = by_cou$mean_Temperature[[i]] - FNDDS$AverageTemperature[[k]]
      p[[k]] = by_cou$mean_Temperature[[i]]
      
    }
  }  
}
FNDDS$Meantemperature = p
FNDDS$anamoly = l
write.csv(FNDDS,"MergedDataSet.csv", row.names = FALSE)