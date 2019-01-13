library(readr)
library(dplyr)
library(tidyr)
library(sqldf)

# Read population and Temperature Datasets and merge them
POPDS <- read_csv("PopulationDataset_03_Preprocessed.csv")
TMPDS <- read_csv("TemperatureDatasetFinal.csv")
MRGDS <- merge(POPDS,TMPDS) # Merging the Temperature and Population sets

# Now, include GDPDataset as well to include
GDPDS <- read_csv("GDPDataset.csv")
GDPDS <- select(GDPDS, "Country Name", "Country Code", "Year","GDP")
#Rename the country code to country for easy merging
colnames(GDPDS)[2]<-"Country"
colnames(GDPDS)[1]<-"CountryName"
GDPDS = GDPDS %>% drop_na()
MRGDS <- merge(MRGDS,GDPDS)

# Now, include CO2 Emissions Dataset
CO2DS <- read_csv("Co2EmissionsDataSet.csv")
CO2DS <- select(CO2DS, "Country", "Year","Emissions")
colnames(CO2DS)[1]<-"CountryName"

# CO2 dataset does not have country code and the country name is different to the existing frame. 
# So, manually changed the country names so that they match (needed for merging)
CO2DS$CountryName[CO2DS$CountryName == "United States of America"] <- "United States"
CO2DS$CountryName[CO2DS$CountryName == "South Korea"] <- "Korea, Rep."
MRGDS <- merge(MRGDS,CO2DS)

# Trimming to take the data only until year 2012. So, starting from 1960 to 2012
MRGDS <- MRGDS %>% filter((format(MRGDS$Year,format="%Y") <= 2012))

# Since all the datasets were ending in the year 2012, we considered the year 2012 for selecting something
# Filtered Dataset
FLTDS = MRGDS %>% filter((format(MRGDS$Year,format="%Y") == 2012))
FLTDS = sqldf('select Country from FLTDS WHERE GDP>1000000000000 OR Population > 100000000')
# In the year 2012, select countries with GDP over a trillion and Population over 100 million
# Now, consider data for only these countries from all the data sources

# Consider only those countries that are returned from the above query
FNLDS <- merge(MRGDS, FLTDS, by = "Country")

# To this, add the deaths (this is added at the end because of limited data)
DISDS <- read_csv("Deaths_by_Year_Country.csv");
DISDS <- select(DISDS, -c("country_name"))
DISDS[is.na(DISDS)] <- 0
FNDDS <- merge(x = FNLDS, y = DISDS, by = c("Year","Country"), all.x=TRUE) # LEFT JOIN
FNDDS[is.na(FNDDS)] <- 0

# Code to calculate the anomaly for the temperature and countries
# IDEA was to check if we can normalize all the factors and then use them for building a model etc. 
by_cou <- FNDDS %>% group_by(Country) %>% 
          summarise(
                  mean_Temperature = mean(AverageTemperature,na.rm = TRUE),
                  mean_GDP = mean(GDP,na.rm = TRUE),
                  mean_CO2 = mean(Emissions,na.rm = TRUE),
                  mean_Population = mean(Population, na.rm=TRUE)
                  )
mean_temp = c()
an_temp = c() # Temperature. this was P before.
mean_gdp = c()
an_gdp = c() # GDP
mean_co2 = c()
an_co2 = c() #CO2
mean_pop = c()
an_pop = c()

for(i in 1:nrow(by_cou))
{ 
  #print(by_cou$mean_Temperature[[i]])
  for( k in 1:(nrow(FNDDS))) {
    if(by_cou$Country[[i]] == FNDDS$Country[[k]]) {
      an_temp[[k]] = FNDDS$AverageTemperature[[k]] - by_cou$mean_Temperature[[i]]
      mean_temp[[k]] = by_cou$mean_Temperature[[i]]
      an_gdp[[k]] = FNDDS$GDP[[k]] - by_cou$mean_GDP[[i]] 
      mean_gdp[[k]] = by_cou$mean_GDP[[i]]
      an_co2[[k]] = FNDDS$Emissions[[k]] - by_cou$mean_CO2[[i]]
      mean_co2[[k]] = by_cou$mean_CO2[[i]]
      an_pop[[k]] = FNDDS$Population[[k]] - by_cou$mean_Population[[i]]
      mean_pop[[k]] = by_cou$mean_Population[[i]]
    }
  }  
}
FNDDS$Meantemperature = mean_temp
FNDDS$AN_Temp = an_temp
FNDDS$MeanGDP = mean_gdp
FNDDS$AN_GDP = an_gdp
FNDDS$MeanCO2 = mean_co2
FNDDS$AN_CO2 = an_co2
FNDDS$MeanPop = mean_pop
FNDDS$AN_POP = an_pop

write.csv(FNDDS,"MergedDataSet.csv", row.names = FALSE)