library(magrittr)
library(dplyr)
library(ggplot2)

rawdata=read.csv("NDG.csv")

#summary(rawdata)
# Replace the missing values in the data frame with 0. For calculations
rawdata[is.na(rawdata)] <- 0
colnames(rawdata)[11] <- "Total.damage" # Renaming the column
View(rawdata)

# Group the accidents, injuries, homeless, total_affected, total_damaged by Year and Country
Grouped_death_data <- rawdata %>% 
  group_by(year,iso) %>%
  summarize(TotalDeaths = sum(Total.deaths), TotalAffected = sum(Total.affected), TotalDamage=sum(Total.damage))
View(Grouped_death_data)

### Time for reading NDT
Disaster_Type=read.csv("NDT.csv")
Disaster_Type[is.na(Disaster_Type)] <- 0
colnames(Disaster_Type)[11] <- "Total.damage" # Renaming the column
View(Disaster_Type)

## Disaster by year and type
Grouped_disaster_data <- Disaster_Type %>% 
  group_by(year,disaster.type) %>%
  summarize(TotalDeaths = sum(Total.deaths), TotalAffected = sum(Total.affected), TotalDamage=sum(Total.damage))
View(Grouped_disaster_data)

## Deaths by year
Deaths_byyear <- Disaster_Type %>% 
  group_by(year) %>%
  summarize(TotalDeaths = sum(Total.deaths), TotalAffected = sum(Total.affected), TotalDamage=sum(Total.damage))
View(Deaths_byyear)

## Disaster by country, year, type
DisType_ctry_grpd_dt <- DisType %>% 
  group_by(year,disaster.type,iso) %>%
  summarize(TotalDeaths = sum(Total.deaths), TotalAffected = sum(Total.affected), TotalDamage=sum(Total.damage))
View(DisType_ctry_grpd_dt) 


ggplot(data=Deaths_byyear, 
       mapping=aes(x = year, y= TotalDeaths))+geom_point()

ggplot(data=Deaths_byyear, 
       mapping=aes(x = year, y= TotalDeaths))+geom_line()
