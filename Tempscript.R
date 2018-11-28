library(readr)
getwd()
df = data.frame(GlobalLandTemperaturesByCountry)
library(dplyr)

library(tibble)
df = cbind(df,df[,2]-df[,3],df[,2]+df[,3])
df = cbind(df,format(df[,1],format="%Y"))
df = df %>% drop_na()
list1 = 1:length(df[[1]])
df1= df[-list1,]
df1[] <- lapply(df1[7], as.double)
#continents = c("Åland","Africa","Antarctica","North America","South America","Asia","Africa","Oceania","Antarctica","Europe")
#write.csv(df,"Temper1.csv")

df = df %>% 
  filter((format(df[,1],format="%Y") >= 1900))
df = df %>%
  filter(df[,4] != "Åland")
df = df %>%
  filter(df[,4] != "Africa")
df = df %>%
  filter(df[,4] != "Antarctica")
df = df %>%
  filter(df[,4] != "North America")
df = df %>%
  filter(df[,4] != "South America")
df = df %>%
  filter(df[,4] != "Asia")
df = df %>%
  filter(df[,4] != "Africa")
df = df %>%
  filter(df[,4] != "Oceania")
df = df %>%
  filter(df[,4] != "Europe")


df %>% as_tibble() %>% mutate(AverageTemperature=format(df[,4],format="%Y"))
write.csv(df,"temperature.csv")
df1 = data.frame(temperature)

df1 <- df1 %>%
  group_by(Country,Year) %>%
  summarise(AverageTemperature = mean(AverageTemperature),MinTemperature = mean(MinTemperature),MaxTemperature = mean(MaxTemperature))
head(df1)
write.csv(df1,"TemperatureDataset.csv")
