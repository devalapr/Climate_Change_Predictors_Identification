library(dplyr)
library(readr)
library(reshape2)

df = data.frame(FinalDataSet)
head(df)
by_cou <- df %>% group_by(Country) %>% summarise(mean_Temperature = mean(AverageTemperature,na.rm = TRUE))

#l = list()
#p = list()
l = c()
p = c()
for(i in 1:nrow(by_cou))
{ 
  print(by_cou$mean_Temperature[[i]])
  for( k in 1:(nrow(df))) {
    if(by_cou$Country[[i]] == df$Country[[k]]) {
      l[[k]] = by_cou$mean_Temperature[[i]] - df$AverageTemperature[[k]]
      p[[k]] = by_cou$mean_Temperature[[i]]
      
    }
  }  
}

df$Meantemperature = p
df$anamoly = l
df
write.csv(df,"fdataset2.csv")
df1 <- data.frame(df)


