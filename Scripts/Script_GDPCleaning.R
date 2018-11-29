library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
print(gdpPercent)
gdp_data<- melt(gdpPercent, id=c("Country Name","Country Code","Indicator Name","Indicator Code"))
print(gdp_data)
write.csv(gdp_data,file="GDPDataSet.csv")




