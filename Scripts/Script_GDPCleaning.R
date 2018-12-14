library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(sqldf)
library(tibble)
#uncomment this code for generating the data set csv from the original data set
#print(gdpPercent)
#gdp_data<- melt(gdpPercent, id=c("Country Name","Country Code","Indicator Name","Indicator Code"))
#print(gdp_data)
#write.csv(gdp_data,file="GDPDataSet.csv")

#formatting the data set to rename the columns from the generated data set for column names variable and value to year and gdp
GDPDataSet <- read_csv("Data sets/GDPDataSet.csv")
GDPDataSet<-GDPDataSet[-c(1)]
colnames(GDPDataSet)[5]<-"Year"
colnames(GDPDataSet)[6]<-"GDP"

#Gdp data set with arregated gdp 
GDPDataSetGroup <- GDPDataSet %>% 
                    group_by(`Country Name`,`Country Code`,Year) %>%
                      summarize(GDP = sum(GDP))
#Getting the list of countries with available number of gdp less than 20 years
Gdp_OmitCountryList=sqldf('select "Country Name","Country Code",count(year) as DataCount from GDPDataSetGroup where GDP!="NA" group by "Country Name"')
Gdp_OmitCountryList=as.data.frame(Gdp_OmitCountryList)
#filtering the list to get only country codes
Gdp_OmitCountryList = Gdp_OmitCountryList %>% 
  filter(Gdp_OmitCountryList[,3] <20) %>% select(`Country Code`)
#creating an array equal to the length of the gdp_omitcountrylist for running it as a for loop for filtering  purpose
GdpOmitListCount<-array(1:NROW(Gdp_OmitCountryList))
for (rowIndex in GdpOmitListCount) {
cab<-Gdp_OmitCountryList[rowIndex,1]
GDPDataSet=GDPDataSet %>% filter(GDPDataSet[,4]!=cab)

}
View(GDPDataSet)


