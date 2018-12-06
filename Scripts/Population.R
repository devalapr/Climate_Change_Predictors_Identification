library(dplyr)
# define the filename
filename <- "population.csv"
# load the CSV file from the local directory
data <- read.csv(filename,header=TRUE, sep=",")
# set the column names in the dataset
head(data)
line_count=0
year=list()

#Copy the required data into df (excluding headers)
#the format is Country X Year
for (row in 1:nrow(data)){
  if (line_count == 0){
    year = row
    line_count = line_count + 1
  }
  else{
    df[nrow(df) + 1,] = row
    line_count = line_count + 1
  }
}
head(df)
#Make a new dataframe of the format Country,Year
df <- data.frame(Country=character(),
                 Year=integer(),
                 Population=integer())
for (i in 1:nrow(data1)){
  for(j in 1:ncol(year)){
    df[nrow(df) + 1,] = list(data[i,0],year[j],data[i,j])
    my_data <- data[i,0] + "," + year[j] + "," + data[i,j]
    write.csv(my_data, file = "output.csv")
  }
}

