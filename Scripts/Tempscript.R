library(readr)
getwd()
library(tidyr)
library(ggplot2)
df = data.frame(TemperatureDatasetFinal)
library(dplyr)
library(seriation)
install.packages("seriation")

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


df = data.frame(TemperatureDatasetFinal)
df = df %>% drop_na()

df = df %>% 
  filter((format(df[,2],format="%Y") >1990)) 
df = df %>% filter(df[,1] =="IND")
iris_dm <- dist(df)
dissplot(iris_dm)

length(df[,3])
library(seriation)
iris_scaled <- iris %>% 
  select(-Species) %>% 
  scale()
iris_dm <- dist(iris_scaled)
df = head(df)
gather(df[,3], key, value) %>%
  ggplot(aes(x = factor(1), y = value)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ key, ncol = 1, scales = "free_x") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank())
df[,1] == "IND"

install.packages("kohonen")
library(kohonen)
census_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
set.seed(123)
som_matrix <- scale(df[,-1])

som_model <- som(X = som_matrix,
                 grid = census_grid,
                 rlen = 500,
                 alpha = c(0.05, 0.01))
plot(som_model, type = "counts", palette.name = viridisLite::viridis)
plot(som_model, type = "dist.neighbours", palette.name = viridisLite::viridis)

pal_variables <- function(n) RColorBrewer::brewer.pal(n, "Dark2")
# Plot fan diagram
plot(som_model, type = "codes", palette.name = pal_variables, main = "")
