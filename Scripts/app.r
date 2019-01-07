library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
setwd("C:/Users/Karthik/Documents/data-science-with-r/Data sets")

FinalDatasetFinal = read.csv("finaldatasetfinal.csv")

FinalDataset = data.frame(FinalDatasetFinal)
ui = fluidPage(
  selectInput(inputId = "country", label = strong("Country"),
              choices = unique(FinalDataset$Country.Name),
              selected = "Germany"),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot"),plotOutput("country"),
               plotOutput(("GDP")),
               plotOutput(("popu"))),
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("correlation Table", tableOutput("table"))
    )
  ),
  #textInput(inputId = "countr",label = "choose country code",value = "IND"),
  actionButton(inputId = "clicks",label="find")
  
)

server  = function(input,output) {
  cou = reactive(input$country)
  df1 =  reactive(FinalDataset %>% filter(FinalDataset[,5]==cou()))
  #df1 = reactive(FinalDataset %>% filter(FinalDataset$Year > Input$slider1[1]))
  #df1 = reactive(FinalDataset %>% filter(FinalDataset$Year < Input$slider1[1]))
  

  
  plotted = reactive(ggplot(df1(),mapping = aes(x = Year,y =anamoly))+ geom_line())
  plotted_gdp = reactive(ggplot(df1(),mapping = aes(x = Year,y=GDP))+ geom_line())
  plotted_popu = reactive(ggplot(df1(),mapping = aes(x = Year,y =Population,color=Year))+ geom_line())
  output$table = renderTable(((df1())))
  output$summary = renderPrint(summary(df1()))
  ggplot(data = df) +
    geom_histogram(mapping = aes(x = anamoly), binwidth = 0.1)
  
  
  
  reactive_update = eventReactive(input$clicks,{plotted()})
  reactive_update1 = eventReactive(input$clicks,{plotted_gdp()})
  reactive_update2 = eventReactive(input$clicks,{plotted_popu()})
  output$country = renderPlot(reactive_update())
  output$GDP = renderPlot(reactive_update1())
  output$popu = renderPlot(reactive_update2())
  
}

shinyApp(ui  = ui, server = server)

FinalDataset = FinalDataset %>% filter(FinalDataset$Country.Name=="India")
head(unique(FinalDataset$Year))
ggplot(data = FinalDataset) +geom_histogram(mapping = aes(x = anamoly), binwidth = 0.1)

       