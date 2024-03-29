library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(shinythemes)

#FinalDatasetFinal = read.csv("Data set/FinalDatasetFinal.csv")

FinalDataset = data.frame(FinaldatasetFinal)
ui = fluidPage(
  shinythemes::themeSelector(),
  
#  theme =shinytheme("united"),
  navbarPage("WHO Cares"),
  sidebarLayout(
    
    sidebarPanel (
      selectInput(inputId = "country", label = strong("Country"),
                  choices = unique(FinalDataset$Country.Name),
                  selected = "Germany"),
      
      actionButton(inputId = "clicks",label="find",class="btn-primary")
          ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput("country"),
                 plotOutput(("GDP")),plotOutput(("emission")),plotOutput("corr"),
                 plotOutput(("popu"))),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("correlation Table", tableOutput("table"))
      )
    )   
  )
)

server  = function(input,output) {
  cou = reactive(input$country)
  df1 =  reactive(FinalDataset %>% filter(FinalDataset[,5]==cou()))
  #df1 = reactive(FinalDataset %>% filter(FinalDataset$Year > Input$slider1[1]))
  #df1 = reactive(FinalDataset %>% filter(FinalDataset$Year < Input$slider1[1]))
  

  
  plotted = reactive(ggplot(df1(),mapping = aes(x = Year,y =anamoly))+ geom_line())
  plotted_gdp = reactive(ggplot(df1(),mapping = aes(x = Year,y=GDP))+ geom_line())
  plotted_popu = reactive(ggplot(df1(),mapping = aes(x = Year,y =Population,color=Year))+ geom_line())
  plotted_emi = reactive(ggplot(df1(),mapping = aes(x=Year,y=Emissions))+geom_line())
  df2 <- reactive(select(df1(),-c("Year","Country.Name","Country","TotalAffected","TotalDamage","MeanTemperature")))
  res <- reactive(cor(df2()))
  reactive(round(res(), 2))
  plotted_corr = reactive(corrplot(res(), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45))
  
  
    
  
  output$table = renderTable(((df2())))
  output$summary = renderPrint(summary(df1()))
  
  
  
  reactive_update = eventReactive(input$clicks,{plotted()})
  reactive_update1 = eventReactive(input$clicks,{plotted_gdp()})
  reactive_update2 = eventReactive(input$clicks,{plotted_popu()})
  reactive_update3 = eventReactive(input$clicks,{plotted_emi()})
  reactive_update4 = eventReactive(input$clicks,{plotted_corr()})
  
  
  output$country = renderPlot(reactive_update())
  output$GDP = renderPlot(reactive_update1())
  output$popu = renderPlot(reactive_update2())
  output$emission = renderPlot(reactive_update3())
  output$corr = renderPlot((reactive_update4()))
  
}

shinyApp(ui  = ui, server = server)
