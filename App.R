library(shiny)
library(ggplot2)
library(dplyr)
dat = data.frame(TemperatureDatasetFinal)
gdp = data.frame(GDPDataSet)
popu = data.frame(PopulationDataset)

ui = fluidPage(
  sliderInput(inputId = "num",
              label = "choose a num",
              value = 1990,min = 1960,max = 2013,dragRange = TRUE),
  textInput(inputId = "countr",label = "choose country code",value = "IND"),
  actionButton(inputId = "clicks",label="find"),
  plotOutput("country"),
  plotOutput(("GDP")),
  plotOutput(("popu"))
)

server  = function(input,output) {
  cou = reactive(input$countr)
  df1 =  reactive(dat %>% filter(dat[,1]==cou()))
  df = reactive(df1()%>% filter(df1()[,2] > input$num))
  
  pop = reactive(popu %>% filter(popu[,1]==cou()))
  popu1 = reactive(pop()%>% filter(pop()[,2] > input$num))
  
  #observe(input$clicks,{head(df)})
  
  gdp_df1 = reactive(gdp %>% filter(gdp[,3]==cou()))
  gdp_df = reactive(gdp_df1() %>% filter(gdp_df1()[,6] > input$num))
  
  plotted = reactive(ggplot(df(),mapping = aes(x = Year,y =AverageTemperature))+ geom_line())
  plotted_gdp = reactive(ggplot(gdp_df(),mapping = aes(x = variable,y =value))+ geom_line())
  plotted_popu = reactive(ggplot(popu1(),mapping = aes(x = Year,y =Population))+ geom_line())
  

  
  
  reactive_update = eventReactive(input$clicks,{plotted()})
  reactive_update1 = eventReactive(input$clicks,{plotted_gdp()})
  reactive_update2 = eventReactive(input$clicks,{plotted_popu()})
  output$country = renderPlot(reactive_update())
  output$GDP = renderPlot(reactive_update1())
  output$popu = renderPlot(reactive_update2())
  
}

shinyApp(ui  = ui, server = server)


x = ggplot(df,mapping = aes(x = Year,y =AverageTemperature))+ geom_line()
x

head(popu)

#gdp_df = (gdp %>% filter(gdp[,2]=="IND"))
#ggplot(gdp_df,mapping = aes(x = variable,y =value))+ geom_line()