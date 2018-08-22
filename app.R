#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("C:/Users/tonyn/Downloads")

library(shiny)
library(leaflet)
library(dplyr)

df <- readRDS("./dataset.rds")



ui <- fluidPage(  
  titlePanel("Plotly"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotlyOutput("plot2"))))

server <- function(input, output) {
  
  output$plot2 <- renderPlotly({
    print(
      ggplotly(ggplot(df, aes()) + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey", color="lightgrey")+xlab("")+ylab("") + 
                 coord_fixed(1.3)+ 
                 geom_point(data = df, aes(x = Latitude, y = Longitude, text = paste(Country,":", Percentage2, "percentage of survey respondents")), color = "blue", size = respondents_merged$freq/800, alpha = 0.4)+xlab("")+ylab("")))
               
       
    
  })
}

shinyApp(ui, server)
   


# Define server logic required to draw a histogram
server <- function(input,output, session){
  
  data <- reactive({
    x <- df
  })
  
  output$mymap <- renderPlotly({

    world <- map_data("world") # we already did this, but we can do it again
    gg1<-ggplot(df, aes()) + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "grey", color="lightgrey")+xlab("")+ylab("") + 
      coord_fixed(1.3)
    
    
    
    p <- gg1 + 
      geom_point(data = df, aes(x = Latitude, y = Longitude, text = paste(Country,":", Percentage2, "percentage of survey respondents")), color = "blue", size = respondents_merged$freq/800, alpha = 0.4)+xlab("")+ylab("")
    
    n <- ggplotly(p)
    n
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

