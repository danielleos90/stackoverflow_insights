source("D:/Users/454594/Documents/GitHub/stackoverflow_insights/map.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Stackoverflow Insights"),
  
  sidebarLayout(sidebarPanel(
    helpText("Developer Profile")),
    mainPanel(
        plotOutput("map"),
        plotOutput("map2")
      )
    )
  )

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    percent_map(counties$white, "darkgreen", "% White")
  })
  output$map2 <- renderPlot({
    percent_map(counties$white, "blue", "% White")
  })
  
}

# Run app ----
shinyApp(ui, server)