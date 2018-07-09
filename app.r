library(shiny)

source("/Users/paulkelly/Dropbox/lecturing/GroupProject/stackoverflow_insights")

# User interface ----
ui <- fluidPage(
  titlePanel("Stackoverflow Insights"),
  sidebarLayout(
    sidebarPanel(
        tags$ul(
          tags$li(tags$a(href="#map1", "Geography")), 
          tags$li(tags$a(href="#map2", "Developer Roles")), 
          tags$li(tags$a(href="#map3", "Experience"))
        )
      ),
    mainPanel(
        plotOutput("map1"),
        plotOutput("map2"),
        plotOutput("map3")
      )
    )
  )

# Server logic ----
server <- function(input, output) {
  output$map1 <- renderPlot({
    percent_map(counties$white, "darkgreen", "% White")
  })
  output$map2 <- renderPlot({
    percent_map(counties$white, "blue", "% White")
  })
  output$map3 <- renderPlot({
    percent_map(counties$white, "red", "% White")
  })
  
}

# Run app ----
shinyApp(ui = htmlTemplate("www/index.html"), server)
