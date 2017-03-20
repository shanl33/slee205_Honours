library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      numericInput("obs", "Obs", 10, min = 0, max = 100),
      numericInput("p2", "P(mu=2)", 50, min = 0, max = 100),
    width = 2)
  )
# Show a plot of the generated distribution
  mainPanel(
      #plotOutput("distPlot"),
      plotOutput("normPlot")
  )

# Server logic
server <- function(input, output) {
  #output$distPlot <- renderPlot({
   # hist(rnorm(input$obs))
  #})
  
  output$poisPlot <- renderPlot({
    hist(rnorm(input$p2))
  })
}

# Complete app with UI and server components
shinyApp(ui, server)

#splitLayout(cellWidths = c("25%", "75%"),