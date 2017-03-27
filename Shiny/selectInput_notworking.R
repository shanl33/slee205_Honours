library(shiny)
library(ggplot2)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")

featurelist <- colnames(AQ)[1:3]
AQ$Month <- as.factor(AQ$Month)
AQ$DayOfMonth <- as.factor(AQ$DayOfMonth)
AQ$DayOfWeek <- as.factor(AQ$DayOfWeek)

ui <- fluidPage(
  selectInput(inputId = "chooseFactor", label = "Choose a Factor",
               choices = featurelist),
  plotOutput(outputId = "scatterplot")
)

server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(AQ) +
      geom_point(aes(x=AirTime, y=NetDelay, color=input$chooseFactor)) +
      labs(title="Flight delays for AQ Carrier in 2001", 
           x="Air time (mins)",
           y="Net delay (mins)")
  })
}

shinyApp(ui=ui, server = server)