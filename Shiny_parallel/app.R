library(shiny)
library(lattice)
load("AQ_delay.RData")
#AQ_delay is a subset with variables Month, DayOfWeek, AirTime, ArrDelay and DepDelay.

ui <- fluidPage(
  radioButtons(inputId = "month", label = "Choose a Month (of 2001)",
               choices = c("January" = 1,
                           "February" = 2,
                           "March" = 3,
                           "April" = 4,
                           "May" = 5,
                           "June" = 6,
                           "July" = 7,
                           "August" = 8,
                           "September" = 9,
                           "October" = 10),
               inline = 1),
  plotOutput(outputId = "parallelPlot")
)

server <- function(input, output) {
  output$parallelPlot <- renderPlot({
    parallelplot(AQ_delay[AQ_delay$Month == input$month, 3:5],
                 groups = AQ_delay$DayOfWeek,
                 auto.key = list(space = "right"),
                 main = "AQ Carrier flights in 2001")
  })
}

shinyApp(ui=ui, server = server)