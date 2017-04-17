library(shiny)
library(lattice)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")

# Arrange parallel plot from weak to strong correlation.
# See "Plotly_corr_heatmap.R"

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
    parallelplot(AQ[AQ$Month == input$month, 
                    c("Distance", "AirTime", "DepTime", "TaxiOut")],
      groups = AQ$DayOfWeek,
      auto.key = list(space = "right"),
      # col = "#0000FF30", #Use of transparent colour not helpful
      main = "AQ Carrier flights in 2001")
  })
}

shinyApp(ui=ui, server = server)

# Rotate axes arguement: scales = list(x = list(rot = 90))
