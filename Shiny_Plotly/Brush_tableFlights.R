# Brush to display info for other variables in a table.
library(plotly)
library(shiny)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("table")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    ggplot(AQ) +
      geom_point(aes(x=AirTime, y=NetDelay)) +
      labs(title="Flight delays for AQ Carrier in 2001", 
           x="Air time (mins))",
           y="Net delay (mins)")
    ggplotly(source = "brush")
  })
  
  output$table <- renderPrint({
    s <- event_data("plotly_selected", source = "brush")
    if (length(s)) {
      AQ[s$pointNumber+1, c("AirTime", "NetDelay", "Distance", "Origin", "Dest", "DayOfWeek", "Month")]
    } else {
      "Click and on plot drag to select" 
    }
  })
}

shinyApp(ui, server)