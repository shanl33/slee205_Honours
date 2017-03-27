# Select groups by lasso or dragging and outputs data of vars.
library(plotly)
library(shiny)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("brush")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    key <- row.names(AQ)
    p <- ggplot(AQ, aes(key=key)) +
      geom_point(aes(x=AirTime, y=NetDelay)) +
      labs(title="Flight delays for AQ Carrier in 2001", 
           x="Air time (mins))",
           y="Net delay (mins)")
    ggplotly(p) %>% layout(dragmode="select")
  })
  
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Click and on plot drag to select" else d
  })
}

shinyApp(ui, server)