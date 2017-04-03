library(plotly)
library(shiny)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay
AQ$DayOfWeek <- as.factor(AQ$DayOfWeek)

ui <- fluidPage(
  mainPanel(
    plotlyOutput("barchart"),
    plotlyOutput("scatterplot2")
  )
)

server <- function(input, output, session) {
  # Brush on scatterplot1 ("sp1"). Note the "source" arguement in ggplotly().
  output$barchart <- renderPlotly({
    ggplot(AQ) +
      geom_bar(aes(x=DayOfWeek))
    ggplotly(source="sp1")
  })
  # Scatterplot2 will display brushed subset from sp1.
  output$scatterplot2 <- renderPlotly({
    # event_data() listens for the "selected" event in the "source".
    s <- event_data("plotly_click", source = "sp1")
    print(s) 
    # Output from the brushing 'event'(x, y, curve#, point#=(obs#-1) in mtcars)
    # Diff of 1 between point# and obs# index is common when using diff languages
    if (length(s)) {
      # Subset AQ using selection
      ggplot() +
        geom_point(data= AQ, aes(x=AirTime, y=NetDelay), colour="#0000FF30") +
        geom_point(data = AQ[AQ$DayOfWeek==s$x, ], aes(x=AirTime, y=NetDelay), colour="#FF000030")
      ggplotly()
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui, server)