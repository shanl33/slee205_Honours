library(plotly)
library(shiny)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay

ui <- fluidPage(
  mainPanel(
    plotlyOutput("scatterplot1"),
    plotlyOutput("scatterplot2")
  )
)

server <- function(input, output, session) {
  # Brush on scatterplot1 ("sp1"). Note the "source" arguement in ggplotly().
  output$scatterplot1 <- renderPlotly({
    ggplot(AQ) +
      geom_point(aes(x=AirTime, y=NetDelay), colour="#0000FF30")
    ggplotly(source="sp1") %>% layout(dragmode="select")
  })
  # Scatterplot2 will display brushed subset from sp1.
  output$scatterplot2 <- renderPlotly({
    # event_data() listens for the "selected" event in the "source".
    s <- event_data("plotly_selected", source = "sp1")
    print(s) 
    # Output from the brushing 'event'(x, y, curve#, point#=(obs#-1) in mtcars)
    # Diff of 1 between point# and obs# index is common when using diff languages
    if (length(s)) {
      # Subset AQ using (point#+1)
      ggplot() +
        geom_point(data= AQ, aes(x=CRSDepTime, y=CRSArrTime), colour="#0000FF30") +
        geom_point(data = AQ[s$pointNumber+1, ], aes(x=CRSDepTime, y=CRSArrTime), colour="#FF000030")
      ggplotly()
    } else {
        plotly_empty()
    }
  })
}

shinyApp(ui, server)