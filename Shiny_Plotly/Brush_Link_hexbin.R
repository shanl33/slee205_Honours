# The eventData picks up on the click but records only 'curveNumber'(=rank of frequency).
# All the bins with frequency = 1 has curveNumber=0.
# The highest frequency (count=11302) bin has curveNumber=75.
library(plotly)
library(shiny)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay

ui <- fluidPage(
  mainPanel(
    plotlyOutput("hexbin"),
    plotlyOutput("scatterplot2")
  )
)

server <- function(input, output, session) {
  # Brush on hexbin ("sp1"). Note the "source" arguement in ggplotly().
  output$hexbin <- renderPlotly({
    ggplot(AQ) +
      geom_hex(aes(x=AirTime, y=NetDelay), binwidth = c(10, 3))
    ggplotly(source="sp1")
  })
  # Scatterplot2 will display brushed subset from sp1.
  output$scatterplot2 <- renderPlotly({
    # "plotly_selected" event returns an empty list "list()".
    # "plotly_click" event returns only the 'curveNumber'=rank of frequency (lowest=0)
    s <- event_data("plotly_click", source = "sp1")
    print(s) 
    if (length(s)) {
      # Subset AQ using (point#+1)
      ggplot() +
        geom_point(data= AQ, aes(x=CRSDepTime, y=CRSArrTime), colour="#0000FF30") +
        #NOT subsetted properly.
        geom_point(data = AQ[s$pointNumber+1, ], aes(x=CRSDepTime, y=CRSArrTime), colour="#FF000030")
      ggplotly()
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui, server)