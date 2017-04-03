library(plotly)
library(shiny)
# Subset smallest carrier, AQ:
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay

ui <-fluidPage(
  mainPanel(
    plotlyOutput("histogram"),
    plotlyOutput("scatterplot")
  ),
  verbatimTextOutput("selection")
)

server <- function(input, output, session) {
  output$histogram <- renderPlotly({
    ggplot(AQ) +
    geom_histogram(aes(x=Distance), binwidth = 50)
    ggplotly()
    ggplotly(source = "selectbin")
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "selectbin")
    if (length(s)==0) {
      "Click on a bin to identify flights on the scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "selectbin")
    print(s)
    if (length(s)) {
      # Subset AQ using selection
      ggplot() +
        geom_point(data= AQ, aes(x=AirTime, y=NetDelay), colour="#0000FF30") +
        geom_point(data = AQ[AQ$Distance==s$x, ], aes(x=AirTime, y=NetDelay), colour="#FF000030")
      ggplotly()
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui, server)

# Output from selecting two extreme bins:
#curveNumber pointNumber   x(km)   y/Count
#0           0              50      530
#curveNumber  pointNumber    x    y
#0            51            2600  360
# There are 51 bins (2600-50)/50 =51. (50km=binwidth)
# s$pointNumber is the bin #.
summary(AQ$Distance)
# Slight delay loading and after selection in displaying scatterplot.
# Worth doing below?
# Order AQ by Distance and create variable bin # by grouping by departure times.
AQ <- AQ[order(AQ$Distance),]
head(AQ)
tail(AQ)
sum(AQ$Distance>=2550)
# Above is 360.
