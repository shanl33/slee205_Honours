library(plotly)
library(shiny)
# Subset smallest carrier, AQ:
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")

ui <-fluidPage(
  mainPanel(
    plotlyOutput("AirTime"),
    plotlyOutput("scatterplot")
  ),
  verbatimTextOutput("selection")
)

server <- function(input, output, session) {
  output$AirTime <- renderPlotly({
    ggplot(AQ) +
      geom_histogram(aes(x=SchedDepTime, weight=Cancelled), binwidth = 1800) +
      labs(x="Air time (mins)") %>%
    ggplotly(source = "Air time")
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    if (length(s)==0) {
      "Click on a bar to identify flights on the scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "SchedDep")
    if (length(s)) {
      
    }
  })
}
# Graph 1: Frequence of scheduled departure time


# Graph 2: Net Delay vs Air time 
# NetDelay (mins) measures if a flight 'made up' for its departure delay.
# NetDelay>0 means flight was delayed further, NetDelay<0 flight overcompensated for delay.
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay
AQ$SchedDepTime <- strptime(AQ$CRSDepTime, format = "%H%M")
ggplot(AQ, aes(key=key)) +
  geom_point(aes(x=SchedDepTime, y=NetDelay, color=AirTime)) +
  labs(title="Flight delays for AQ Carrier in 2001", 
       x="Scheduled departure time (hh:mm)",
       y="Net delay (mins)")
ggplotly()
