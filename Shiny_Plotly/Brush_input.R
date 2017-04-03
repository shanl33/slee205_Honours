# Brush to select working but subFreq not working.
library(plotly)
library(shiny)
library(dplyr)
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")

AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay
AQ$DayOfWeek <- as.integer(AQ$DayOfWeek)

ui <- fluidPage(
  plotlyOutput("scatterplot"),
  plotlyOutput("subFreq")
)

server <- function(input, output) {
  output$scatterplot <- renderPlotly({
    plot_ly(data=AQ, x=AQ$DayofMonth, y=AQ$NetDelay, 
            color=AQ$DayOfWeek, source = "subset", mode = "markers", mode = "scatter") %>%
      layout(xaxis=list(title="Day of Month"), yaxis=list(title="Net Delay"), dragmode="select")
  })
  
  output$subFreq <- renderPlotly({
    # Get subset from brushing
    brushed <- event_data("plotly_selected", 
                          source = "subset")
    print(brushed)
    if(is.null(brushed)==T) return(NULL)
    # Creates a subset based on the lasso selection.
    # Need the two intial subsets to capture 'ends' of lasso.
    wkend <- subset(AQ, AQ$DayOfWeek<=5)[subset(brushed, curveNumber==0)$pointNumber + 1,]
    wkday <- subset(AQ, AQ$DayOfWeek>5)[subset(brushed, curveNumber==1)$pointNumber + 1,]
    # Combine to complete lasso subset.
    plot.subset <- rbind(wkend, wkday)
    # Summarise
    plot.sum <- plot.subset %>%
      group_by(DayOfWeek) %>%
      summarize(Mean = mean(NetDelay))
    # Assign to parent frame
    plot.sum <<- plot.sum
    # Plot with mean net delay.  Not sure if that is really being calculated (?)
    # Colour of bars cannot be set to a "new" variable, not previously used (ie. Month).
    plot_ly(plot.sum, x = plot.sum$DayOfWeek, y=plot.sum$Mean, 
            type="bar", source = "select", color = plot.sum$DayOfWeek) %>%
      layout(xaxis=list(title="Day of Week"), yaxis=list(title="Mean net delay (mins)"))
  })
}

shinyApp(ui, server)