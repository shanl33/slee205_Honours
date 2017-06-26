library(shiny)
library(GGally)
library(plotly)
tdf2013 <- read.delim("http://www.theusrus.de/Blog-files/TDF2013.txt")
# Order by finishing time, for scatterplot ("plot")
tdf2013$rider <- factor(tdf2013$rider, 
                        levels = tdf2013$rider[order(tdf2013$T21, decreasing = T)])
# Cannot brush on pcp: Returns null list

ui <- fluidPage(
  mainPanel(
      plotlyOutput("plot"),
      plotlyOutput("pcp"),
      sliderInput("alpha", "Alpha color scale", min=0, max=1, value = 0.5)
    )
  )

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    t <- event_data("plotly_click", source = "p2")
    print(t)
    ggplot(tdf2013, aes(x=T21, y=rider)) +
      geom_point(na.rm = T) +
      theme(axis.text.y = element_blank())
    ggplotly(source = "p") %>% layout(dragmode = "select")
  })
  
  output$pcp <- renderPlotly({
    s <- event_data("plotly_selected", source = "p")
    print(s)
    # Order by "brushed" grouping so that it gets drawn last
    if (length(s)) {
      tdf2013$brushed <- rep("0", length(tdf2013$rider))
      for (i in 1:length(s$x)){
        tdf2013$brushed[s$pointNumber[i]+1]="1"
      }
      #print(tail(tdf2013[order(tdf2013$brushed),]))
      ggparcoord(data = tdf2013, columns = 8:28, scale = "center", 
                 scaleSummary = "median", alphaLines = input$alpha,
                 groupColumn = "brushed", showPoints = TRUE) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("black", "red"))
      ggplotly() 
    } else {
      ggparcoord(data = tdf2013, columns = 8:28, scale = "center", 
                 scaleSummary = "median", alphaLines = input$alpha)
      ggplotly(source = "p2") %>% layout(dragmode = "select") 
    }
  })
}
shinyApp(ui, server)
