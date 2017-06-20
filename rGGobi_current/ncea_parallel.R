library(shiny)
library(GGally)
library(plotly)
load("/Users/shanlee/Desktop/Project/slee205_Honours/Datasets/achieved.csv")

achieved$Decile <- factor(achieved$Decile)

ui <- fluidPage(
  mainPanel(
    plotlyOutput("plot"),
    plotlyOutput("pcp"),
    sliderInput("alpha", "Alpha color scale", min=0, max=1, value = 0.5)
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    ggplot(achieved) +
      geom_bar(aes(x=Decile))
    ggplotly(source = "p")
  })
  
  output$pcp <- renderPlotly({
    s <- event_data("plotly_click", source = "p")
    print(s)
    # Order by "brushed" grouping so that it gets drawn last
    if (length(s)) {
      achieved$brushed <- rep("0", length(achieved$School))
      for (i in 1:length(achieved$brushed)){
        if (achieved$Decile[i]==(s$x-1)) {
          achieved$brushed[i] = "1"
        }
      }
      ggparcoord(data = achieved, columns = 2:5, scale = "uniminmax", groupColumn = "brushed",
                alphaLines = input$alpha, showPoints = T) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("blue", "red"))
      ggplotly() 
    } else {
      # ggparcoord(data = achieved, columns = 2:5, scale = "center", scaleSummary = "median",
      #alphaLines = input$alpha, showPoints = T)
      ggparcoord(data = achieved, columns = 2:5, scale = "uniminmax", 
                 alphaLines = input$alpha, showPoints = T)
      ggplotly()
    }
  })
}
shinyApp(ui, server)
