# Brush to display info for other variables in a table.
library(plotly)
library(shiny)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("table")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    ggplot(mtcars) +
      geom_point(aes(x=hp, y=qsec))
    ggplotly(source = "brush")
  })
  
  output$table <- renderPrint({
    s <- event_data("plotly_selected", source = "brush")
    if (length(s)) {
      mtcars[s$pointNumber+1, c("hp", "qsec", "mpg", "cyl")]
    } else {
      "Make a selection on the scatterplot" 
    }
  })
}

shinyApp(ui, server)