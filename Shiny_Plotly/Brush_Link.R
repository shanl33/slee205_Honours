library(plotly)
library(shiny)

ui <- fluidPage(
  mainPanel(
    plotlyOutput("scatterplot1"),
    plotlyOutput("scatterplot2")
  )
)

server <- function(input, output, session) {
  # Brush on scatterplot1 ("sp1"). Note the "source" arguement in ggplotly().
  output$scatterplot1 <- renderPlotly({
    ggplot(mtcars) +
      geom_point(aes(x=hp, y=qsec))
    ggplotly(source="sp1") %>% layout(dragmode = "select")
  })
  # Scatterplot2 will display brushed subset from sp1.
  output$scatterplot2 <- renderPlotly({
    # event_data() listens for the "selected" event in the "source".
    s <- event_data("plotly_selected", source = "sp1")
    print(s) 
    # Output from the brushing 'event'(x, y, curve#, point#=(obs#-1) in mtcars)
    # Diff of 1 between point# and obs# index is common when using diff languages
    if (length(s)) {
      # Subset mtcars using (point#+1)
      ggplot() +
        geom_point(data=mtcars, aes(x=mpg, y=disp)) +
        geom_point(data = mtcars[s$pointNumber+1, ], aes(x=mpg, y=disp), colour="red")
    } else {
        ggplot(mtcars) +
        geom_point(aes(x=mpg, y=disp))
      ggplotly()
    }
  })
}

shinyApp(ui, server)
