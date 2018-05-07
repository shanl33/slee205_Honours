# No interaction occuring.
devtools::install_github("rstudio/crosstalk")
#devtools::install_github("ropensci/plotly")
library(crosstalk)
library(shiny)
library(plotly)
library(htmltools)

# Prepare mtcars
mtcars$cyl <- factor(mtcars$cyl)
# Create the variable 'rownames' to use as the 'key' for ShareData.
mtcars$rowname <- rownames(mtcars)

ui <- fluidPage(
  fillRow(height = 500,
    plotlyOutput("p1"),
    plotlyOutput("p2")
  )
)

server <- function(input, output, session) {
  sd <- SharedData$new(mtcars, key=mtcars$rowname, group="A")
  df <- sd$data(withSelection=TRUE)
  print(df$selected_)
  #Prints NA for all despite selection.
  #plot_ly(key=mtcars$rowname, set="A") with ggplotly() makes no difference
  # Interaction is only one way (p2 onto p1). p1 gets frozen.
  output$p1 <- renderPlotly({
    ggplot(sd) +
      geom_point(aes(x=wt, y=mpg, colour=cyl))
    ggplotly() %>% layout(dragmode="select")
  })
 
  output$p2 <- renderPlotly({
    ggplot(sd) +
      geom_point(aes(x=wt, y=disp, colour=cyl))
    ggplotly() %>% layout(dragmode="select")
  })
}

shinyApp(ui, server)

# Modified from https://beta.rstudioconnect.com/jcheng/shiny-crosstalk/
# But the code in the link doesn't work (?) 'Object wt' cannot be found.

# Example of Crosstalk from ggplotly() help
d <- SharedData$new(mtcars)
subplot(
  qplot(data = d, x = mpg, y = wt),
  qplot(data = d, x = mpg, y = vs)
)
 