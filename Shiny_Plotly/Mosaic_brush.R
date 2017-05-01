install.packages("productplots")
library(productplots)
library(ggplot2)
library(plotly)
library(shiny)

data("happy")
# hap and mar not working.

ui <- fluidPage(
  mainPanel(
    plotlyOutput("mosaic"),
    plotlyOutput("scatter")
  )
)

server <- function(input, output, session) {
  output$mosaic <- renderPlotly({
    mosaic2_coords <- prodcalc(happy, ~ marital + happy, 
                               c("vspine", "hspine"), na.rm = TRUE)
    # level = 1 or 2 since 2 vars involved. Need to plot only level 2
    p <- ggplot(mosaic2_coords[mosaic2_coords$level==2,]) +
      geom_rect(aes(xmin=l, xmax=r, ymin=b, ymax=t, fill=marital, color=happy)) +
      scale_color_discrete (c=0, l=100) +
      theme(panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(),
            axis.text.x = element_blank())
    ggplotly(p, tooltip = c("fill", "color"), source = "plot1") 
  })
  # Scatterplot2 will display brushed subset from sp1.
  output$scatter <- renderPlotly({
    # event_data() listens for the "selected" event in the "source".
    s <- event_data("plotly_click", source = "plot1")
    print(s) 
    level <- s$curveNumber + 1
    # level for 'happy' categorical var
    # Output from the brushing 'event'(x, y, curve#, point#=(obs#-1) in mtcars)
    # Diff of 1 between point# and obs# index is common when using diff languages
    if (length(s)) {
      # curveNumber is the category from (0 to 14)
      # Numbered from the bottom up, starting with left col
      if (0 < level < 6) {
        hap <- levels(happy$happy)[1]
      } else if (5 < level < 11) {
        hap <- levels(happy$happy)[2]
      } else {
        hap <- levels(happy$happy)[3]
      }
      # level for 'marital'
      m <- level %% 5
      mar <- levels(happy$marital)[m]
      ggplot() +
        geom_point(data=happy, aes(x=age, y=wtssall)) +
        geom_point(data = happy[c(happy$happy==hap, happy$marital==mar), ], aes(x=age, y=wtssall), colour="red")
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui, server)