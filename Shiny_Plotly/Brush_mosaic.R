install.packages("productplots")
library(productplots)
library(ggplot2)
library(plotly)
library(shiny)

data("happy")
# 51,020 obs so interaction is VERY slow.
# Change "scatter" to a different plot, maybe bar chart w proportion highlighted

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
    lev <- s$curveNumber + 1
    # "lev" converts curveNumber so it starts at 1 and can match up with length of levels for each factor
    # Max (curveNumber+1) = total # of rectangles = length(levels(factor1)))*length(levels(factor2)))
    # factor1 levels will align w dividing max curve# into intervals of length: length(levels(factor2)))
    # eg. "hap" = level for 'happy' categorical var (1-5 is level1 of happy, 6-10 is..)
    # factor2 levels will align w modular division of (curveNumber+1) %% lenght(levels(factor2))
    # Remainder 0 = max level for factor2 = length(levels(factor2)) 
    # Otherwise the rest match up as is (ie. rem)
    # "mar" = level for 'marital'
    if (length(s)) {
      # curveNumber is the category from (0 to 14)
      # Numbered from the bottom up, starting with left col
      if (lev<6) {
        hap <- levels(happy$happy)[1]
      } else if (lev>5 & lev<11) {
        hap <- levels(happy$happy)[2]
      } else {
        hap <- levels(happy$happy)[3]
      }
      m <- lev %% 5
      if (m==0) {
        mar <- levels(happy$marital)[5]
      } else {
        mar <- levels(happy$marital)[m]
      }
      ggplot() +
        geom_point(data=happy, aes(x=year, y=age)) +
        geom_jitter() +
        geom_point(data = happy[c(happy$happy==hap, happy$marital==mar), ], 
                   aes(x=year, y=age), colour="red") +
        geom_jitter()
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui, server)