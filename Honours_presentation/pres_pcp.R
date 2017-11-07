library(ggplot2)
library(plotly)
library(shiny)
library(tidyr) # For PCs reshaping output for plot
library(crosstalk)
#library(tourr) # For rescale function for univariate_minmax

# Load Auckland schoos data
load("files/akl.RData")

# Transform data frame to plot PCP
ggpcp <- function(df, a=1) {
  # Transform df to a long data frame
  df$ID <- rownames(df)
  long <- gather(df, "Qualification", "Achievement", c("L1", "L2", "L3", "UE")) %>%
    SharedData$new(key=~ID, group="ncea.pcp")
  # Static PCP
  pcp.static <- ggplot(long, aes(x=Qualification, y=Achievement, colour=Decile, group=ID)) +
    geom_line(alpha=a) +
    geom_point(alpha=a, size=0.01) +
    labs(x="Qualification level", y="Achievement rate") 
  return(pcp.static)
}

# Shiny app

ui <- fluidPage(
  splitLayout(cellWidths=c("55%", "15%", "30%"),
              plotlyOutput("pcp"), 
              radioButtons("scaling", "Select axes scaling", 
                           c("Standardise", "Univariate_minmax", "Global_minmax"), selected="Global_minmax")
  )
)

server <- function(input, output, session) {
  # PCP plot
  output$pcp <- renderPlotly({
    if (input$scaling == "Standardise") {
      nzqa.scale <- cbind(akl[6], scale(akl[2:5]))
    } else if (input$scaling == "Univariate_minmax") {
      scaled <- apply(akl[2:5], 2, function(x) (x - min(x))/diff(range(x)))
      nzqa.scale <- cbind(akl[6], scaled)
    } else {
      nzqa.scale <- akl
    }
    static <- ggpcp(nzqa.scale) +
      ggtitle("Achievement rates of Auckland schools in 2016") +
      labs(x="Qualification level", y="Achievement")
    ggplotly(static, tooltip=c("group", "colour", "x", "y")) %>%
      highlight(on="plotly_select", off="plotly_deselect", persistent=T, dynamic=T) %>%
      layout(autosize=F, width=800, height=400, dragmode="select")
  })
}

shinyApp(ui, server)