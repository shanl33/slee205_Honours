library(GGally)

# Retrieve data
source("files/NZQAdata.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("scaling", "Select axes scaling", choices=c(
        "Standardise univariately" = "std", 
        "Univariate min and max" = "uniminmax", 
        "Global min and max" = "globalminmax"), selected="univariate")), 
  mainPanel(plotOutput("pcp", width="100%"))
  )
)

server <- function(input, output) {
  output$pcp <- renderPlot(
    ggparcoord(akl, columns=2:5, groupColumn="Decile", scale=input$scaling, title="Achievement of Auckland schools in 2016")
    )
}

shinyApp(ui, server)
