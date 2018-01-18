# Packages required
# For interactive plots
library(ggplot2)
library(plotly)
library(shiny)
library(crosstalk)
library(DT)
# Install dev version of DT?

# Read in data
load("files/SecAKL2016.RData")

# colnames(SecAKL2016)
#axis_choices <- as.list(colnames(SecAKL2016)[c(2, 7, 17:24)])

# Set up shared data to linking
sd <- SharedData$new(SecAKL2016, key=~Name, group="shared")

# Shiny app
ui <- fluidPage(
  #inputPanel(
    #selectInput("xaxis", "Horizontal_axis", choices=axis_choices, selected="Decile"),
    #selectInput("yaxis", "Vertical_axis", choices=axis_choices[-1], selected="International_p"),
    #selectInput("colour", "Colour_variable", choices=as.list(colnames(SecAKL2016)[3:6]), selected="Suburb")
  #),
  mainPanel(
    helpText("Drag and select region to zoom into plot. Double-click on legend items to filter by suburb."),
    plotlyOutput("plot"),
    DT::dataTableOutput("table")
    #fluidRow(
     # p(class="text-center", downloadButton("file", "Download filtered data"))
    #)
  )
)

server <- function(input, output) {
  # Set up shared data to linking
  sd <- SharedData$new(SecAKL2016, key=~Name, group="shared")
  
  output$plot <- renderPlotly({
    #sd <- SharedData$new(SecAKL2016, key=~Name, group="shared")
    #s <- input$table_rows_selected
    #if (!length(s)) {
     # p
    #}
    p <- ggplot(sd, aes(x=Decile, y=International_p, colour=Suburb, label=Name, label1=Roll)) +
      geom_point()
    ggplotly(p) %>%
      highlight(on="plotly_select", off="plotly_deselect", persistent=TRUE)
      #layout(dragmode="select")
  })
  
  # Show selected schools in table
  output$table <- DT::renderDataTable({
    # Set up shared data to link table 
    #sdTable <- SharedData$new(SecAKL2016, key=~Name, group=sd.group)
    df <- SecAKL2016[sd$selection(), ]
    dt <- DT::datatable(SecAKL2016)
    #print(df)
    if (nrow(df) == 0) {
      dt
    } else {
      df_dt <- DT::datatable(df)
      df
    }
  })
  
  # Download filtered ata
  #output$file = downloadHandler("selected_schools.csv", content=function(file) {
   # s <- input$table_rows_selected
    #if(length(s)) {
     # write.csv(SecAKL2016[s, , drop=FALSE], file)
    #} else if (!length(s)) {
     # write.csv(SecAKL2016[sd$selection(), ], file)
    #}
  #})
}

shinyApp(ui, server)


