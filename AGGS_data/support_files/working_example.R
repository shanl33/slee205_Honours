# Working example
# Must define separately in each 'output' definition in server.
# Cannot read file outside of 'output' definition ("reactive environment")
# Write a function for tidying KAMAR data. 

library(shiny)
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # Sidebar panel for inputs ----
    inputPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("scatter"),
      # Output: Data file ----
      tableOutput("contents")
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    return(df)
    
  })
  
  output$scatter <- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath)
    ggplot(df, aes(x=Attendance, y=External)) +
      geom_point()
  })
  
}
# Run the app ----
shinyApp(ui, server)