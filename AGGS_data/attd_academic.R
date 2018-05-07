library(ggplot2)
library(plotly)
library(shiny)
library(DT)

# Check: L1.Total and L1.Attempted include practice externals?
# Tutor group variable required

# Read in csv file from Kamar
kamar <- read.csv("attd_credits.csv")

# Extract cols of interest
# Removing Year.Level (3rd col)
yr11 <- kamar[ , c(1, 2, 4:12)]

# Find col indices of factors (except First and Last name)
# To use as input options for colour of scatter points
fac_cols <- sapply(yr11, is.factor) %>%
  grep("TRUE", .)

# Shorten col heading names so DT table fits across screen?

# Check data
# head(yr11)
# summary(yr11)

# Achievement% = L1.Total/L1.Attempted
yr11$Achievement <- round((yr11$L1.Total/yr11$L1.Attempted)*100)

## Shiny app
## ui setup:
ui <- fluidPage(
  h2("Year 11 Class of 2020"),
  inputPanel(
    # Upload file
    #fileInput("file", "Choose CSV file",
     #         accept=c("text/csv", 
      #                 "text/comma-separated-values,text/plain",
       #                ".csv")
        #      ),
    # Select factor variable to use for colour setting
    selectInput("col_factor", label="Select colour variable", 
                choices=as.list(colnames(yr11)[fac_cols[-c(1:2)]])),
    sliderInput("attd_line", label="Attendance boundary", 
                min=0, max=100, step=1, value=60),
    sliderInput("ach_line", label="Achievement boundary", 
                min=0, max=100, step=1, value=60)
  ),
  fluidRow(
    column(8, plotlyOutput("plot")),
    column(12, dataTableOutput("tbl"))
  )
)

## server setup:
server <- function(input, output) {
  
  ## read in file
  #inFile <- input$file
  
  #if(is.null(inFile))
  #  return(NULL)
  
  #kamar <- read.csv(inFile$datapath)
  
  # Extract cols of interest
  # Removing Year.Level (3rd col)
  #yr11 <- kamar[ , c(1, 2, 4:12)]
  
  # Find col indices of factors (except First and Last name)
  # To use as input options for colour of scatter points
  #fac_cols <- sapply(yr11, is.factor) %>%
   # grep("TRUE", .)
  
  # Achievement% = L1.Total/L1.Attempted
  #yr11$Achievement <- round((yr11$L1.Total/yr11$L1.Attempted)*100)
  
  ## render plot
  output$plot <- renderPlotly({
    yr11$col_factor <- yr11[, input$col_factor] 
    # Scatterplot of Attd% vs Acheivement%
    # Remove first name for shinyapp version
    p <- ggplot(yr11, aes(x=Attendance, y=Achievement, label=First.Name, 
                          colour=col_factor)) +
      geom_point() +
      geom_hline(yintercept=input$ach_line, size=0.5, colour="grey60", linetype="dashed") +
      geom_vline(xintercept=input$attd_line, size=0.5, colour="grey60", linetype="dashed") +
      labs(x="Attendance%", y="Achievement% (Credits gained/Credits attempted)", 
           title="Attendance vs Achievement for Year 11 (2018)",
           colour=input$col_factor) +
      coord_equal()
    
    ggplotly(p) %>%
      layout(dragmode="select")
  })
  
  ## render table
  output$tbl <- renderDataTable({
    if (!is.null(event_data("plotly_selected"))) {
      ## add one because they count from 0, but assumes pointNumber = index
      ind <- event_data("plotly_selected")[, "pointNumber"] + 1
      # hopefully indices match to original df:
      selected <- yr11[ind, ]
      datatable(selected)
    } else {
      datatable(yr11)
    }
  })
  
}

shinyApp(ui, server)

# Test plot
#ggplot(yr11, aes(x=Attendance, y=Achievement, label=First.Name)) +
#  geom_point() +
#  geom_hline(yintercept=50, size=0.5, colour="grey40", linetype="dashed") +
#  geom_vline(xintercept=50, size=0.5, colour="grey40") +
#  labs(x="Attendance%", y="Achievement% (Credits gained/Credits attempted)", 
#       title="Attendance vs Achievement for Year 11 (2018)")
