library(ggplot2)
library(plotly)
library(shiny)
library(DT)

## Check: L1.Total and L1.Attempted include practice externals?
## Tutor group variable required

# Read in csv file from Kamar
#kamar <- read.csv("attd_credits.csv")
#summary(kamar)
#is.factor(kamar$Attendance)
# Check data
# head(yr11)
# summary(yr11)

# Shorten col heading names so DT table fits across screen?

# Function for reading and tidying file from Kamar
read_kamar <- function(path) {
  kamar <- read.csv(path)
  # Extract cols of interest
  # Removing Year.Level (3rd col)
  yr11 <- kamar[ , c(1, 2, 4:12)]
  # Achievement% = L1.Total/L1.Attempted
  yr11$Achievement <- round((yr11$L1.Total/yr11$L1.Attempted)*100)
  return(yr11)
}

# Can't use this if using uploading own csv, since can't be predefined
# Find col indices of factors (except First and Last name)
# To use as input options for colour of scatter points
#fac_cols <- sapply(yr11, is.factor) %>%
 # grep("TRUE", .)


## Shiny app
## ui setup:
ui <- fluidPage(
  h2("Academic & Attendance progress"),
  inputPanel(
    # Upload file
    fileInput("file", "Choose CSV file",
              multiple=TRUE,
              accept=c("text/csv", 
                       "text/comma-separated-values,text/plain",
                       ".csv")),
    # Select factor variable to use for colour setting
    # Can't use this if using uploading own csv, since can't be predefined
    # Need to set up a second categorical plot and use linked brushing.
    # Or can pre-set names to match Kamar headings?
    #selectInput("col_factor", label="Select colour variable", 
     #           choices=as.list(colnames(yr11)[fac_cols[-c(1:2)]])),
    #To do: Input to choose x-axis variable
    #need to create L1, L2 columns (L1=L1+L2+L3 Totals, L2=L2+L3)
    #change scatterplot x to input$x_factor and change hline vline to sloped x+y=80
    #write a function based on input$x_factor's input value?
    selectInput("x_factor", label="Select x-axis variable", 
                choices=c("Attendance%"="Attendance",
                          "L1+ gained"="L1",
                          "L2+ gained"="L2")),
    sliderInput("attd_line", label="Attendance boundary", 
                min=0, max=100, step=1, value=90),
    sliderInput("ach_line", label="Achievement boundary", 
                min=0, max=100, step=1, value=80)
  ),
  fluidRow(
    column(8, plotlyOutput("plot")),
    #column(8, plotOutput("plot")),
    column(12, dataTableOutput("tbl"))
  )
)

## server setup:
server <- function(input, output) {
  
  ## render plot
  output$plot <- renderPlotly({
    req(input$file)
    yr11 <- read_kamar(input$file$datapath)
    
    # Scatterplot of Attd% vs Acheivement%
    p <- ggplot(yr11, aes(x=Attendance, y=Achievement, label=First.Name, 
                          colour=Literacy, shape=Numeracy)) +
      geom_point() +
      geom_hline(yintercept=input$ach_line, size=0.5, colour="grey60", linetype="dashed") +
      geom_vline(xintercept=input$attd_line, size=0.5, colour="grey60", linetype="dashed") +
      labs(x="Attendance%", y="Achievement% (Credits gained/Credits attempted)", 
           title="Attendance vs Achievement for Year 11 (2018)") 
    
    ggplotly(p, source="brush") %>%
      layout(dragmode="select")
  })
  
  ## render table
  output$tbl <- renderDataTable({
    req(input$file)
    yr11 <- read_kamar(input$file$datapath)
    # Print brush info to check
    s <- event_data("plotly_selected", source="brush")
    #print(s) 
    if (length(s)) {
      ## add one because they count from 0, but assumes pointNumber = index
      selected <- yr11[s$pointNumber+1, ]
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
