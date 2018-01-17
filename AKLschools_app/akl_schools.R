# Packages required
# For reading xls files (binary)
library(gdata)
# For interactive plots
library(ggplot2)
library(plotly)
library(shiny)
library(magrittr)

# Download file
download.file("https://www.educationcounts.govt.nz/__data/assets/excel_doc/0005/152609/Student-rolls-by-School-2009-2017.xlsx", "schools.xlsx")
# Read in sheet with 2017 data only
schools2017 <- read.xls("schools.xlsx", sheet="2017", skip=3, header=FALSE)

# Checking
#head(schools2017)
#levels(schools2017$V7) # Regional Council
#levels(schools2017$V4) # Type (Primary, ...)
#levels(schools2017$V19) # Last variable to keep (# Int fee paying)

# Auckland schools and keep only variables of interest
AKL2017 <- schools2017[schools2017$V7=="Auckland Region", c(2:6, 9:19)]
colnames(AKL2017) <- c("Name", "Decile", "Type", "Authority", "Gender", 
                       "Suburb", "Roll", "Female", "Male", "Maori", 
                       "Pasifika", "Asian", "MELAA", "Other", "European", "International")

# Secondary schools only
SecAKL2017 <- subset(AKL2017, grepl("^Secondary", AKL2017$Type))

# Drop schools with Decile > 10 
SecAKL2017 <- subset(SecAKL2017, Decile < 11)
# Drop unused levels for factors
SecAKL2017[, 3:6] <- droplevels(SecAKL2017[, 3:6])
# Trim Suburb names
levels(SecAKL2017$Suburb) <- gsub("Auckland- ", "", levels(SecAKL2017$Suburb))

# Calculate proportions from frequencies
# eg. Collapse # of female & male students to proportion male
propns <- round(SecAKL2017[, 9:16]/SecAKL2017$Roll*100, 1)
colnames(propns) <- paste0(colnames(SecAKL2017[, 9:16]), "_p")

# Merge w rest of data
SecAKL2017 <- cbind(SecAKL2017, propns)
# colnames(SecAKL2017)
axis_choices <- as.list(colnames(SecAKL2017)[c(2, 7, 17:24)])

# Function to subset  dataframeaccording to Name of selected school(s)
selected_schools <- function(event_df) {
  selected <- subset(SecAKL2017, Name %in% event_df$key
}

# Shiny app
ui <- fluidPage(
  inputPanel(
    selectInput("x_axis", "Horizontal_axis", choices=axis_choices, selected="Decile"),
    selectInput("y_axis", "Vertical_axis", choices=axis_choices[-1], selected="International_p"),
    selectInput("colour", "Colour_variable", choices=as.list(colnames(SecAKL2017)[3:6]), selected="Suburb")
  ),
  mainPanel(
    plotlyOutput("plot"),
    verbatimTextOutput("hover"),
    verbatimTextOutput("select")
  )
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    p <- ggplot(SecAKL2017, aes(x=input$x_axis, y=input$y_axis, colour=input$colour, key=Name)) +
      geom_point()
    ggplotly(p) %>% 
      layout(dragmode="select")
  })
  output$hover <- renderPrint({
    d <- event_data("plotly_hover", source="plot")
    if (is.null(d)) "Data for point hovered over appears here" else 
  })
}

shinyApp(ui, server)


