library(ggplot2)
library(plotly)
library(shiny)
library(DT)

# Function for reading and tidying file from Kamar
read_kamar <- function(path) {
  kamarRaw <- read.csv(path)
  # Select vars to keep
  kamar <- kamarRaw[c(1:3, 7, 8, 
                      12, 15, 21, 24, 
                      32, 40, 41, 44, 
                      47, 50, 56, 59)]
  # Rename vars
  colnames(kamar) <- c("ID", "Last_name", "First_name", "Tutor", "Ethnicity", 
                       "Attd_percent", "Prev_Level", "Current_Level", "Credits_remain", 
                       "Int_remain", "Ext_remain", "Lit1_AS", "Lit1_US",
                       "Lit_Read", "Lit_Write", "Num_AS", "Num_US")
  
  # Create L1 literacy and numeracy vars (numeric)
  kamar$Lit1 <- kamar$Lit1_AS + kamar$Lit1_US
  kamar$Num1 <- kamar$Num_AS + kamar$Num_AS
  
  # Create UE num and lit vars (factors)
  kamar$UE_Num <- ifelse(kamar$Num1<10, "No_Num", "UE_Num")
  kamar$Lit <- ifelse(kamar$Lit_Read>=5 & kamar$Lit_Write>=5, "UE_Lit",
                      ifelse(kamar$Lit1<10, "No_L1_Lit", "L1_Lit_only")) 
  
  # Total credits earned for current certificate
  kamar$Credits_for_cert <- ifelse(kamar$Prev_Level<20, kamar$Prev_Level+kamar$Current_Level,
                                   kamar$Current_Level+20)
  kamar$Rank <- rank(kamar$Credits_for_cert, ties.method="first")
  kamar$Merit <- kamarRaw[, 19]
  kamar$Excellence <- kamarRaw[, 20]
  return(kamar)
}

## Shiny app
## ui setup:
ui <- fluidPage(
  h2("NCEA progress"),
  #inputPanel(
    # Upload file
    fileInput("file", "Choose CSV file",
              multiple=TRUE,
              accept=c("text/csv", 
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  #),
  plotlyOutput("plot"),
  dataTableOutput("tbl")
  #fluidRow(
    #column(8, plotlyOutput("plot")),
    #column(12, dataTableOutput("tbl"))
  #)
)

## server setup:
server <- function(input, output) {
  
  ## render plot
  output$plot <- renderPlotly({
    req(input$file)
    kamar <- read_kamar(input$file$datapath)
    
    # Plot. UE Numeracy as pch (symbol) and Lit as colour
    p <- ggplot(kamar, aes(y=Rank, colour=Lit, label=First_name, label1=Last_name, label2=Tutor)) +
      geom_point(aes(x=Credits_for_cert, pch=UE_Num)) +
      geom_point(aes(x=Credits_for_cert+Int_remain), alpha=0.4, pch=3) +
      geom_vline(xintercept=80, colour="grey") +
      geom_segment(aes(x=Credits_for_cert, xend=Credits_for_cert+Credits_remain, yend=Rank), alpha=0.2) +
      geom_segment(aes(x=Credits_for_cert, xend=Credits_for_cert+Int_remain, yend=Rank), alpha=0.4) +
      labs(x="Credits for Current NCEA Level Certificate", y="", 
           title="+ marks Max total credits with Internals remaining") +
      theme(axis.ticks.y = element_blank(), 
            axis.text.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.title = element_blank())
    
    ggplotly(p, source="brush", tooltip = c("label", "label1", "label2", "x")) %>%
      layout(dragmode="select")
  })
  
  ## render table
  output$tbl <- renderDataTable({
    req(input$file)
    kamar <- read_kamar(input$file$datapath)
    # Display some vars only
    kamar_table <- kamar[c(3, 2, 4:6, 22, 10, 11, 7, 8, 19, 18, 14, 15, 24, 25)]
    s <- event_data("plotly_selected", source="brush")
    if (length(s)) {
      ## Use y-cord, Rank to find selected students
      selected <- kamar_table[kamar$Rank %in% s$y, ]
      datatable(selected, extensions="FixedColumns",
                options=list(
                  dom="t",
                  scrollX=TRUE,
                  fixedColumns=list(leftColumns=2))
      )
    } else {
      datatable(kamar_table)
    }
  })
  
}

shinyApp(ui, server)
