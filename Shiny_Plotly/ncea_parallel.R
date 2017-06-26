library(shiny)
library(GGally)
library(plotly)
library(dplyr)
library(tidyr)
# To do: Change 'plot' to a slider for entry of decile

# Wrangling data ----------------------------------------------------------
nzqa2016 <- read.csv("http://www.nzqa.govt.nz/assets/Studying-in-NZ/Secondary-school-and-NCEA/stats-reports/2016/Qualification-Statistics-School-2016-29032017.csv")
# Drop vars that will not be used (eg. cumulative achievement)
nzqa <- nzqa2016[,-c(1,8,10)]
names(nzqa) <- c("Decile", "Region", "School", "Year", "Qualification", "Achieve_participate", "Achieve_roll", "Small_sch")
levels(nzqa$Qualification) <- c("L1", "L2", "L3", "UE")
# Subset to use only Year 11 with Level 1, etc
nzqa <- nzqa %>% filter(((Qualification=="L1")&(Year==11))|((Year==12) & (Qualification=="L2"))|
                      ((Year==13) & (Qualification=="L3"))|((Year==13) & (Qualification=="UE")))
# Reshape so that one row = one school
# Current.Achievement.Rate.Participation kept for analysis only
achieved <- nzqa %>% 
  spread(Qualification, Achieve_participate, fill=0) %>%
  group_by(School) %>%
  summarise_at(c("L1", "L2", "L3", "UE"), sum) %>%
  inner_join(nzqa[, c(1, 2, 3, 8)]) %>% #Add Decile and Region and Small_sch variables
  distinct() %>% #One row per school
  filter(!((L1==0)&(L2==0)&(L3==0))) #Remove schools with 0% achievement rate for all levels
# Function to replace 0% with NA
zeros <- function(col) {
  replace(col, col==0, NA)
}
achieved$L1 <- zeros(achieved$L1)
achieved$L2 <- zeros(achieved$L2)
achieved$L3 <- zeros(achieved$L3)
achieved$UE <- zeros(achieved$UE)
# Remove schools with 'small cohort' warning (obscures pattern in non-small cohorts).
achieved <- achieved[achieved$Small_sch=="",] #437 school left
achieved <- achieved[, -8]
# 'achieved' contains schools with ONE or more % achievement rate (by participation)
# NA's used otherwise
# Small cohort schools removed

achieved$Decile <- factor(achieved$Decile)

ui <- fluidPage(
  mainPanel(
    plotlyOutput("plot"),
    plotlyOutput("pcp"),
    sliderInput("alpha", "Alpha color scale", min=0, max=1, value = 0.5)
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    ggplot(achieved) +
      geom_bar(aes(x=Decile))
    ggplotly(source = "p")
  })
  
  output$pcp <- renderPlotly({
    s <- event_data("plotly_click", source = "p")
    print(s)
    # Order by "brushed" grouping so that it gets drawn last
    if (length(s)) {
      achieved$brushed <- rep("0", length(achieved$School))
      for (i in 1:length(achieved$brushed)){
        if (achieved$Decile[i]==(s$x-1)) {
          achieved$brushed[i] = "1"
        }
      }
      ggparcoord(data = achieved, columns = 2:5, scale = "uniminmax", groupColumn = "brushed",
                alphaLines = input$alpha, showPoints = T) +
        theme(legend.position = "none") +
        scale_color_manual(values = c("blue", "red"))
      ggplotly() 
    } else {
      # ggparcoord(data = achieved, columns = 2:5, scale = "center", scaleSummary = "median",
      #alphaLines = input$alpha, showPoints = T)
      ggparcoord(data = achieved, columns = 2:5, scale = "uniminmax", 
                 alphaLines = input$alpha, showPoints = T)
      ggplotly()
    }
  })
}
shinyApp(ui, server)
