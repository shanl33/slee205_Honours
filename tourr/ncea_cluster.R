# For wrangling data
library(dplyr)
library(tidyr)
# For graphical visualisation
library(ggplot2)
library(plotly)
library(tourr)
library(shiny)

# NCEA Data setup --------------------------------------------------------------
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
achieved[2:5] <- sapply(achieved[2:5], zeros)
# Remove schools with 'small cohort' warning (obscures pattern in non-small cohorts).
achieved <- achieved[achieved$Small_sch=="",] #437 school left
achieved <- achieved[, -8] #Remove Small_sch var
# 'achieved' contains schools with ONE or more % achievement rate (by participation)
# NA's used otherwise
# Small cohort schools removed
# Remove obs with any NA values 
ach_narm <- achieved[complete.cases(achieved),] #407 schools left
ach_narm$Decile <- as.factor(ach_narm$Decile)
class(ach_narm) <- "data.frame"
rownames(ach_narm) <- ach_narm$School
akl <- as.data.frame(ach_narm[ach_narm$Region=="Auckland", ]) #90 schools in Akl
str(akl)
rownames(akl) <- akl$School
rownames(akl)
guidedTour_app(akl[,-1], factors=1, index="holes")


# Check with tourr tour ---------------------------------------------------
library(colorspace)
# Colour by decile
pal <- sequential_hcl(11)
col <- pal[ach_narm$Decile]
animate_xy(ach_narm[,2:5], planned_tour(t1), col=col)
# Centers are not far apart, LDA not appropriate.
ach_narm %>% 
  group_by(Decile) %>% 
  summarise(mean=mean(L3), std = sqrt(var(L3)), med = median(L3))
t2 <- save_history(ach_narm[,2:5], guide_tour(holes, max.tries=50), sphere = TRUE, max=50)
animate_xy(ach_narm[,2:5], planned_tour(t2), col=col)
# Tour (Projection Pursuit using cmass) -------------------------------------------------------------------
# Save tour
t1 <- save_history(ach_narm[,2:5], guided_tour(cmass, d=2, max.tries = 50), sphere=TRUE, max=50)
t1interp <- interpolate(t1) # Uses basis to create smooth tour
# Central mass index and projection pursuit tour function
ach_sphere <- attr(t1, "data") #(n by p) = (407 by 4)
max_axis <- 0
min_axis <- 0
cmass_tour <- function(basis) {
  # Projected data matrix 
  XA <- ach_sphere%*%matrix(basis, ncol=2) # (n by d) = (407 by 2)
  axes_max <- apply(XA, 2, max)
  max_axis <<- max(axes_max, max_axis)
  axes_min <- apply(XA, 2, min)
  min_axis <<- min(axes_min, min_axis)
  cmass_index <- (sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1]-exp(-dim(XA)[2]/2))/(1-exp(-dim(XA)[2]/2))
  list(XA, cmass_index)
}
# Apply function to each projection basis
t1_tour <- apply(t1interp, 3, FUN = cmass_tour)
cmass_index <- data.frame(iteration=1:length(t1_tour))
for(i in 1:length(t1_tour)) {
  cmass_index$index[i] <- unlist(t1_tour[[i]][2])
}

# Shiny app ---------------------------------------------------------------

ui <- fluidPage(navbarPage("2D Guided Tour of NCEA data",
                  tabPanel("Animation",
                    mainPanel(
                      sliderInput("iteration", "Interation", min = 1, value = 1, step = 1,
                                  max = length(t1_tour), animate = animationOptions(300), width = 500),
                      plotlyOutput("tour")
                    )       
                  ),
                  tabPanel("Central Mass Index",
                    mainPanel(
                      plotlyOutput("index"),
                      plotlyOutput("tour2")
                    )
                  )
))

server <- function(input, output, session) {
  output$tour <- renderPlotly({
    proj <- as.data.frame(t1_tour[[input$iteration]][[1]])
    proj$Decile <- as.numeric(as.character(ach_narm$Decile))
    proj$School <- ach_narm$School
    ggplot(proj, aes(x=V1, y=V2, label=School, col=Decile)) +
      geom_point() +
      scale_x_continuous(limits = c(min_axis, max_axis)) +
      scale_y_continuous(limits = c(min_axis, max_axis)) +
      scale_color_gradient(low = "#56B1F7", high = "#132B43")
    ggplotly() # Renders faster as a plotyly object rather than ggplot2
  })
  
  output$index <- renderPlotly({
    ggplot(cmass_index, aes(x=iteration, y=index)) +
      geom_point() +
      geom_line()
    ggplotly(source = "index_plot")
  })
  
  output$tour2 <- renderPlotly({
    s <- event_data("plotly_click", source = "index_plot")
    print(s)
    if (length(s)) {
      iter <- s$x
    } else {
      iter <- 1
    }
    proj <- as.data.frame(t1_tour[[iter]][[1]])
    proj$Decile <- as.numeric(as.character(ach_narm$Decile))
    proj$School <- ach_narm$School
    ggplot(proj, aes(x=V1, y=V2, label=School, col=Decile)) +
      geom_point() +
      scale_x_continuous(limits = c(min_axis, max_axis)) +
      scale_y_continuous(limits = c(min_axis, max_axis)) +
      scale_color_gradient(low = "#56B1F7", high = "#132B43")
    ggplotly() # Renders faster as a plotyly object rather than ggplot2
  })
}
shinyApp(ui, server)

