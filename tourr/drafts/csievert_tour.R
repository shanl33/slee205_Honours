#devtools::install_github("ropensci/plotly#554")
library(plotly)
library(crosstalk)
library(htmltools)
library(dplyr)
library(tidyr)
library(tourr)

# See if tour is smoother using Crosstalk+Plotly rather than Shiny+Plotly
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
# Remove obs with any NA values 
ach_narm <- achieved[complete.cases(achieved),] #407 schools left
ach_narm$Decile <- as.factor(ach_narm$Decile)

# Code from CSievert ------------------------------------------------------
# Standardise measurement vars
ncea01 <- rescale(ach_narm[,2:5]) # Xs [0,1]
rownames(ncea01) <- rownames(ach_narm)
# new_tour interpolates and generates new bases when needed
tour <- new_tour(ncea01, guided_tour(cmass, d=2, max.tries = 50, scale=FALSE),NULL) 

tour_dat <- function(step_size) {
  step <- tour(step_size) #step is a list of 3 ($proj, $target, $step)
  # $proj basis is the current proj basis
  # $target basis stays the same for all tour(#)
  # $step is a cumulative counter of number of calls to the tour() fn
  print(step)
  proj <- center(ncea01 %*% step$proj) # Projected data matrix
  # df with projected x and y coordinates
  data.frame(x = proj[,1], y = proj[,2], Name = rownames(ncea01))
}

proj_dat <- function(step_size) {
  step <- tour(step_size)
  # df with x and y coordinate coeff weights for measurement vars
  data.frame(x = step$proj[,1], y = step$proj[,2], measure = colnames(ncea01))
}

steps <- c(0, rep(1/15, 1000))
stepz <- cumsum(steps)

# tidy version of tour data
tour_dats <- lapply(steps, tour_dat)
tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
tour_dat <- dplyr::bind_rows(tour_datz)
tour_dat$Decile <- rep_len(ach_narm$Decile, dim(tour_dat)[1])

# tidy version of tour projection data
proj_dats <- lapply(steps, proj_dat)
proj_datz <- Map(function(x, y) cbind(x, step = y), proj_dats, stepz)
proj_dat <- dplyr::bind_rows(proj_datz)

# Axes for tour plot ("tour") and measurement var coeffs plot ("axes")
ax <- list(
  title = "", range = c(-1.1, 1.1), 
  zeroline = F, showticklabels = F
)

options(digits = 3)
# Tried colour = ~Decile but does not register and extends processing time for subplot() later on
tour <- tour_dat %>%
  SharedData$new(~Name, group = "ncea") %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black"), 
          height = 450, width = 800) %>%
  add_markers(text = ~Name, hoverinfo = "text") %>%
  layout(xaxis = ax, yaxis = ax)

axes <- proj_dat %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, hoverinfo = "none") %>%
  add_segments(xend = 0, yend = 0, color = I("gray85")) %>%
  add_text(text = ~measure, color = I("black")) %>%
  layout(xaxis = ax, yaxis = ax)

# very important these animation options are specified _after_ subplot()
# since they call plotly_build(., registerFrames = T)
# (Took a few minutes to run)
tour <- subplot(tour, axes, nrows = 1, shareY = T, margin = 0) %>% 
  animation_opts(33) %>%
  hide_legend() %>%
  layout(dragmode = "select") %>%
  highlight(persistent = TRUE)

html <- tags$div(
  style = "display: flex; flex-wrap: wrap",
  tags$div(tour, align = "center", style = "width: 50%; padding: 1em;")
  #tags$div(p2, style = "width: 50%; padding: 1em; border: solid;"),
  #tags$div(p3, style = "width: 50%; padding: 1em; border: solid;"),
  #tags$div(p4, style = "width: 50%; padding: 1em; border: solid;")
)

# opens in an interactive session
res <- html_print(html)

# Testing
step0 <- tour(0)
XA0 <- center(ncea01%*%step0$proj)
class(XA0)
