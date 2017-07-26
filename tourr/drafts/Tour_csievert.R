#devtools::install_github("ropensci/plotly#554")
library(plotly)
library(crosstalk)
library(htmltools)
library(dplyr)
library(tidyr)
library(tourr)
library(MASS) # crabs dataset
data("crabs")
# See if tour is smoother using Crosstalk+Plotly rather than Shiny+Plotly
# Code from <https://github.com/cpsievert/pedestrians/blob/master/docs/stl-tour.R>

# Code from CSievert ------------------------------------------------------
# Standardise measurement vars
Xdataset <- rescale(crabs[,4:8]) # Xs [0,1]
rownames(Xdataset) <- rownames(crabs)
# new_tour interpolates and generates new bases when needed
tour <- new_tour(Xdataset, grand_tour(), NULL) 

tour_dat <- function(step_size) {
  step <- tour(step_size) #step is a list of 3 ($proj, $target, $step)
  # $proj basis is the current proj basis
  # $target basis stays the same for all tour(#)
  # $step is a cumulative counter of number of calls to the tour() fn
  proj <- center(Xdataset %*% step$proj) # Projected data matrix
  # df with projected x and y coordinates
  data.frame(x = proj[,1], y = proj[,2], Name = rownames(Xdataset))
}

proj_dat <- function(step_size) {
  step <- tour(step_size)
  # df with x and y coordinate coeff weights for measurement vars
  data.frame(x = step$proj[,1], y = step$proj[,2], measure = colnames(Xdataset))
}

steps <- c(0, rep(1/15, 1000))
stepz <- cumsum(steps)

# tidy version of tour data
tour_dats <- lapply(steps, tour_dat)
tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
tour_dat <- dplyr::bind_rows(tour_datz)

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
  SharedData$new(~Name, group = "crabs") %>%
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
XA0 <- center(Xdataset%*%step0$proj)
class(XA0)
