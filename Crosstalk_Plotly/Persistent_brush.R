devtools::install_github("ropensci/plotly")
devtools::install_github("rstudio/crosstalk")
library(dplyr)
library(crosstalk)
library(plotly)

mtcars$rownames <- rownames(mtcars)

sd <- SharedData$new(mtcars, key=~rownames)

# For x-axis label to show only on p2.
x <- list(title = "")

p1 <- plot_ly(mtcars, y=~mpg, x=~wt) %>%
  add_markers(color=I("black")) %>%
  add_markers(data=sd, color=I("black"))%>%
  layout(xaxis=x)

p2 <- plot_ly(mtcars, y=~hp, x=~wt) %>%
  add_markers(color=I("black")) %>%
  add_markers(data=sd, color=I("black")) 

subplot(p1, p2, nrows = 2, titleX=T, titleY = T) %>%
  layout(dragmode="select") %>%
  hide_legend() %>%
  highlight(off="plotly_deselect", color = "blue", persistent=T) 

# Persistent brushing: options(persistent=T)