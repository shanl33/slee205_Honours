devtools::install_github("ropensci/plotly")
devtools::install_github("rstudio/crosstalk")
library(dplyr)
library(crosstalk)
library(plotly)

load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay
AQ$rownames <- rownames(AQ)

sd <- SharedData$new(AQ, key=~rownames) 

p1 <- plot_ly(AQ, x=~AirTime, y=~NetDelay) %>%
  add_markers(alpha=0.1, color=I("black")) %>%
  # Layers on selected points in red.
  add_markers(data=sd, color=I("red"))

p2 <- plot_ly(AQ, x=~CRSDepTime, y=~CRSArrTime) %>%
  add_markers(alpha=0.1, color=I("black")) %>%
  add_markers(data=sd, color=I("red"))

# Arranges and draws plot side-by-side in the same view
subplot(p1, p2) %>%
  layout(dragmode="select")