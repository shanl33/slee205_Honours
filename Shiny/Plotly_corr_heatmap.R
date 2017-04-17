library(plotly)
library(shiny)

load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
# Subset numeric vars
AQnumeric <- AQ[, c("DayofMonth", "DepTime", "CRSDepTime", "ArrTime", 
                    "CRSArrTime", "ActualElapsedTime", "CRSElapsedTime", 
                    "AirTime", "ArrDelay", "DepDelay", "Distance", 
                    "TaxiIn", "TaxiOut")]
# Need to convert into proper time format for correlation?
summary(AQ$DepTime)
# Correlation matrix
correlation <- round(cor(AQnumeric), 3)
nms <- names(AQnumeric)
# Interactive correlation matrix as a heatmap w tooltips
plot_ly(x = nms, y = nms, z = correlation, type="heatmap")


