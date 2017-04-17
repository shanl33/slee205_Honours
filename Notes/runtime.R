# Flight data: 
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay

# Compare different glyphs for base graphics
system.time(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=1))
system.time(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=3))
microbenchmark(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=1))
microbenchmark(p <- plot(x=AQ$AirTime, y=AQ$NetDelay, pch=3))
# system.time not reliable, should measure over repeated times (using a loop)

# Compare different glyphs for grid graphics
library(ggplot2)
library(microbenchmark) 
microbenchmark(
  ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1),
  ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3)
)
system.time(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1))
system.time(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3))

microbenchmark(
  ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1),
  ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3),
  plot(x=AQ$AirTime, y=AQ$NetDelay, pch=1),
  plot(x=AQ$AirTime, y=AQ$NetDelay, pch=3)
)

library(plotly)
microbenchmark(
  plot_ly(data=AQ, x=~AirTime, y=~NetDelay, marker=list(symbol=0)),
  plot_ly(data=AQ, x=~AirTime, y=~NetDelay, marker=list(symbol=3))
)

microbenchmark(
  ggplotly(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1)),
  ggplotly(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3))
)

# All together: Takes a couple of mins to run the following.
# Plot results for report
runtime <- microbenchmark(
  plot_ly(data=AQ, x=~AirTime, y=~NetDelay, marker=list(symbol=0)),
  plot_ly(data=AQ, x=~AirTime, y=~NetDelay, marker=list(symbol=3)),
  ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1),
  ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3),
  plot(x=AQ$AirTime, y=AQ$NetDelay, pch=1),
  plot(x=AQ$AirTime, y=AQ$NetDelay, pch=3),
  ggplotly(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1)),
  ggplotly(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3))
)

# Example from: http://adv-r.had.co.nz/Performance.html
x <- runif(100)
microbenchmark(
  sqrt(x),
  x^0.5
)

n <- 1e6
system.time(for (i in 1:n){
  sqrt(x)
}) / length(n)
system.time(for (i in 1:n){
  x^0.5
}) / length(n)