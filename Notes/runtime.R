# Flight data: 
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay
library(microbenchmark) 


# Compare different glyphs for base graphics ----------------------------
system.time(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=1))
system.time(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=3))
# system.time almost halved using pch=3 crosses rather than open circles.
# system.time not reliable, should measure over repeated times (using a loop, see bottom)
# microbenchmark takes too long. Maybe use a plot of 50 000 runif() points.
microbenchmark(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=1))
microbenchmark(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=3))

# Compare different glyphs for grid graphics ------------------------------
# Need to use print() since grid graphics do not print on default inside the function
library(ggplot2)
system.time(print(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1)))
system.time(print(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3)))
# not as big of a difference as in base plots.


# Compare base and grid graphics ------------------------------------------
system.time(plot(x=AQ$AirTime, y=AQ$NetDelay, pch=3))
system.time(print(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3)))
# Base plots faster than grid plots (confirmed by Paul too)
# Diff between plotting symbols seems more pronounced for base plots (?unreliable results).
# Less pixels reqd for crosses than circles, so should be more time efficient. See ggobiManual ('Large datasets' section)

# Compare plot_ly and ggplotly --------------------------------------------
# Not sure if need to include print()?
# Seems obvious that plot_ly should be faster since one less function to go through.
library(plotly)
system.time(print(plot_ly(data=AQ, x=~AirTime, y=~NetDelay, marker=list(symbol=0))))
system.time(print(plot_ly(data=AQ, x=~AirTime, y=~NetDelay, marker=list(symbol=3))))
system.time(print(ggplotly(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=1))))
system.time(print(ggplotly(ggplot(AQ)+geom_point(aes(AirTime, NetDelay), pch=3))))

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