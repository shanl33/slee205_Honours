library(trelliscopejs)
library(plotly)

load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay
summary(AQ) #For setting up axes limits

# Conditional vars: Dep and arr locations 
qplot(ArrDelay, DepDelay, data = AQ) +
  xlim(-70, 1030) + ylim(-65, 1025) + theme_bw() +
  facet_trelliscope(~ Origin + Dest, nrow=2, ncol=4, as_plotly = TRUE)
# Sorting by mean ArrDelay and DepDelay identifies which airports are more punctual
# Can Filter by Distance/AirTime since we know that is related to delay times
# So you can compare the delays for short and long flights separately
# A little slow at first, but not too bad once loaded. 
# Tooltips from Plotly work well.

# Conditional vars: Day of week and Month
qplot(Distance, NetDelay, data = AQ) +
  xlim(60, 2600) + ylim(-45, 75) + theme_bw() +
  facet_trelliscope(~ Month + DayOfWeek, nrow=2, ncol=4, as_plotly = TRUE)
# Sorting by Netdelay_mean (descending) suggests that it may be day of week rather than month that effects the chance of longer delays
# Longer delays tend to be on Thurs

