load("flights.RData")

# Proportion of flights cancelled by month and carrier:
cancel <- aggregate(flights$Cancelled,
          by=list(Month=flights$Month, 
                  Carrier=flights$UniqueCarrier),
          FUN=mean)
plot(cancel[cancel$Carrier=="AA", 1], cancel[cancel$Carrier=="AA", 3],
     ylim = range(cancel$x), type = "o",
     main = "Flight cancellations by Carrier",
     xlab = "Month",
     ylab = "Proportion of flights cancelled",
     las = 1)
# Low-level functions to add on graphs for other carriers:
points(cancel[cancel$Carrier=="AQ", 1], cancel[cancel$Carrier=="AQ", 3],
       pch = 2, col = "blue")
lines(cancel[cancel$Carrier=="AQ", 1], cancel[cancel$Carrier=="AQ", 3],
      col = "blue")
# Etc. Legend needed. Better to use lattice grid package.


library(lattice)
# Grid graphics, lattice package: 
# Time series of Cancelled flights every Month by Carrier groups
xyplot(x ~ Month, data = cancel, 
       group = Carrier, 
       type = "l", 
       auto.key = list(space = "right"), 
       scales = list(x = list(at = 1:12,
                              labels = c("Jan", "Feb", "Mar", "Apr","May", "Jun",
                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))),
       main = "Flight cancellation by Carrier",
       xlab = "Month",
       ylab = "Proportion of cancelled flights")

# Grid graphics, lattice package: Parallel plot
# Too much overplotting when using all flights and takes too long:
parallelplot(~flights[12:16], data = flights, 
             groups = Month,
             auto.key = list(space = "right"))
# AQ carrier subset:
load("AQ.RData")
parallelplot(~AQ[12:16], data = AQ,
             groups = Month,
             auto.key = list(space = "right"),
             main = "AQ Carrier flights Jan to Oct 2001")

# Parallel plot for Shiny interaction (by Month).
# May be more useful with log scales for ArrDelay and DepDelay (?)
parallelplot(AQ[AQ$Month == 1, 14:16],
             groups = AQ$DayOfWeek,
             auto.key = list(space = "right"),
             main = "AQ Carrier flights in 2001")
