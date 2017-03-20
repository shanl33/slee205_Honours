# Read in data for Month, DayofWeek, Scheduled Departure and Arrival Times (CRSDepTime, CRSArrTime), 
# UniqueCarrier, ArrDelay, DepDelay, AirTime:
load("delay.RData")

# Scatterplot of DepDelay vs ArrDelay for all airlines takes a long time.
# Early flights are not plotted since using log scales (see Warnings).
# Good example of why not to export as pdf (compared to png).
par(mar = c(4, 4, 3, 2) + 0.1)
plot(delay$DepDelay, delay$ArrDelay, 
     main = "Flight delay for all Carriers",
     xlab = "Departure delay (mins)",
     ylab = "Arrival delay (mins)",
     las = TRUE,
     log = "xy")
abline(0, 1, col = "red")

# Alternative: Smoothed colour density representation of a scatterplot.
# Subset only positive values (exculde early flights) inorder to use log scale.
pos_delay <- delay[(delay$ArrDelay > 0) & (delay$DepDelay > 0),]
smoothScatter(pos_delay$DepDelay, pos_delay$ArrDelay, 
              main = "Flight delay for all Carriers", 
              xlab = "Departure delay (mins)",
              ylab = "Arrival delay (mins)", 
              log = "xy",
              las = 1,
              mgp = c(2.5, 1, 0))

# 1 to 10 mins delay are recorded to nearest minute so the smoothing looks strange (?).
# ggplot hexagonal binning may be better: stat_binhex()
######################################################################


# Subset smallest carrier, AQ:
load("AQ.RData")
# Compare the delay times (on arrival and departure):
plot(AQ$DepDelay,AQ$ArrDelay, main = "Flight delay times for AQ Carrier", 
     xlab = "Departure delay (mins)",
     ylab = "Arrival delay (mins)", 
     las = 1,
     log = "xy",
     xaxt = "n",
     yaxt = "n",
     mgp = c(2, 1, 0))
abline(0, 1, col = "red")
axis(1, at = c(1, 5, 15, 30, 60, 120, 180, 300, 1000), labels = c(1, 5, 15, 30, 60, 120, 180, 300, 1000),
     tcl = -0.2, padj = -1)
axis(2, at = c(1, 5, 15, 30, 60, 120, 180, 300, 1000), labels = c(1, 5, 15, 30, 60, 120, 180, 300, 1000),
     tcl = -0.2, las = 1, hadj = 0.6)
axis(3, at = c(1, 5, 15, 30, 60, 120, 180, 300, 1000), tcl = -0.2, labels = FALSE)
axis(4, at = c(1, 5, 15, 30, 60, 120, 180, 300, 1000), tcl = -0.2, labels = FALSE)

# Smoothed scatterplot for AQ carrier delay.
# Positive delay values only for log scales.
pos_AQ <- AQ[(AQ$DepDelay > 0) & (AQ$ArrDelay > 0),]
smoothScatter(pos_AQ$DepDelay, pos_AQ$ArrDelay,
              main = "Flight delay for AQ Carrier",
              xlab = "Depature delay (mins)",
              ylab = "Arrival delay (mins)",
              las = 1,
              log = "xy",
              mgp = c(2.5, 1, 0),
              nbin = 300)

library(lattice)
# Grid graphics, lattice package: Multipanel plot of flight delay by Month.
# Plot takes a long time if using all Carriers and too much overplotting to be of use.
# NetDelay (mins) created to measure if a flight 'made up' for its departure delay.
# NetDelay>0 means flight was delayed further, NetDelay<0 flight overcompensated for delay.
NetDelay <- AQ$ArrDelay-AQ$DepDelay
AQ$NetDelay <- NetDelay
summary(AQ$NetDelay)
xyplot(NetDelay ~ AirTime | factor(Month), data = AQ, 
       main = "Flight delay by Month for AQ Carrier",
       xlab = "Air time (mins)",
       ylab = "Net delay (mins)",
       scales = list(y = list(alternating = 1,
                              limits = c(-50, 80))),
       panel = function(...) {
         panel.xyplot(...)
         panel.abline(h=0, lty = "dashed")
       })
       
# Interactivity: Identification
# Select a single point and identify length of delay, Carrier, etc.
library(plotly)
AQ$Month <- as.factor(AQ$Month)
ggplot(AQ) +
  geom_point(aes(x=AirTime, y=NetDelay, color=Month)) +
  labs(title="Flight delays for AQ Carrier in 2001", 
       x="Air time (mins)",
       y="Net delay (mins)",
       color="Month")
# Add interaction:
ggplotly()
# scale_x_continous(): Transforming the axes
# Produces NaNs for log of negative values and will plot NaNs in static plots.
# Interaction: NaNs disappear from web browser plot and default tooltips display log values.
ggplot(AQ) +
  geom_point(aes(x=DepDelay, y=ArrDelay)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  geom_vline(aes(xintercept=1))
ggplotly()
# coord_trans(): Transforming the coordinates
# Results in log scales axes but with different ticks labeled.
# Data set must be all positive values.
# Interaction: Plot completely changed and tooltip is again log values.
ggplot(pos_AQ) +
  geom_point(aes(x=DepDelay, y=ArrDelay)) +
  coord_trans(x="log10", y="log10")
ggplotly()
# Interactivity: Brushing and linking
# Linking a mosaic graph (with Day of week and month) and scatterplot of delay.