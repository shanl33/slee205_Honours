# Read-in full data set.
load("flights.RData") 

# Which day of the week had more cancelled flights?
day_cancel <- tapply(flights$Cancelled, flights$DayOfWeek, mean)
midpts <- barplot(day_cancel)
width <- diff(midpts[1:2])/4
left <- rep(midpts, 4) - width
right <- rep(midpts, 4) + width
heights <- seq(0.01, 0.04, by = 0.01)
barplot(day_cancel, main = "Flight cancellations by Day of week", 
        xlab = "Day of week", ylab = "Proportion of flights cancelled", 
        las = 1, xaxt = "n", yaxp = c(0, 0.05, 5))
axis(1, at = midpts, 
     labels = c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"), 
     tick = 0)
segments(left, heights, right, heights, col = "white")

# Interactivity: Select bar and proportion of cancelled flights appears.

