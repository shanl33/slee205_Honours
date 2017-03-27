# Read in subset of flights 2001 data.
load("~/Desktop/Project/slee205_Honours/Datasets/cancel.RData")
str(cancel)
# day_cancel is a summary of the proportion of cancel cancelled by day.
day_cancel <- tapply(cancel$Cancelled, cancel$DayOfWeek, mean)

# Which day of the week had more cancelled cancel?
midpts <- barplot(day_cancel)
width <- diff(midpts[1:2])/4
left <- rep(midpts, 4) - width
right <- rep(midpts, 4) + width
heights <- seq(0.01, 0.04, by = 0.01)
barplot(day_cancel, main = "Flight cancellations by Day of week", 
        xlab = "Day of week", ylab = "Proportion of cancel cancelled", 
        las = 1, xaxt = "n", yaxp = c(0, 0.05, 5))
axis(1, at = midpts, 
     labels = c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"), 
     tick = 0)
segments(left, heights, right, heights, col = "white")

# Interactivity: Select bar and proportion of cancelled cancel appears.
library(plotly) 
df_cancel <- as.data.frame(day_cancel)
df_cancel$day <- 1:7
tot_flights <- xtabs(~DayOfWeek, data = cancel)
df_cancel$tot_flights <- as.vector(tot_flights)
plot_ly(df_cancel, x = ~day, y = ~day_cancel, 
        type = "bar", color = ~tot_flights)
# Can't seem to add title, axes labels easily (?) using same syntax as base graphics.
# Low-level functions do not apply to plots in "Viewer" (rather than "Plots") window.

# ggplot2 automatically loaded with plotly.
ggplot(df_cancel) +
  geom_bar(aes(x=day, y=day_cancel, fill=tot_flights), 
           stat="identity") +
  labs(title="Cancelled flights in 2001",
       x="Day of week",
       y="Proportion of flights cancelled",
       fill="# of scheduled flights") +
  scale_x_discrete(limits= 1:7,
                   labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
# Make interactive via plotly: Tooltip not working for bar plot??? 
ggplotly()
# Maybe the values in the aes variables have too many digits?
# Tried ggplotly(tooltip = "x") but also does not work.
load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$SchedDepTime <- strptime(AQ$CRSDepTime, format = "%H%M")
ggplot(AQ) +
  geom_histogram(aes(x=SchedDepTime), binwidth = 1800) +
  labs(x="Scheduled departure time (hh:mm)")
ggplotly()
