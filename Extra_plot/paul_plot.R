library(ggplot2)

bus6215 <- read.csv("Files/bus-6215-2017-04-01.csv", header=FALSE)
colnames(bus6215) <- c("trip", "route", "vehicle", "departure", "arrival", "stop_sequence", "stop_id", "timestamp")
bus6215$stop <- as.factor(bus6215$stop_id)

# Distance from city
distance <- data.frame(stop = c("7018", "1464", "7060", "7062", "7164", "7168",
                                "8502", "8512", "8516", "8524", "8532", "2006"), 
                       dist = c(0, 0.65, 1.65, 1.95, 2.55, 2.8, 3.5, 5.3, 5.75, 
                                6.95, 8.15, 21.65))
bus_stops <- c("Customs St East (Mercure Hotel)", "64 Hobson St", "380 Queen St", "490 Queen St",
               "Symonds St overbridge", "Opposite 157 Symonds St", "Mt Eden Rd by train station", 
               "Opposite 459 Mt Eden Rd", "588 Mt Eden Rd", "822 Mt Eden Rd", "Three Kings School",
               "AKL Airport")

# Subset route 30002 (City to Airport via Mt Eden)
airport <- bus6215[grep("^30002-*", bus6215$route), ]
# Only 2 trips from city to airport via Mt Eden by bus6215 on this day
# Delete duplicates
uniqueairport <- airport[!duplicated(airport), ]
# Merge with distance info
airport_dist <- merge(uniqueairport, distance)
# Plot (I need to add more details for y-axis, labels etc..)
# Colour just for testing
ggplot(airport_dist, aes(x=timestamp, y=dist)) +
  geom_line(aes(group=trip)) +
  theme_bw() +
  scale_y_continuous(name=NULL, breaks=distance$dist, labels=bus_stops, expand=c(0, 0)) +
  #scale_x_continuous(name=NULL, breaks=seq(0, 24, 1), minor_breaks=seq(0, 24, 1/6), expand=c(0, 0)) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(colour="grey30"),
        panel.border=element_blank(),
        axis.ticks=element_blank())

# Ideas/To do's:
## 1. Possible format for x's: Time in hours from midnight
# a. Strip the times from the file names. 
# (This code is what my group did in Lab2, we had the recorded the file name from ls)
times <- substring(sizes$name, 22, 27)
# b. Define a function to convert a time string into a numeric hour value.
read_time <- function(time) {
  # Split string into pairs of characters, and weight each pair according to hours, minutes, or seconds.
  starts <- seq(1, nchar(time) - 1, 2)
  split <- substring(time, first = starts, last = starts + 1)
  result <- as.integer(split) %*% c(1, 1 / 60, 1 / 3600)
  return(as.numeric(result))
}
