# merge agency and routes together:
agency <- read.csv('agency.txt', header = TRUE)
routes <- read.csv('routes.txt', header = TRUE)

info <- merge(routes, agency, by = "agency_id")
info$route_id <- gsub("-.*", "", info$route_id)
# find airport routes with skybus:
sky <- info[which(info$agency_name == "SkyBus"), ]
info[which(info$route_id == "30012"), ]
# 380 Queen St to Airport via Mt Eden Rd #30010
# Airport to 380 Queen St Via Dominion Rd #30011
sky[, c("route_long_name", "route_id")]

# 30001 and 30002 (Downtown to Airport Via Mt Eden Rd)
# 30003 and 30004 (Downtown to Inernational Airport Via Mt Eden Rd)
# 30009 and 30010 (380 Queen Street to Airport Via Mt Eden) between 7am to 6pm

# 30005 and 30006 (Downtown to Airport Via Dominion Rd)
# 30007 and 30008 (Downtown to International Airport Via Dominion)
# 30011 and 30012 (380 Queen Street to Airport Via Dominion)

# to copy csv files:
scp -r ysoh286@stats769prd01.its.auckland.ac.nz:~/SONAS_HOME/Documents/plot ~/Dropbox/plot

# subset by grep:
grep -h 30003 /course/AT/BUSDATA/trip_updates_201704* > bus-route-30003.csv

grep -h 30002 /course/AT/BUSDATA/trip_updates_* > bus-routes-30002.csv

# by day:
grep --no-filename ',6215,' /course/AT/BUSDATA/trip_updates_20170401*.csv > bus-route-6215.csv
# april:
april <- read.csv('bus-route-6215.csv', header = FALSE)

# get rid of duplicates:
tt <- april[!duplicated(april), ]
# convert timestamps:
tt$time <- as.POSIXct(tt[, "timestamp"], origin = "1970-01-01")
# separate by trip:
X <- split(tt, tt$trip)

# 2nd of April:
grep --no-filename ',6215,' /course/AT/BUSDATA/trip_updates_20170401*.csv > bus-route-6215-ap2.csv
april2 <- read.csv('bus-route-6215-ap2.csv', header = FALSE)
colnames(april2) <- c("trip", "route", "vehicle", "departure",
                     "arrival", "stop_sequence", "stop_id", "timestamp")
# get rid of duplicates:
tt2 <- april2[!duplicated(april2), ]
# convert timestamps:
tt2$time <- as.POSIXct(tt2[, "timestamp"], origin = "1970-01-01")
# separate by trip:
X2 <- split(tt2, tt2$trip)

#--------------------------------------------------------------------------------------------------
# Trying with routes instead since vehicle no has different routes!
grep --no-filename ',30002-' /course/AT/BUSDATA/trip_updates_20170401*.csv > bus-route-30002.csv
a3 <- read.csv('bus-route-30002.csv', col.names = c("trip", "route", "vehicle", "departure",
                                                   "arrival", "stop_sequence", "stop_id", "timestamp"))
a <- a3[!duplicated(a3), ]
a$time <- as.POSIXct(a[, "timestamp"], origin = "1970-01-01")

# separate by trip:
X <- split(a, a$trip)

extractRoute <- function(routeNo, date) {
  command <- paste0("grep --no-filename ',", routeNo, "-' /course/AT/BUSDATA/trip_updates_", date, "*.csv > bus-route-",  routeNo, "-", date, ".csv")
  system(command)
}

# routes: SKYBUS:
# 30001, 30002, 30003, 30004, 30005, 30006, 30007, 30008, 30009, 30010, 30011, 30012


storeRoutes <- function(tagNo) {
  df <- read.csv(paste0('bus-route-', tagNo, ".csv"), col.names = c("trip", "route", "vehicle", "departure",
                                                                         "arrival", "stop_sequence", "stop_id", "timestamp"))
  d <- df[!duplicated(df), ]
  # make trip id:
  d$trip <- gsub("-.*", "", d$trip)
  # make route id:
  d$route <- gsub("-.*", "", d$route)
  d$time <- as.POSIXct(d[, "timestamp"], origin = "1970-01-01")
  d$hours <- as.numeric(d$time - trunc(d$time, "days"))
  #calculate time diff:
  #timeDiff <- round(ifelse(is.na(d$departure), 0, d$departure) + ifelse(is.na(d$arrival), 0, d$arrival), 3)
  #d$tt <- d$time + timeDiff
  d <- d[, c("trip", "route", "vehicle", "departure", "arrival", "stop_sequence", "stop_id", "time", "hours", "timestamp")]
  return(d)
}


# trial plot:
par(las = 1)

# trial plot:
generatePlot <- function(df) {
  A <- split(df, df$trip)
  plot(NULL, xaxt = "n", xlim = range(df$tt), ylim = range(df$stop_sequence),
      xlab = "Time", ylab = "Stop")
  #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "black")
  abline(h = df$stop_sequence, v = df$tt, col = rgb(211/255, 211/255, 211/255, 0.4))
  axis.POSIXct(side=1, at=cut(df$tt, "hours"), format="%H:%M")
  for (i in 1:length(A)) {
    trial <- A[i]
    df <- trial[[1]]
    df <- df[order(df$stop_sequence), ]
    tf <- df[, c("tt", "stop_sequence")]
    points(tf, pch = 16, col = "blue", type = "l")
  }
}



# 30001 and 30002 (Downtown to Airport Via Mt Eden Rd)
# 30003 and 30004 (Downtown to International Airport Via Mt Eden Rd)
# 30009 and 30010 (380 Queen Street to Airport Via Mt Eden)

# Do on a single day: 3rd to 7th are weekdays.
# Take Wed 5th April: Take all of Mt Eden.
t1 <- storeRoutes("30001-20170405") # from
t2 <- storeRoutes("30002-20170405") # to
t3 <- storeRoutes("30003-20170405") # from
t4 <- storeRoutes("30004-20170405") # to
t5 <- storeRoutes("30009-20170405") # from airport
t6 <- storeRoutes("30010-20170405") # to airport

par(mfrow = c(1, 1))
generatePlot(t1)
generatePlot(t2)
generatePlot(t3)
generatePlot(t4)
generatePlot(t5)
generatePlot(t6)


# change from Airport - stop sequence needs to be reversed?? that's t5.

# create new stop information:
stops <- read.csv('stops.txt')
# reverse the stops for to airport.
reversedStop <- stops[11:1, ]
reversedStop$seq <- 1:11
reversedStop
t5a <- merge(t5, reversedStop, by = "stop_id")
A <- split(t5a, t5a$trip)


# plot on top of each other:
par(las = 1, mar = c(5.1, 10, 2.1, 2.1), yaxs = "i")
df <- t6
A <- split(df, df$trip)
plot(NULL, xaxt = "n", xlim = range(df$tt), ylim = range(df$stop_sequence),
     yaxt = "n", xlab = "", ylab = "")
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "black")
abline(h = df$stop_sequence, v = seq(min(df$tt, "hours"), max(df$tt), length = 4*24), col = rgb(211/255, 211/255, 211/255, 0.4))
axis.POSIXct(side=1, at=cut(df$tt, "hours"), format="%H:%M", tick = FALSE)
title()
for (i in 1:length(A)) {
  trial <- A[i]
  df <- trial[[1]]
  df <- df[order(df$stop_sequence), ]
  tf <- df[, c("tt", "stop_sequence")]
  points(tf, pch = 16, col = "blue", type = "l")
}

# replot 'from airport' route:
t5a <- merge(t5, reversedStop, by = "stop_id")
df <- t5a
A <- split(df, df$trip)
axis(side = 2, at = 1:nrow(stops), label = rev(stops$stop_name), tick = FALSE)


for (i in 1:length(A)) {
  trial <- A[i]
  df <- trial[[1]]
  df <- df[order(df$stop_sequence), ]
  tf <- df[, c("tt", "seq")]
  points(tf, pch = 16, col = "red", type = "l")
}


## 
a1 <- storeRoutes('30001-20170405')
a2 <- storeRoutes('30002-20170405')
a3 <- storeRoutes('30009-20170405')
a4 <- storeRoutes('30010-20170405')
a5 <- storeRoutes('30003-20170405')
a6 <- storeRoutes('30004-20170405')

A <- split(a1, a1$trip)

#-------------------------------- ATTEMPT 3 ------------------------------
# do it across lists:
duplicateAD <- function(x) {
  ones <- x[which(table(x$stop_sequence) == 1), ]
  #dep <- ifelse(is.na(ones$departure), 0, ones$departure)
  #arr <- ifelse(is.na(ones$arrival), 0, ones$arrival)
  #ones$tt <- ones$tt - dep - arr
  ones$arrival <- ifelse(is.na(ones$arrival), 0, NA)
  ones$departure <- ifelse(is.na(ones$departure), 0, NA)
  #add to thing:
  dd <- rbind(x, ones)
  dd <- dd[order(dd$stop_sequence, dd$tt), ]
  invisible(dd)
}

tt <- split(a1, a1$trip)
lapply(tt, duplicateAD)

#plot the lines
plotLine <- function(x, col) {
  tf <- x[, c("tt", "stop_sequence")]
  points(tf, pch = 16, col = col, type = "l")
  invisible(NULL)
}

# TODO: what's my time label? 2017-04-05 from 5am till 12am.
five <- as.POSIXct(1491325200, origin = "1970-01-01")
twelve <- as.POSIXct(1491393600, origin = "1970-01-01")

# new generatePlot function
startPlot <- function(df) {
  A <- split(df, df$trip)
  orderedList <- lapply(A, duplicateAD)
  plot(NULL, xaxt = "n", xlim = range(df$tt), ylim = range(df$stop_sequence),
       xlab = "Time", ylab = "Stop")
  #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "black")
  abline(h = df$stop_sequence, v = df$tt, col = rgb(211/255, 211/255, 211/255, 0.4))
  axis.POSIXct(side=1, at=cut(df$tt, "hours"), format="%H:%M")
  lapply(orderedList, plotLine, col = "blue")
  invisible(NULL)
}

# set some plot parameters
par(las = 1, mar = c(5.1, 10, 2.1, 2.1), yaxs = "i")
startPlot(a1)
# add routes to plot
addPlotRoute <- function(df, col = "black") {
  A <- split(df, df$trip)
  orderedList <- lapply(A, duplicateAD)
  lapply(orderedList, plotLine, col = col)
  invisible(NULL)
}

addPlotRoute(a2)
addPlotRoute(a3, "red")
addPlotRoute(a4, "pink")

# TODO: some are still going backwards.... :/

# extract out 2010 and leave just 2006 stopID - for 30002.
a2 <- a2[-which(a2$stop_id == "2010"), ]
startPlot(a2)

## do a ggplot2 version:
startPlot <- function(df) {
  A <- split(df, df$trip)
  orderedList <- lapply(A, duplicateAD)
  plot(NULL, xaxt = "n", xlim = range(df$tt), ylim = range(df$stop_sequence),
       xlab = "Time", ylab = "Stop")
  #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "black")
  abline(h = df$stop_sequence, v = df$tt, col = rgb(211/255, 211/255, 211/255, 0.4))
  axis.POSIXct(side=1, at=cut(df$tt, "hours"), format="%H:%M")
  lapply(orderedList, plotLine, col = "blue")
  invisible(NULL)
}

# need to rearrange.
A <- split(a2, a2$trip)
orderedList <- do.call(rbind, lapply(A, duplicateAD))
stuff <- orderedList[, c("trip", "stop_id", "stop_sequence", "tt")]

# link distances together (from Shan-I):
distance <- data.frame(stop_id = c("7018", "1464", "7060", "7062", "7164", "7168",
                                "8502", "8512", "8516", "8524", "8532", "2006"), 
                       dist = c(0, 0.65, 1.65, 1.95, 2.55, 2.8, 3.5, 5.3, 5.75, 
                                6.95, 8.15, 21.65))
trial <- merge(stuff, distance, by = "stop_id")

ggplot(trial, aes(x = tt, y = dist)) + geom_line(aes(group = trip)) +
      labs(y = "Distance from city") + theme_bw() + 
      scale_x_datetime(breaks = timeInt) + scale_y_continuous(minor_breaks = distance$dist)
timeInt <- seq(five, twelve, by = 10*60)
