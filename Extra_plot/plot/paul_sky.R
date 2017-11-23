library(ggplot2)

# 30001 and 30002 (Downtown to Airport Via Mt Eden Rd)
# 30003 and 30004 (Downtown to International Airport Via Mt Eden Rd)
# 30009 and 30010 (380 Queen Street to Airport Via Mt Eden) between 7am to 6pm

# 30005 and 30006 (Downtown to Airport Via Dominion Rd)
# 30007 and 30008 (Downtown to International Airport Via Dominion)
# 30011 and 30012 (380 Queen Street to Airport Via Dominion)

# Distance from city info
distance <- data.frame(stop = c("7018", "1464", "7060", "7062", "7164", "7168",
                                "8502", "8512", "8516", "8524", "8532", "2006"), 
                       dist = c(0, 0.65, 2.65, 3.05, 3.65, 4.1, 4.7, 6.3, 6.75, 
                                7.95, 9.15, 21.65))
eden_to_air <- c("Customs St East / Mercure Hotel", "64 Hobson St", "380 Queen St", "490 Queen St",
               "Symonds St overbridge", "Opposite 157 Symonds St", "Mt Eden Rd train station", 
               "Opposite 459 Mt Eden Rd", "588 Mt Eden Rd", "822 Mt Eden Rd", "Three Kings School",
               "AKL Airport")

# Function to retrieve data for each trip by route ID
storeRoutes <- function(tagNo) {
  df <- read.csv(paste0('bus-route-', tagNo, ".csv"), 
                 col.names = c("trip", "route", "vehicle", "departure", "arrival", "stop_sequence", "stop_id", "timestamp"))
  d <- df[!duplicated(df), ]
  # make trip id:
  d$trip <- gsub("-.*", "", d$trip)
  # make route id:
  d$route <- gsub("-.*", "", d$route)
  d$time <- as.POSIXct(d[, "timestamp"], origin = "1970-01-01")
  d$hours <- as.numeric(d$time - trunc(d$time, "days"))
  d$stop <- as.factor(d$stop_id)
  #d_dist <- merge(d, distance)
  #single_route <- d_dist[, c("trip", "route", "vehicle", "stop_sequence", "stop_id", "stop", "dist", "time", "hours")]
  #return(single_route)
  return(d)
}

t1_dist <- merge(t1, distance)

# Function to create plot
bus_graphic <- function (df, bus_stops) {
  ggplot(df, aes(x=hours, y=dist)) +
    geom_line(aes(group=trip), size=0.8, colour="red") +
    theme_bw() +
    scale_y_continuous(name=NULL, breaks=distance$dist, labels=bus_stops, expand=c(0, 0), 
                       sec.axis=sec_axis(trans=~., breaks=derive(), labels=derive())) +
    scale_x_continuous(name=NULL, breaks=seq(5, 24, 1), minor_breaks=seq(5, 24, 0.25), 
                       limits=c(5, 24), expand=c(0, 0), labels=c(5:11, "MIDI", 1:11, "MINUIT"),
                       sec.axis=sec_axis(trans=~., breaks=derive(), labels=derive())) +
    theme(panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_line(colour="grey30"),
          axis.text.y.right=element_text(colour="red"),
          panel.grid.major.x=element_line(colour="grey60"),
          panel.grid.minor.x=element_line(colour="grey60"),
          panel.border=element_blank(),
          axis.ticks=element_blank())
}

# Do on a single day: 3rd to 7th are weekdays.
# Take Wed 5th April: Take all of Mt Eden.
t1 <- storeRoutes("30001-20170405") # from airport
t2 <- storeRoutes("30002-20170405") # to
t3 <- storeRoutes("30003-20170405") # from airport
t4 <- storeRoutes("30004-20170405") # to
t5 <- storeRoutes("30009-20170405") # from airport
t6 <- storeRoutes("30010-20170405") # to airport

bus_graphic(t1_dist, eden_to_air)
