library(ggplot2)
library(plotly)

# 30001 and 30002 (Downtown to Airport Via Mt Eden Rd)
# 30003 and 30004 (Downtown to International Airport Via Mt Eden Rd)
# 30009 and 30010 (380 Queen Street to Airport Via Mt Eden) between 7am to 6pm

# 30005 and 30006 (Downtown to Airport Via Dominion Rd)
# 30007 and 30008 (Downtown to International Airport Via Dominion)
# 30011 and 30012 (380 Queen Street to Airport Via Dominion)

# Distance from city info
distance <- data.frame(stop = c("7018", "7047", "7053", "1464", "7060", "7062", "7147", 
                                "7164", "7149", "7168", "7151", "8502", "8503", "8512", "8513",
                                "8516", "8517", "8524", "8523", "8532", "8533", "2006"), 
                       dist = c(0, 0.69, 1.15, 1.15, 2.60, 3.04, 3.04,
                                3.65, 3.65, 4.1, 4.1, 4.7, 4.7, 6.3, 6.3,
                                6.75, 6.75, 7.95, 7.95, 9.15, 9.15, 20))
eden_to_air <- c("Customs St East\n/ Mercure Hotel", "", "64 Hobson St",
                 "380 Queen St", "490 Queen St", "Symonds St overbridge", "Opposite 157 Symonds St", "Mt Eden Rd train station", 
               "Opposite 459 Mt Eden Rd", "588 Mt Eden Rd", "822 Mt Eden Rd", "Three Kings School", "AKL Airport")

air_to_eden <- c("Customs St East", "99 Queen St", "237 Queen St",
                 "380 Queen St", "25 Symonds St", "83 Symonds St", "157 Symonds St", "Mt Eden Rd train station", 
                 "415 Mt Eden Rd", "581 Mt Eden Rd", "909 Mt Eden Rd", "Three Kings Plaza", "AKL Airport")

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
  # Even route number is 'to airport' and odd is 'from aiport'
  d$direction <- ifelse(as.integer(d$route)/2==trunc(as.integer(d$route)/2), "to_air", "from_air")
  d_dist <- merge(d, distance)
  single_route <- d_dist[, c("trip", "route", "vehicle", "stop_sequence", "stop_id", 
                             "stop", "dist", "time", "hours", "direction")]
  return(single_route)
}

# Function to create plot
bus_graphic <- function (df, stops_to_air, air_to_stops) {
  ggplot(df, aes(x=hours, y=dist)) +
    geom_line(aes(group=trip, colour=direction), size=0.6) +
    theme_bw() +
    scale_color_manual(values=c("red", "blue"), guide=FALSE) +
    scale_y_continuous(name=NULL, breaks=unique(distance$dist), labels=stops_to_air, expand=c(0, 0), 
                       sec.axis=sec_axis(trans=~., breaks=derive(), labels=air_to_stops)) +
    scale_x_continuous(name=NULL, breaks=seq(5, 24, 1), minor_breaks=seq(5, 24, 0.25), 
                       limits=c(5.75, 24), expand=c(0, 0), labels=c(5:11, "MIDI", 1:11, "MINUIT"),
                       sec.axis=sec_axis(trans=~., breaks=derive(), labels=derive())) +
    theme(panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_line(colour="grey30", size=0.3),
          axis.text.y.right=element_text(colour="red"),
          axis.text.y.left=element_text(colour="blue"),
          axis.text.x.top=element_text(margin=margin(t=5, b=5)),
          axis.text.x.bottom=element_text(margin=margin(t=5, b=5)),
          panel.grid.major.x=element_line(colour="grey30", size=0.3),
          panel.grid.minor.x=element_line(colour="grey60"),
          panel.border=element_blank(),
          axis.ticks=element_blank())
}

## retain_earliest works but something wrong with the impute_airport function
## Maybe easier to apply the fns to eden_050417 df rather than each route's tag file

## Works

# Check start and final stops
# If multiple timestamps for stops 7018 or 2006 then only retain earliest
retain_earliest <- function(df_trip, repeated_stop) {
  if (summary(df_trip$stop)[repeated_stop]>1) {
    other_stops <- subset(df_trip, df_trip$stop!=repeated_stop)
    repeated <- subset(df_trip, df_trip$stop==repeated_stop)
    retain_first <- repeated[repeated$hours==min(repeated$hours), ]
    df_keep <- rbind(other_stops, retain_first)
  } else {
    df_keep <- df_trip
  }
  return(df_keep)
}

## Doesn't work
# Check terminal stops for each trip (doesn't work)
d$trip <- as.factor(d$trip)
trips <- levels(d$trip)
final_d <- do.call(rbind, lapply(trips, function(x) check_trips(x, d)))

# If no aiport (2006) stop (as start or final), impute with average travel time
impute_airport <- function(df_trip, df_route) {
  if (summary(df_trip$stop)["2006"]==0) {
    # Find the closest stop to airport with timestamp
    closest_stop <- max(levels(df_trip$stop)[summary(df_trip$stop)!=0])
    # Subset times from all trips on the same route (for closest_stop and airport stop)
    trips1 <- subset(df_route, df_route$stop==closest_stop, c("trip", "hours"))
    trips2 <- subset(df_route, df_route$stop=="2006", c("trip", "hours"))
    trips <- merge(trips1, trips2, "trip")
    max_time <- max(df_trip[df_trip$stop==closest_stop, "hours"])
    impute <- subset(df_trip, df_trip$hours==max_time)
    impute$hours <- impute$hours - mean(trips$hours.x - trips$hours.y)
    impute$stop <- "2006"
    impute$dist <- 20
    df_keep <- rbind(df_trip, impute)
  } else {
    df_keep <- df_trip
  }
    return(df_keep)
}
## Doesn't work (check_repeats previously worked, but changed it)
check_trips <- function(trip_tag, route_df) {
  df <- route_df[route_df$trip==trip_tag, ]
  # Impute average time if no timestamp for Aiport stop
  df1 <- impute_airport(df, route_df)
  # Omit stationary buses at terminal points of trips
  df2 <- retain_earliest(df1, "2006") # Airport
  # Customs St / Mecure Hotels
  df3 <- ifelse("7018" %in% levels(df2$stop), retain_earliest(df1, "7018"), df2) 
  # 380 Queen St
  df4 <- ifelse("7060" %in% levels(df2$stop), retain_earliest(df1, "7060"), df3)
  return(df4)
}

# Test
t2$trip <- as.factor(t2$trip)
t2_trips <- levels(t2$trip)
test_tripdf <- t2[t2$trip==t2_trips[8], ]
test_impute <- impute_airport(test_tripdf, t2)



# All SkyBus trips via Mt Eden Rd on Wed 5 April 2017 
# 6 possible routes for return trip and different time of day
eden_routes <- lapply(c(1:4, 9), function(x) paste0("3000", x, "-20170405"))
eden_routes[[6]] <- "30010-20170405"
eden_050417 <- do.call(rbind, lapply(eden_routes, storeRoutes))

# This worked!!!
test_route1 <- check_repeats(eden_050417, "2006") 
test_route2 <- check_repeats(test_route1, "7018")
test_route3 <- check_repeats(test_route2, "7060")

# Plot
bus_graphic(eden_050417, eden_to_air, air_to_eden)
bus_graphic(t2, eden_to_air, air_to_eden)
ggplotly()


# Testing
# Bus status updates SkyBus via Mt Eden for Wed 5th April
t1 <- storeRoutes("30001-20170405") # from airport
t2 <- storeRoutes("30002-20170405") # to
t3 <- storeRoutes("30003-20170405") # from airport
t4 <- storeRoutes("30004-20170405") # to
t5 <- storeRoutes("30009-20170405") # from airport
t6 <- storeRoutes("30010-20170405") # to airport



# Missing airport dept time (trips 10, 13)
lapply(t1_trips, function(x) summary(t1_dist[t1_dist$trip==x, "stop"]))
lapply(t1_trips, function(x) summary(t1_dist[t1_dist$trip==x, "stop"])["2006"]==0)




# 'From airport' routes must start at the airport
# Impute an estimated departure time if no status update