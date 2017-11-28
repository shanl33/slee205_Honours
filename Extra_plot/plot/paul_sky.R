library(ggplot2)
# For checking trips
# library(plotly)

# Even route number is 'to airport' and odd is 'from aiport'
# 30001 and 30002 (Downtown - Airport Via Mt Eden Rd)
# 30003 and 30004 (Downtown - International Airport Via Mt Eden Rd)
# 30009 and 30010 (380 Queen Street - Airport Via Mt Eden) between 7am to 6pm

# 30005 and 30006 (Downtown - Airport Via Dominion Rd)
# 30007 and 30008 (Downtown - International Airport Via Dominion)
# 30011 and 30012 (380 Queen Street - Airport Via Dominion)

# Distance from city info
distance <- data.frame(stop = c("7018", "7047", "7053", "1464", "7060", "7062", "7147", 
                                "7164", "7149", "7168", "7151", "8502", "8503", "8512", "8513",
                                "8516", "8517", "8524", "8523", "8532", "8533", "2006",
                                "8410", "8418", "8428", "7063", "8407", "8419", "8431"), 
                       dist = c(0, 0.69, 1.15, 1.15, 2.60, 3.04, 3.04,
                                3.6, 3.6, 4.1, 4.1, 4.7, 4.7, 6.3, 6.3,
                                6.75, 6.75, 7.95, 7.95, 9.15, 9.15, 20,
                                4.6, 5.7, 7.5, 3.04, 4.6, 5.7, 7.5))
eden_to_air <- c("Customs St East\n/ Mercure Hotel", "", "64 Hobson St",
                 "380 Queen St", "490 Queen St", "Symonds St overbridge", "Opposite 157 Symonds St", "Mt Eden Rd train station", 
               "Opposite 459 Mt Eden Rd", "588 Mt Eden Rd", "822 Mt Eden Rd", "Three Kings School", "AKL Airport")

air_to_eden <- c("Customs St East", "99 Queen St", "237 Queen St",
                 "380 Queen St", "25 Symonds St", "83 Symonds St", "157 Symonds St", "Mt Eden Rd train station", 
                 "415 Mt Eden Rd", "581 Mt Eden Rd", "909 Mt Eden Rd", "Three Kings Plaza", "AKL Airport")

dom_to_air <- c("Customs St East\n/ Mercure Hotel", "", "64 Hobson St",
                 "380 Queen St", "490 Queen St", "Symonds St overbridge", "Opposite 157 Symonds St", 
                "270 Dominion Rd", "660 Dominion Rd", "1248 Dominion Rd", "AKL Airport")

air_to_dom <- c("Customs St East", "99 Queen St", "237 Queen St",
                 "380 Queen St", "421 Queen St", "", "",
                "215 Dominion Rd", "591 Dominion Rd", "993 Dominion Rd", "AKL Airport")


# Function to retrieve data for each trip by route ID
storeRoutes <- function(tagNo) {
  df <- read.csv(paste0('bus-route-', tagNo, ".csv"), 
                 col.names = c("trip", "route", "vehicle", "departure", "arrival", "stop_sequence", "stop_id", "timestamp"))
  d <- df[!duplicated(df), ]
  # make trip id:
  d$trip <- gsub("-.*", "", d$trip)
  d$trip <- as.factor(d$trip)
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

# If multiple timestamps for terminal stops then only retain earliest
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

# If no aiport (2006) stop (as start or final), impute with average travel time
impute_airport <- function(df_trip, df_route) {
  # Find the closest stop to airport with timestamp
  closest_stop <- max(levels(df_trip$stop)[summary(df_trip$stop)!=0])
  # Subset times from all trips on the same route (for closest_stop and airport stop)
  trips1 <- subset(df_route, df_route$stop==closest_stop, c("trip", "hours"))
  trips2 <- subset(df_route, df_route$stop=="2006", c("trip", "hours"))
  trips <- merge(trips1, trips2, "trip")
  max_time <- max(df_trip[df_trip$stop==closest_stop, "hours"])
  impute <- subset(df_trip, df_trip$hours==max_time)
  impute$hours <- impute$hours - mean(trips$hours.x - trips$hours.y)
  if (is.na(impute$hours) && df_trip$direction=="to_air") {
    # Impute 45 mins (minimum travel time) since missing for mostly off-peak times
    impute$hours <- min(df_trip$hours) + 0.75 
  } else if (is.na(impute$hours) && df_trip$direction=="from_air") {
    impute$hours <- max(df_trip$hours) - 0.75
  }
  impute$stop <- "2006"
  df_keep <- rbind(df_trip, impute)
  return(df_keep)
}

# Check terminal stop repeats and if imputation needed for airport stop
check_trips <- function(trip_tag, route_df) {
  t <- subset(route_df, route_df$trip==trip_tag)
  # Delete repeats
  if ("2006" %in% levels(t$stop)) {
    t1 <- retain_earliest(t, "2006")
  } else {
    t1 <- t
  }
  if ("7018" %in% levels(t1$stop)) {
    t2 <- retain_earliest(t1, "7018")
  } else {
    t2 <- t1
  }
  if ("7060" %in% levels(t2$stop)) {
    t3 <- retain_earliest(t2, "7060")
  } else {
    t3 <- t2
  }
  # Impute time for aiport stop if needed
  if (!("2006" %in% levels(t3$stop))) {
    t4 <- impute_airport(t3, route_df)
  } else if (summary(t3$stop)["2006"]==1) {
    t4 <- t3
  } else {
    t4 <- impute_airport(t3, route_df)
  }
  return(t4)
}

storeRoutes2 <- function(tagNo) {
  df <- read.csv(paste0('bus-route-', tagNo, ".csv"), 
                 col.names = c("trip", "route", "vehicle", "departure", "arrival", "stop_sequence", "stop_id", "timestamp"))
  d <- df[!duplicated(df), ]
  # make trip id:
  d$trip <- gsub("-.*", "", d$trip)
  d$trip <- as.factor(d$trip)
  # make route id:
  d$route <- gsub("-.*", "", d$route)
  d$time <- as.POSIXct(d[, "timestamp"], origin = "1970-01-01")
  d$hours <- as.numeric(d$time - trunc(d$time, "days"))
  d$stop <- as.factor(d$stop_id)
  # Even route number is 'to airport' and odd is 'from aiport'
  d$direction <- ifelse(as.integer(d$route)/2==trunc(as.integer(d$route)/2), "to_air", "from_air")
  # Tidy terminal stops for each trip
  trip_tags <- levels(d$trip)
  d_tidy <- do.call(rbind.data.frame, lapply(trip_tags, function(x) check_trips(x, d)))
  d_dist <- merge(d_tidy, distance)
  single_route <- d_dist[, c("trip", "route", "vehicle", "stop_sequence", "stop_id", 
                             "stop", "dist", "time", "hours", "direction")]
  return(single_route)
}

# Function to create plot
bus_graphic <- function (df, stops_to_air, air_to_stops, colour_from, colour_to, day) {
  ggplot(df, aes(x=hours, y=dist)) +
    geom_line(aes(group=trip, colour=direction), size=0.8, alpha=0.5) +
    theme_bw() +
    scale_color_manual(values=c(colour_from, colour_to), guide=FALSE) +
    scale_y_continuous(name=NULL, breaks=sort(unique(df$dist)), labels=stops_to_air, expand=c(0, 0), 
                       sec.axis=sec_axis(trans=~., breaks=derive(), labels=air_to_stops)) +
    scale_x_continuous(name=NULL, breaks=seq(5, 24, 1), minor_breaks=seq(5, 24, 0.25), 
                       limits=c(5, 25), expand=c(0, -1), labels=c(5:11, "MIDI", 1:11, "MINUIT"),
                       sec.axis=sec_axis(trans=~., breaks=derive(), labels=derive())) +
    labs(caption=paste("La SkyBus Graphique,", day)) +
    theme(panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_line(colour="grey50", size=0.3),
          axis.text.y.right=element_text(colour=colour_from),
          axis.text.y.left=element_text(colour=colour_to),
          axis.text.x.top=element_text(margin=margin(t=0.4, b=0.2, unit="cm")),
          axis.text.x.bottom=element_text(margin=margin(t=0.2, b=0.2, unit="cm")),
          panel.grid.major.x=element_line(colour="grey50", size=0.3),
          panel.grid.minor.x=element_line(colour="grey60"),
          panel.border=element_blank(),
          axis.ticks=element_blank(),
          plot.caption=element_text(family="Times", face="bold.italic" , colour="grey45", 
                                    margin=margin(t=0.1, b=0.3, unit="cm")))
}


# 6 possible routes for return trip and different time of day
# Dates chosen because they produced interesting trips!
# All SkyBus trips via Mt Eden Rd on Thursday 6th April 2017 
eden_routes <- lapply(c(1:4, 9), function(x) paste0("3000", x, "-20170406"))
eden_routes[[6]] <- "30010-20170406"
eden170406 <- do.call(rbind, lapply(eden_routes, storeRoutes2))
# All SkyBus trips via Dominion Rd on Wednesday 5th April 2017 
dom_routes <- lapply(5:8, function(x) paste0("3000", x, "-20170405"))
dom_routes <- c(dom_routes, lapply(11:12, function(x) paste0("300", x, "-20170405")))
dom170405 <- do.call(rbind, lapply(dom_routes, storeRoutes2))
  
# Plots
bus_graphic(eden170406, eden_to_air, air_to_eden, colour_from="red", colour_to="black", 
            day="jeudi 6 avril 2017")
bus_graphic(dom170405, dom_to_air, air_to_dom, colour_from="red", colour_to="blue",
            day="mercredi le 5 avril 2017")
