devtools::install_github("ropensci/plotly")
devtools::install_github("rstudio/crosstalk")
library(dplyr)
library(crosstalk)
library(plotly)

load("~/Desktop/Project/slee205_Honours/Datasets/AQ.RData")
AQ$NetDelay <- AQ$ArrDelay-AQ$DepDelay
AQ$rows <- rownames(AQ)
Sept <- AQ[AQ$Month==9,]
Aug <- AQ[AQ$Month==8,]

# Sept only
sd1 <- SharedData$new(Sept, key=~TailNum)
p1 <- ggplot(sd1, aes(x=Distance, y=NetDelay, color=TailNum, frame=DayofMonth)) +
  labs(title="September flights for AQ carrier") +
  geom_point()
ggplotly(p1) %>%
  layout(dragmode="select") 
# Can brush points using legend, but loses the rest of the points.
# Can brush multiple groups of points using legend (persists)
# Cannot Play animation if points selected from legend.
# Animation plays if selected on-plot by default but not sure if really accurate. 
# Purposely coding the persistent brushing may help it work better, see:
# https://cpsievert.github.io/plotly_book/linking-animated-views.html#fnref17
# Can still use R console but very slow.

# Aug and Sept
# Don't show axes labels for top plot
none <- list(title = "")
# sd1 from above (Sept)
p1 <- ggplot(sd1, aes(x=Distance, y=NetDelay, color=TailNum, frame=DayofMonth)) +
  ##labs(title="September flights for AQ carrier") +
  geom_point()
gp1 <- ggplotly(p1)
  # %>% layout(dragmode="select") 

# Aug flights
sd2 <- SharedData$new(Aug, key=~TailNum)
p2 <- ggplot(sd2, aes(x=Distance, y=NetDelay, color=TailNum, frame=DayofMonth)) +
  labs(title="Aug & Sep flights for AQ carrier") +
  geom_point() 
gp2 <- ggplotly(p2) %>%
  # hide_legend() %>%
  # layout(dragmode="select") %>%
  layout(xaxis=none, yaxis=none)
  

subplot(gp2, gp1, nrows = 2, titleY = T, titleX = T) %>%
  layout(dragmode="select") %>%
  animation_opts(frame=800)
  # highlight(off="plotly_deselect", persistent=T)

# Sometimes a point seems to be 'left behind'?
# Warning msg: Can only brush/highlight in one plot but seems ok (gets a bit stuck).
# Warning message about frames not matching in replacement length.