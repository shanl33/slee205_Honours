library(ggplot2)
library(plotly)
library(tourr)
library(abind)
library(MASS) # For crabs dataset
data("crabs")

# Sphere data
X_sphere <- sphere(crabs[,4:8])

#intial basis (scatterplot w first two vars)
t2 <- save_history(X_sphere, guided_tour(cmass, d=2, max.tries = 50), max=50)
t2[1,,]
t2[,1,]
t2[,,1]

# Retain orthogonal projection as initial basis
t0 <- t2[,,1]
t0[,,1] <- matrix(c(1, rep(0, 5), 1, rep(0, 3)), ncol = 1)
t3 <- abind(t2, t0, along = 3)
class(t3) <- "history_array"
t3[[1]]
str(t3)
str(t2)

tinterp <- interpolate(t3)

cmass_tour <- function(basis) {
  # Projected data matrix 
  XA <- X_sphere%*%matrix(basis, ncol=2) # (n by d) = (407 by 2)
  cmass_index <- (sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1]-exp(-dim(XA)[2]/2))/(1-exp(-dim(XA)[2]/2))
  list(rescale(XA), cmass_index)
}
# Apply function to each projection basis
t2_tour <- apply(tinterp, 3, FUN = cmass_tour) 
t2_tour[[i]][[1]][,1]

cmass_index2 <- data.frame(iteration=1:length(t2_tour))
for(i in 1:length(t2_tour)) {
  cmass_index2$index[i] <- unlist(t2_tour[[i]][2])
}

# Colour
group_t <- "Decile"
fac_t <- unlist(subset(ach_narm, select = group_t)) %>% unlist() %>% as.factor()
levels(fac_t)
pal_t <- rainbow_hcl(length(levels(fac_t))) # Compute a rainbow of colours (qualitative palette)
group_col_t <- as.factor(pal_t[as.numeric(fac_t)])
group_col2 <- data.frame(col=group_col_t, group=fac_t)

# projection axes
plot_ly(x = ~x, y = ~y, frame = ~step, hoverinfo = "none") %>%
  add_segments(xend = 0, yend = 0, color = I("gray85")) %>%
  add_text(text = ~measure, color = I("black")) %>%
  layout(xaxis = ax, yaxis = ax)

ggplot(proj, aes(x=V1, y=V2, label=ID, col=group_col)) +
  geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) 
ggplotly(hoverinfo = "none") 

# Using new_tour() function
X_sphere <- sphere(crabs[,4:8])
head(X_sphere)
tour <- new_tour(X_sphere, tour_path = guided_tour(cmass, d=2, max.tries = 50))
steps <- c(0, rep(1/15, 1000))
stepz <- cumsum(steps)
tail(steps)

tour_dat <- function(step_size) {
  step <- tour(step_size) #step is a list of 3 ($proj, $target, $step)
  # $proj basis is the current proj basis
  # $target basis stays the same for all tour(#)
  # $step is a cumulative counter of number of calls to the tour() fn
  print(step)
  proj <- center(X_sphere %*% step$proj) # Projected data matrix
  # df with projected x and y coordinates
  data.frame(x = proj[,1], y = proj[,2], Name = rownames(ncea01))
}