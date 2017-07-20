library(ggplot2)
library(plotly)
library(tourr)
library(abind)
library(MASS) # For crabs dataset

data("crabs")
str(crabs)

# Brush to select by group ------------------------------------------------
# Mosaic plot (would need shiny click?) or facet_wrap of scatterplots (crosstalk only)
# facet_wrap would allow up to 4 factors to be crossed into groups
# Subset categorical vars
fac_cols <- sapply(crabs, class)=="factor"
Fdataset <- crabs[, fac_cols]
fac_n <- length(Fdataset)
Fdataset$Names <- rownames(crabs)
Fdataset$All <- factor(rep("1", length(Fdataset$Names)))
sd <- SharedData$new(data=Fdataset, key=~Names, group="2Dtour")
if (fac_n==1) {
  p <- ggplot(sd, aes(x=sd[,1], y=All)) +
    geom_jitter(width = 0.25, height = 0.25) + 
    labs(x=colnames(sd)[1])
} else if (fac_n==2) {
  p <- ggplot(sd, aes(x=sd[,1], y=sd[,2])) +
    geom_jitter(width = 0.25, height = 0.25) + 
    labs(x=colnames(sd)[1], y=colnames(sd)[2])
} else if (fac_n==3) {
  p <- ggplot(sd, aes(x=sd[,1], y=sd[,2])) +
    geom_jitter(width = 0.25, height = 0.25) + 
    labs(x=colnames(sd)[1], y=colnames(sd)[2]) +
    facet_grid(.~sd[,3])
} else if (fac_n==4) {
  p <- ggplot(sd, aes(x=sd[,1], y=sd[,2])) +
    geom_jitter(width = 0.25, height = 0.25) + 
    labs(x=colnames(sd)[1], y=colnames(sd)[2]) +
    facet_grid(sd[,4]~sd[,3])
} else if (fac_n>4) {
  p <- ggplot(sd, aes(x=sd[,1], y=sd[,2])) +
    geom_jitter(width = 0.25, height = 0.25) + 
    labs(x=colnames(sd)[1], y=colnames(sd)[2], 
         title="Only the first 4 factors have been displayed") +
    facet_grid(sd[,4]~sd[,3])
} else {
  p <- plotly_empty() # fac_n=0 No factors in dataset
}

# TO DO: 
#Use persistent brush
#need to make all plots plotly objects ie. ggplotly()

ggplot(Fdataset, aes(x=Fdataset[,1], y=All)) + 
  geom_jitter(width = 0.25, height = 0.25) + 
  labs(x=colnames(Fdataset)[1]) +
  facet_grid(.~Fdataset[,2])

ggplot(Fdataset, aes(x=Fdataset[,1], y=Fdataset[,2])) +
  geom_jitter(width = 0.25, height = 0.25) + 
  labs(x=colnames(Fdataset)[1], y=colnames(Fdataset)[2], 
       title="Only the first 4 factors have been displayed")


# Assgin SharedData as an object
d <- SharedData$new(m, ~rowname)
m[d$selection(),]

# Crosstalk n Plotly fn ---------------------------------------------------
# Start view as orthogonal projection of first two measures
# Retain orthogonal projection as initial basis
t0 <- t[,,1]
t0 <- matrix(c(1, rep(0, dim(Xdataset)[2]), 1, rep(0, (dim(Xdataset)[2]-2))), ncol = 2)
t1 <- abind(t0, t, along = 3)
class(t1) <- "history_array"
tinterp <- interpolate(t1) 
# Apply index function to each projection basis
pp_index <- data.frame(iteration=1:length(tinterp))
# 3rd component of tinterp is (1 by 2p) containing x-coeffs for each measurement var followed by y-coeffs
# matrix(3rd_comp, ncol=2) converts it into a projection matrix, A.
t_tour <- apply(tinterp, 3, FUN = cmass_tour) # pp index, projected x and y coords
p <- length(crabs[,4:8])
x <- c()
y <- c()
PC_x <- c()
PC_y <- c()
for(i in 1:length(tinterp)) {
  pp_index$index[i] <- unlist(t_tour[[i]][2]) # pp index value
  x <- c(x, unlist(t_tour[[i]][[1]][,1])) # projected x-coord for plot
  y <- c(y, unlist(t_tour[[i]][[1]][,2])) # projected y-coord for plot
  # Take first 'p' values to be x-coords for PCs
  PC_x <- c(PC_x, tinterp[[i]][1:p])
  # Take remaining 'p' values to be y-coords for PCs
  PC_y <- c(PC_y, tinterp[[i]][(p+1):(p+p)]) 
}
proj <- data.frame(Name=rep(rownames(crabs), length(tinterp)), 
                   step=rep(1:length(tinterp),each=dim(Xdataset)[1]), x=x, y=y)
head(proj)
tail(proj)

# Basis PC axes coords
basis <- data.frame(measure=rep(colnames(X_sphere), length(tinterp)),
                    step=rep(1:length(tinterp),each=p), magnitude= PC_x^2+PC_y^2, x=PC_x, y=PC_y)

# Sphere-ing data ---------------------------------------------------------
# Sphere data (Manually)
M_sphere <- sphere(crabs[,4:8])
# Rescaled then sphere'd data (preferred)
t <- save_history(crabs[,4:8], guided_tour(cmass, d=2, max.tries = 50), max=50, sphere = TRUE)
X_sphere <- attr(t, "data")
head(t_sphere)
head(X_sphere)
head(sphere(rescale(crabs[,4:8]))) # save_history = t_sphere is RESCALEd THEN sphered.
pairs(X_sphere) 
pairs(t_sphere) # More defined groups visible. RESCALE then sphere (order matters!)

# Initial orthog projection -----------------------------------------------
#intial basis (scatterplot w first two vars)
t2 <- save_history(Xdataset, guided_tour(cmass, d=2, max.tries = 50), max=50, sphere = TRUE)
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

# Find the projected data coords and calculate the pp index
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


# Colour argument ---------------------------------------------------------
# Colour
group_t <- "Decile"
fac_t <- unlist(subset(ach_narm, select = group_t)) %>% unlist() %>% as.factor()
levels(fac_t)
pal_t <- rainbow_hcl(length(levels(fac_t))) # Compute a rainbow of colours (qualitative palette)
group_col_t <- as.factor(pal_t[as.numeric(fac_t)])
group_col2 <- data.frame(col=group_col_t, group=fac_t)


# Projection axes plot ----------------------------------------------------
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


# new_tour() function -----------------------------------------------------
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
