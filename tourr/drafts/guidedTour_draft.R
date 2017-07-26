library(ggplot2)
library(plotly)
library(tourr)
library(abind)
library(MASS) # For crabs dataset
library(tidyr)

data("crabs")
str(crabs)

# Brush to select by group ------------------------------------------------
# Mosaic plot (would need shiny click?) or facet_wrap of scatterplots (crosstalk only)
# facet_grid would allow up to 4 factors to be crossed into groups
# Subset categorical vars
fac_cols <- sapply(crabs, class)=="factor"
Fdataset <- crabs[, fac_cols]
fac_n <- length(Fdataset)
Fdataset$Names <- rownames(crabs)
Fdataset$All <- factor(rep("1", length(Fdataset$Names)))
sd <- SharedData$new(data=Fdataset, key=~Names, group="2Dtour")

fac_cols <- sapply(mtcars, class)=="factor"
sd <- mtcars[, fac_cols]
sd$ID <- rownames(sd)
ggplot(sd, aes(x=sd[,1], y=sd[,2], label=ID,
               text=paste(nms[1], ":", sd[,1],
                          "<br>", nms[2], ":", sd[,2],
                          "<br>", nms[3], ":", sd[,3],
                          "<br>", nms[4], ":", sd[,4]))) +
  geom_jitter(width = 0.2, height = 0.2) + 
  #geom_point() +
  labs(x=nms[1], y=nms[2], 
       title="Only the first 4 factors have been displayed") +
  facet_grid(sd[,4]~sd[,3], scales = "free", space = "free") 
ggplotly(tooltip = c("text", "label"))

ggplot(sd, aes_string(x=names(sd)[1], y=names(sd)[2],
                      label1=names(sd)[3],
                      label2=names(sd)[4])) +
  geom_jitter(aes(label=ID), width = 0.2, height = 0.2) + 
  labs(title="Only the first 4 factors have been displayed") +
  facet_grid(sd[,4]~sd[,3], scales = "free", space = "free") 
ggplotly(tooltip = c("x", "y", "label", "label1", "label2"))

sd$All <- factor(rep("1", nrow(sd)))
ggplot(sd, aes_string(x=names(sd)[1])) +
  geom_jitter(aes(y=sd$All, label=ID), width = 0.2, height = 0.2) 
  #labs(title="Only the first 4 factors have been displayed") +
  #facet_grid(sd[,4]~sd[,3], scales = "free", space = "free") 
ggplotly(tooltip = c("x", "label"))

ggplot(sd, aes(x=cyl, y=vs)) +
  geom_jitter(aes(label=ID, label1=am, label2=gear), 
              width = 0.2, height = 0.2) + 
  #geom_point() +
  #labs(x=nms[1], y=nms[2], 
  #     title="Only the first 4 factors have been displayed") +
  facet_grid(gear~am, scales = "free", space = "free") 
ggplotly(tooltip = c("x", "y", "label", "label1", "label2"))

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
X_sphere <- as.data.frame(attr(t, "data"))
head(t_sphere)
head(X_sphere)
head(sphere(rescale(crabs[,4:8]))) # save_history = t_sphere is RESCALEd THEN sphered.
pairs(X_sphere) 
pairs(t_sphere) # More defined groups visible. RESCALE then sphere (order matters!)


# Plot for principal components coefficients -----------------------------
# tourr has rescale=T (so rescale first)
pc_rescale <- prcomp(rescale(crabs[,4:8]))
loads <- as.data.frame(pc_rescale$rotation)
# NOTE in prcomp(): For 'loads' above
#The signs of the columns of the rotation matrix are arbitrary, 
#and so may differ between different programs for PCA.
# We are interested in the magnitude of differences between the coefficients
# This reflects which var(s) the PC is contrasting between (see p357 MASS bk)
coeffs <- gather(loads, PC, coeff)
coeffs$X <- rep_len(rownames(loads), nrow(coeffs))
coeffs$sd <- rep(pc_rescale$sdev, each=5)
#plot
ggplot(coeffs, aes(x=PC, y=coeff, colour=sd, size=abs(coeff))) + 
  geom_point(aes(label=X)) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "#56B1F7", high = "#132B43") +
  scale_size_area(max_size = 3)
ggplotly(tooltip = c("x", "y", "colour", "label"))

X_df <- as.data.frame(X_sphere)
# Scatterplot matrix for selecting intial proj ----------------------------
d <- SharedData$new(data=X_df, key=~colnames(X_df), group="PC")
PC_plot <- ggpairs(d)
ggplotly(PC_plot)
# PROBLEM: This does NOT capture which PC matrix was clicked on
# Initial matrix
t0 <- matrix(c(1, rep(0, p), 1, rep(0, (p-2))), ncol = 2)
X_sphere[d$selection(),]
# Assgin SharedData as an object
# m is a dataset
d <- SharedData$new(m, ~rowname)
m[d$selection(),]


# pc and pc_center 
pc <- princomp(crabs[,4:8])
pc_center <- princomp(center(crabs[,4:8]))
# pc and pc_center are the same
pc$loadings
pc_center$loadings
head(pc$scores)
head(pc_center$scores)
# same as
head(as.matrix(center(crabs[,4:8]))%*%A)
A <- matrix(unlist(pc$loadings[1:5,]), ncol = 5)
A_center <- matrix(unlist(pc_center$loadings[1:5,]), ncol = 5)


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
