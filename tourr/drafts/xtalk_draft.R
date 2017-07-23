library(ggplot2)
library(plotly)
library(tourr)
library(abind)
library(colorspace) # For colour as an argument
library(crosstalk) # For (crosstalk+plotly) function
library(htmltools)
library(MASS) # For crabs dataset
library(tidyr) # For PCs reshaping output for plot
library(GGally) # For ggpairs plot of PCs
data("crabs")

# 'factors=n' argument specifies number of factors to include in the plot for brushing groups
# The first 'n' factors will be used.
# Variables will be sphere'd so that principal coordinates are used in the analysis
pp2D_xtalk <- function(dataset, index="cmass", factors=2, ...) {
  # Subset real-valued vars (type="double") for touring
  num_cols <- sapply(dataset, typeof)=="double"
  Xdataset <- dataset[, num_cols]
  p <- length(Xdataset) # Number of real-valued Xs used in tour
  # Save tour
  if (index=="cmass") {
    t <- save_history(Xdataset, guided_tour(cmass, d=2, max.tries = 50), sphere=TRUE, max=50, ...) 
  } else if (index=="holes") {
    t <- save_history(Xdataset, guided_tour(holes, d=2, max.tries = 50), sphere=TRUE, max=50, ...)
  } else {
    stop("Invalid 'index' argument. Choose either: cmass or holes")
  }
  # Start view as orthogonal projection of first two measures
  # Retain orthogonal projection as initial basis
  t0 <- t[,,1]
  t0 <- matrix(c(1, rep(0, p), 1, rep(0, (p-2))), ncol = 2)
  t1 <- abind(t0, t, along = 3)
  class(t1) <- "history_array"
  tinterp <- interpolate(t1) 
  X_sphere <- attr(t, "data") #(n by p) 
  # Central mass index and projection pursuit tour function
  cmass_tour <- function(basis) {
    # Projected data matrix 
    XA <- X_sphere%*%matrix(basis, ncol=2) # (n by d)
    cmass_index <- (sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1]-exp(-dim(XA)[2]/2))/(1-exp(-dim(XA)[2]/2))
    list(rescale(XA), cmass_index)
  }
  # Apply index function to each projection basis
  t_tour <- apply(tinterp, 3, FUN = cmass_tour)
  pp_index <- data.frame(iteration=1:length(tinterp))
  x <- c()
  y <- c()
  PC_x <- c()
  PC_y <- c()
  for(i in 1:length(t_tour)) {
    pp_index$index[i] <- unlist(t_tour[[i]][2])
    x <- c(x, unlist(t_tour[[i]][[1]][,1]))
    y <- c(y, unlist(t_tour[[i]][[1]][,2]))
    # Take first 'p' values to be x-coords for PCs
    PC_x <- c(PC_x, unlist(tinterp[[i]][1:p])) 
    # Take remaining 'p' values to be y-coords for PCs
    PC_y <- c(PC_y, unlist(tinterp[[i]][(p+1):(p+p)])) 
  }
  proj <- data.frame(ID=rep(rownames(dataset), length(tinterp)), 
                     step=rep(1:length(tinterp),each=dim(Xdataset)[1]), x=x, y=y)
  basis <- data.frame(measure=rep(colnames(X_sphere), length(tinterp)),
                      step=rep(1:length(tinterp),each=p), 
                      magnitude=PC_x^2+PC_y^2, x=PC_x, y=PC_y)
  
  # Axes for tour plot ("tour") and measurement var coeffs plot ("axes")
  tx <- list(
    title = "", range = c(-0.1, 1.1), 
    zeroline = F, showticklabels = F
  )
  
  ax <- list(
    title = "", range = c(-1.1, 1.1), 
    zeroline = F, showticklabels = F
  )
  # tour plot
  tour <- proj %>%
    SharedData$new(~ID, group = "2Dtour") %>%
    plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black"), 
            height = 450, width = 800) %>%
    add_markers(text = ~ID, hoverinfo = "text") %>%
    layout(xaxis = tx, yaxis = tx)
  
  axes <- basis %>%
    plot_ly(x = ~x, y = ~y, frame = ~step, hoverinfo = "none") %>%
    add_segments(xend = 0, yend = 0, color = I("gray"), size = I(1)) %>%
    # Color segments don't work
    #add_segments(xend = 0, yend = 0, color = ~magnitude, size = I(1), showlegend=FALSE) %>%
    add_text(text = ~measure, color=I("black")) %>%
    layout(xaxis = ax, yaxis = ax)
  # Doesn't work if use ggplot() for axes plot
  
  # very important these animation options are specified _after_ subplot()
  # since they call plotly_build(., registerFrames = T)
  # (Took a few minutes to run)
  tour <- subplot(tour, axes, nrows = 1, margin = 0) %>%
    animation_opts(33) %>% #33 milliseconds between frames
    hide_legend() %>%
    layout(dragmode = "select") %>%
    highlight(on="plotly_select", off= "plotly_deselect", color = "blue", persistent = TRUE)
  
  # Subset categorical vars
  fac_cols <- sapply(dataset, class)=="factor"
  Fdataset <- dataset[, fac_cols]
  #fac_n <- length(Fdataset)
  if (factors <= length(Fdataset)) {
    fac_n <- factors
  } else if (length(Fdataset) >= 2) {
    fac_n = 2 # Use default if 'factors' arg not sensible
    warning("'factors' argument exceeds number of factors in data.")
  } else {
    fac_n = length(Fdataset)
    warning("'factors' argument exceeds number of factors in data.")
  }
  Fdataset$ID <- rownames(dataset)
  Fdataset$All <- factor(rep("1", nrow(dataset)))
  
  # facet_grid would allow up to 4 factors to be crossed into groups
  sd <- SharedData$new(Fdataset, key=~ID, group = "2Dtour") 
  if (fac_n==1) {
    p <- ggplot(sd, aes_string(x=names(Fdataset)[1])) +
      geom_jitter(aes(y=Fdataset$All, label=ID), 
                  width = 0.25, height = 0.25) +
      labs(title="The first factor is displayed") 
  } else if (fac_n==2) {
    p <- ggplot(sd, aes_string(x=names(Fdataset)[1], y=names(Fdataset)[2])) +
      geom_jitter(aes(label=ID), 
                  width = 0.25, height = 0.25) +
      labs(title="The first 2 factors are displayed") 
  } else if (fac_n==3) {
    p <- ggplot(sd, aes_string(x=names(Fdataset)[1], y=names(Fdataset)[2],
                               label1=names(Fdataset)[3])) +
      geom_jitter(aes(label=ID), 
                  width = 0.2, height = 0.2) +
      labs(title="The first 2 factors are displayed") +
      facet_grid(.~Fdataset[,3])
  } else if (fac_n>3) {
    p <- ggplot(sd, aes_string(x=names(Fdataset)[1], y=names(Fdataset)[2],
                               label1=names(Fdataset)[3],
                               label2=names(Fdataset)[4])) +
      geom_jitter(aes(label=ID), 
                  width = 0.2, height = 0.2) +
      labs(title="The first 2 factors are displayed") +
      facet_grid(Fdataset[,4]~Fdataset[,3], scales = "free", space = "free")
  }
  
  if (fac_n==0) { # fac_n=0 No factors in dataset
    brush_group <- plotly_empty()
    warning("There are no variables that are factors in the data set.")
  } else {
    brush_group <- ggplotly(p, tooltip=c("x", "y", "label", "label1", "label2")) %>%
      layout(dragmode="select") %>%
      hide_legend() %>%
      highlight(on="plotly_select", off= "plotly_deselect", color = "blue", 
                persistent=T, dynamic=T)
  }
  # tourr has rescale=T (so rescale first)
  pc_rescale <- prcomp(rescale(crabs[,4:8]))
  loads <- as.data.frame(pc_rescale$rotation)
  # Note: signs of coeffs are random.
  # We are interested in the magnitude of differences between the coefficients
  # This reflects which var(s) the PC is contrasting between (see p357 MASS bk)
  coeffs <- gather(loads, PC, coeff)
  coeffs$X <- rep_len(rownames(loads), nrow(coeffs))
  coeffs$sd <- rep(pc_rescale$sdev, each=5)
  # Plot for coeffs
  pc <- ggplot(coeffs, aes(x=PC, y=coeff, colour=sd, size=abs(coeff))) + 
    geom_point(aes(label=X)) +
    theme(legend.position = "none") +
    scale_color_gradient(low = "#56B1F7", high = "#132B43") +
    scale_size_area(max_size = 3) 
  pc_coeffs <- ggplotly(pc, tooltip = c("x", "y", "colour", "label"))
  
  html <- tags$div(
    style = "display: flex; flex-wrap: wrap",
    tags$div(tour, align = "center", style = "width: 50%; padding: 1em;"),
    tags$div(brush_group, align = "center", style = "width: 50%; padding: 1em;"),
    tags$div(pc_coeffs, align = "center", style = "width: 50%; padding: 1em;")
  )
  
  # opens in an interactive session
  res <- html_print(html)
}

# Testing
pp2D_xtalk(crabs, index = "holes")

lcrabs <- crabs
lcrabs[,4:8] <- log(lcrabs[,4:8])
pp2D_xtalk(lcrabs, index = "holes")

data("mtcars")
str(mtcars) #cyl, vs, am, gear, carb are all factors
summary(mtcars)
mtcars[, c(2,8,9,10,11)] <- lapply(mtcars[, c(2,8,9,10,11)], factor)
pp2D_xtalk(mtcars, factors = 4)

# Plotly tooltips not accurate when using facet_grid.
# This happens when one cell does not contain any data
# See <https://github.com/tidyverse/ggplot2/issues/165>