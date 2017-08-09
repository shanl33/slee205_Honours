library(ggplot2)
library(plotly)
library(tourr)
library(crosstalk) # For (crosstalk+plotly) function
library(shiny)
library(htmltools)
library(MASS) # For crabs dataset
library(tidyr) # For PCs reshaping output for plot
library(GGally) # For ggpairs plot of PCs
data("crabs")
# No plot for coefficients of principal components
# Click on index plot to select intial basis.
# 'factors=n' argument specifies number of factors to include in the plot for brushing groups
# The first 'n' factors will be used.

# Function for reordering first two cols (df can be sphere'd or not)
# All possible combinations of Xs as first two variables
# Output is a list with p(p-1)/2 components that are (n by p) data frames (ready for touring)
col_reorder <- function (df) {
  p <- ncol(df)
  Xs <- list()
  k <- 1
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      Xs[[k]] <- df[c(i,j,(1:p)[-c(i,j)])]
      k <- k + 1
    }
  }
  return(Xs)
}

# Extracts xy coords for "tour" projection plots of a single tour (ie. proj coords of a tour)
XYsingle <- function (single_tour) {
  n <- length(single_tour)
  x <- c()
  y <- c()
  for(i in 1:n) {
    x <- c(x, unlist(single_tour[[i]][[1]][,1]))
    y <- c(y, unlist(single_tour[[i]][[1]][,2]))
  }
  XY <- data.frame(x=x, y=y, iteration = rep(1:n, each=length(x)/n))
  # need to add ID=rep(rownames(dataset), n) if using sdT
}

# Extracts xy coords for "axes" plots of a single tour 
PCsingle <- function (single_tinterp) {
  n <- length(single_tinterp)
  p <- dim(single_tinterp)[1] #number of real valued Xs
  PC_x <- c()
  PC_y <- c()
  for(i in 1:n) {
    # Take first 'p' values to be x-coords for PCs
    PC_x <- c(PC_x, unlist(single_tinterp[[i]][1:p])) 
    # Take remaining 'p' values to be y-coords for PCs
    PC_y <- c(PC_y, unlist(single_tinterp[[i]][(p+1):(p+p)])) 
  }
  PC <- data.frame(x = PC_x, y = PC_y, iteration = rep(1:n, each = p),
                   magnitude = PC_x^2+PC_y^2, measure=rep(colnames(attr(single_tinterp, "data")), n))
}

# Functions for extracting last projection of a (single) tour (for scattermatrix)
# Extract the last projection for a tour
last_XY <- function(XY) {
  m <- max(XY$iteration)
  XY[which(XY$iteration==m), -3]
}

# Extract the last axes plot coords for a tour
last_PC <- function(PC) {
  m <- max(PC$iteration)
  PC[which(PC$iteration==m), -3]
}

# Draws "tour" (proj) plot 
tour_plot <- function(df) {
  tx <- list(
    title = "", range = c(-0.1, 1.1), 
    zeroline = F, showticklabels = F
  )
  #All tour plots linked by brushing
  df %>% SharedData$new(key=~ID, group = "2Dtour") %>% 
    plot_ly(x = ~x, y = ~y, frame = ~iteration, color = I("black")) %>%
    add_markers(text = ~ID, hoverinfo = "text") %>%
    layout(xaxis = tx, yaxis = tx)
}

# Draws tour "axes" plot
tour_axes <- function(df) {
  ax <- list(
    title = "", range = c(-1.1, 1.2), 
    zeroline = F, showticklabels = F
  )
  plot_ly(df, x = ~x, y = ~y, frame = ~iteration, hoverinfo = "none") %>%
    add_segments(xend = 0, yend = 0, color = I("darkgray"), size = I(1)) %>%
    add_text(text = ~measure, color=I("black")) %>%
    layout(xaxis = ax, yaxis = ax)
}

# Draws LAST "tour" (proj) plot for scattermatrix
last_plot <- function(df) {
  sd <- SharedData$new(df, key=~ID, group = "2Dtour")
  ggplot(sd, aes(x=x, y=y, label=ID)) +
    geom_point() +
    scale_x_continuous(limits = c(-0.1, 1.1)) +
    scale_y_continuous(limits = c(-0.1, 1.1)) +
    theme_void() +
    theme(legend.position = "none")
  ggplotly() %>%
    layout(dragmode="select") %>%
    highlight(persistent=T)
}
# Draws LAST "tour" (proj) plot for scattermatrix
last_axis_plot <- function(df) {
  ggplot(df, aes(x=x, y=y)) +
    geom_segment(aes(xend=0, yend=0, col=magnitude)) +
    geom_text(aes(label=measure)) +
    scale_x_continuous(limits = c(-1.1, 1.1)) +
    scale_y_continuous(limits = c(-1.1, 1.1)) +
    theme_void() +
    theme(legend.position = "none") +
    scale_color_gradient(low = "#56B1F7", high = "#132B43")
}

guidedTour_app <- function(dataset, index="cmass", factors=2, PC= TRUE, ...) {
  # Subset real-valued vars (type="double") for touring
  Xdataset <- as.data.frame(rescale(dataset[, which(sapply(dataset, typeof)=="double")]))
  if (PC) {
    Xdataset <- as.data.frame(apply(predict(prcomp(Xdataset)), 2, scale)) # Want df so can easily reorder cols
  }
  # All possible combinations of Xs as first two variables
  Xdfs <- col_reorder(Xdataset)
  # Save tour (rescale=F since already scaled to col to range [0,1])
  if (index=="cmass") {
    t <- lapply(Xdfs, function (x) save_history(x, guided_tour(cmass, d=2, max.tries = 50), rescale=FALSE, max=50, ...))
  } else if (index=="holes") {
    t <- lapply(Xdfs, function (x) save_history(x, guided_tour(holes, d=2, max.tries = 50), rescale=FALSE, max=50, ...))
  } else {
    stop("Invalid 'index' argument. Choose either: cmass or holes")
  }
  # 'Inserting' initial base as orthogonal projection of first two measures
  p <- ncol(Xdataset) # Number of real-valued Xs 
  t0 <- matrix(c(1, rep(0, p), 1, rep(0, (p-2))), ncol = 2)
  class(t0) <- "history_array"
  t1 <- lapply(t, function (x) array(c(t0, x), dim = dim(x) + c(0,0,1)))
  # Assign class and data attributes
  for (i in 1:length(t1)) {
    class(t1[[i]]) <- "history_array"
    attr(t1[[i]], "data") <- Xdfs[[i]]
  }
  tinterp <- lapply(t1, interpolate)
  #t, t1, tinterp are all lists with p(p-1)/2 components (all possible pairwise combos)
  # Central mass index and projection pursuit tour function
  cmass_tour <- function(base) {
    # Projected data matrix 
    XA <- X_matrix%*%matrix(base, ncol=2) # (n by d)
    cmass_index <- (sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1]-exp(-dim(XA)[2]/2))/(1-exp(-dim(XA)[2]/2))
    list(rescale(XA), cmass_index)
  }
  
  # Holes index
  holes_tour <- function(base) {
    XA <- X_matrix%*%matrix(base, ncol=2) # (n by d)
    holes_index <- (1-sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1])/(1-exp(-dim(XA)[2]/2))
    list(rescale(XA), holes_index)
  }
  
  #t_tour is a list with p(p-1)/2 lists with varying dimensions (depend on the length of each tour)
  #t_tour contains info about the pursuit_index: for 'index_plot'
  #t_tour contains info about the x and y positions of the projections: for the plot 'tour' 
  #t_tour contains info about the tour axes coords: for the plot 'axes' (subplot of 'tour')
  t_tour <- list()
  #pp_index is a data frame with the vars: init_pair, iteration, index
  init_pair=c()
  iteration=c()
  pursuit_index=c()
  for(i in 1:length(tinterp)) {
    X_matrix <- as.matrix(Xdfs[[i]])
    # Apply index function to each projection basis
    t_tour[[i]] <-apply(tinterp[[i]], 3,  FUN = paste(index,"_tour", sep=""))
    # m is the number of iterations for that tour
    m <- length(tinterp[[i]])
    init_pair <- c(init_pair, rep.int(i, m))
    iteration <- c(iteration, 1:m)
    for (j in 1:m) {
      pursuit_index <- c(pursuit_index, unlist(t_tour[[i]][[j]][2]))
    }
  }
  pp_index <- data.frame(init_pair, iteration, pursuit_index)

  # Subset categorical vars
  Fdataset <- dataset[, which(sapply(dataset, class)=="factor")]
  if (factors <= length(Fdataset)) {
    fac_n <- factors
  } else if (length(Fdataset) >= 2) {
    fac_n = 2 # Use default if 'factors' arg not sensible
    warning("'factors' argument exceeds number of factors in data.")
  } else {
    fac_n = length(Fdataset)
    warning("'factors' argument exceeds number of factors in data.")
  }
  Fdataset$ID <- rownames(Fdataset)
  Fdataset$All <- factor(rep("1", nrow(Fdataset)))
  # "tour" proj plot xy coords for ALL tours
  XYall <- lapply(t_tour, XYsingle) 
  # Add "ID" variable, rownames of original dataset
  XYall <- lapply(XYall, function (x) {
    ID <- rownames(crabs)
    cbind(x, ID)
  })
  # "axes" plot coords for ALL tours
  PCall <- lapply(tinterp, PCsingle)
  
  # Pull out last tour projection and tour axes plot for each tour.
  last_projs <- lapply(XYall, last_XY)
  last_axes <- lapply(PCall, last_PC)
  
  # SharedData for the factors plot
  sdF <- SharedData$new(Fdataset, key=~ID, group = "2Dtour") 
  # SharedData for the tours plot (To do if needed selection() maybe needed in "tour" plot then)
  #sdT <- SharedData$new(proj, key=~ID, group = "2Dtour") 
  
  ## Shiny app
  ui <- fluidPage(
    splitLayout(cellWidths = c("60%", "40%"),
      plotlyOutput("tour"),
      plotlyOutput("indexPlot")
    ),
    plotlyOutput("factors"),
    plotlyOutput("scatter")
  )
  server <- function(input, output, session) {
  ## Proj pursuit index plot
  output$indexPlot <- renderPlotly({
    ggplot(pp_index, aes(x=iteration, y=pursuit_index, group=init_pair, col=as.factor(init_pair))) +
      geom_point() +
      geom_line() +
      ggtitle("Projection pursuit index") +
      theme(legend.position = "none")
    #index_plotly <- ggplotly(index_plot)
      ggplotly(source="index_plot")
  })
  output$tour <- renderPlotly({
    s <- event_data("plotly_click", source = "index_plot")
    if (length(s)) {k <- s$curveNumber + 1} else {k <- 1}
    # Functions to apply over each component in the list "tinterp"
    # tour_data(tinterp)
    # tour_plots()
    
    proj <- cbind(ID=rownames(dataset), XYsingle(t_tour[[k]]))
    basis <- PCsingle(tinterp[[k]])
    
    # Axes for tour plot and tour axes plot
    #tx <- list(
     # title = "", range = c(-0.1, 1.1), 
    #  zeroline = F, showticklabels = F
    #)
    #ax <- list(
     # title = "", range = c(-1.1, 1.2), 
    #  zeroline = F, showticklabels = F
    #)
    
    ## tour plot
    tour <- proj %>% tour_plot()
      #SharedData$new(key=~ID, group = "2Dtour") %>% 
      #plot_ly(x = ~x, y = ~y, frame = ~iteration, color = I("black")) %>%
      #add_markers(text = ~ID, hoverinfo = "text") %>%
      #layout(xaxis = tx, yaxis = tx)
    ## tour axes plot
    axes <- basis %>% tour_axes()
      #plot_ly(x = ~x, y = ~y, frame = ~iteration, hoverinfo = "none") %>%
      #add_segments(xend = 0, yend = 0, color = I("darkgray"), size = I(1)) %>%
      #add_text(text = ~measure, color=I("black")) %>%
      #layout(xaxis = ax, yaxis = ax)
    # Doesn't seem to work if use ggplot() for axes plot and use with subplot()
    # Maybe both plots need to be ggplot() to work?
    # Combine the two tour plots with a single slider
    subplot(tour, axes, titleY = T, widths = c(0.7, 0.3), margin = 0) %>%
      animation_opts(33) %>% #33 milliseconds between frames
      hide_legend() %>%
      layout(dragmode = "select") %>%
      highlight(on="plotly_select", off= "plotly_deselect", color = "blue", persistent = TRUE)
  })
  output$factors <- renderPlotly({
    # facet_grid would allow up to 4 factors to be crossed into groups
    if (fac_n==1) {
      gp <- ggplot(sdF, aes_string(x=names(Fdataset)[1])) +
        geom_jitter(aes(y=Fdataset$All, label=ID), 
                    width = 0.25, height = 0.25) +
        labs(title="The first factor is displayed") 
    } else if (fac_n==2) {
      gp <- ggplot(sdF, aes_string(x=names(Fdataset)[1], y=names(Fdataset)[2])) +
        geom_jitter(aes(label=ID), 
                    width = 0.25, height = 0.25) +
        labs(title="The first 2 factors are displayed") 
    } else if (fac_n==3) {
      gp <- ggplot(sdF, aes_string(x=names(Fdataset)[1], y=names(Fdataset)[2],
                                  label1=names(Fdataset)[3])) +
        geom_jitter(aes(label=ID), 
                    width = 0.2, height = 0.2) +
        labs(title="The first 3 factors are displayed") +
        facet_grid(.~Fdataset[,3])
    } else if (fac_n>3) {
      gp <- ggplot(sdF, aes_string(x=names(Fdataset)[1], y=names(Fdataset)[2],
                                  label1=names(Fdataset)[3],
                                  label2=names(Fdataset)[4])) +
        geom_jitter(aes(label=ID), 
                    width = 0.2, height = 0.2) +
        labs(title="The first 4 factors are displayed") +
        facet_grid(Fdataset[,4]~Fdataset[,3], scales = "free", space = "free")
    }
    
    if (fac_n==0) { # fac_n=0 No factors in dataset
      plotly_empty()
      warning("There are no variables that are factors in the data set.")
    } else {
      ggplotly(gp, tooltip=c("x", "y", "label", "label1", "label2"), height=400) %>%
        layout(dragmode="select") %>%
        hide_legend() %>%
        highlight(on="plotly_select", off= "plotly_deselect", color = "blue", 
                  persistent=T, dynamic=T)
    }
  })
  output$scatter <- renderPlotly({
    #last_axis_plot(last_axes[[1]]) #works!
    last_plot(last_projs[[1]]) # works
  })
  }
  shinyApp(ui, server)
}

# Testing
guidedTour_app(crabs, index = "holes")

lcrabs <- crabs
lcrabs[,4:8] <- log(lcrabs[,4:8])
pp2D_xtalk(lcrabs, index = "holes")

# Testing brush_group plot
data("mtcars")
mtcars[, c(2,8,9,10,11)] <- lapply(mtcars[, c(2,8,9,10,11)], factor)
guidedTour_app(mtcars, factors = 4)
