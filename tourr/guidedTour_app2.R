library(ggplot2)
library(plotly)
library(tourr)
library(crosstalk) # For (crosstalk+plotly) function
library(shiny)
library(htmltools)
library(MASS) # For crabs dataset
library(tidyr) # For PCs reshaping output for plot
library(GGally) # For ggpairs plot of PCs
# For wrangling NCEA data
library(dplyr)

# No plot for coefficients of principal components
# Click on index plot to select intial basis.
# 'factors=n' argument specifies number of factors to include in the plot for brushing groups
# The first 'n' factors will be used.

# Function for reordering first two cols (df can be sphere'd or not)
# All possible combinations of Xs as first two variables
# Output is a list with p(p-1)/2 components that are (n by p) data frames (ready for touring)
col_reorder <- function (df) {
  p <- ncol(df)
  Xnames <- colnames(df)
  Xs <- list()
  tour_names <- vector(mode="character", length=p*(p-1)/2)
  k <- 1
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      Xs[[k]] <- df[c(i,j,(1:p)[-c(i,j)])]
      tour_names[k] <- paste(Xnames[i], Xnames[j], sep="&")
      k <- k + 1
    }
  }
  reordered <- list(Xs, tour_names)
  return(reordered)
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
                   measure=rep(colnames(attr(single_tinterp, "data")), n))
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
    title="", range = c(-0.1, 1.2), 
    zeroline=F, showticklabels=F
  )
  #All tour plots linked by brushing
  df %>% SharedData$new(key=~ID, group = "2Dtour") %>% 
    plot_ly(x=~x, y=~y, frame=~iteration, color=I("black")) %>%
    add_markers(text=~ID, hoverinfo="text") %>%
    layout(xaxis=tx, yaxis=tx, 
           margin=list(l=0, r=0, b=0, t=0, pad=0))
}

# Draws tour "axes" plot
tour_axes <- function(df) {
  ax <- list(
    title="", range=c(-1.1, 1.2), 
    zeroline=F, showticklabels=F
  )
  plot_ly(df, x=~x, y=~y, frame=~iteration, hoverinfo="none") %>%
    add_segments(xend = 0, yend = 0, color = I("#FFC5D0"), size = I(1)) %>%
    add_text(text = ~measure, color=I("black")) %>%
    layout(xaxis = ax, 
           yaxis = list(title="", range=c(-2.1, 2.2), 
                        zeroline=F, showticklabels=F), 
           margin=list(l=0, r=0, b=0, t=0, pad=0))
}

# Draws LAST "tour" (proj) plot for scattermatrix
last_plot <- function(proj_list) {
  names(proj_list) <- c("x", "y", "col")
  df <- data.frame(x=proj_list$x, y=proj_list$y)
  ggplot(df, aes(x=x, y=y)) +
    geom_point(col=proj_list$col) +
    scale_x_continuous(limits = c(-0.1, 1.1)) +
    scale_y_continuous(limits = c(-0.1, 1.1)) +
    theme_void() +
    theme(legend.position = "none")
}

# Draws LAST "tour" (proj) plot for scattermatrix
last_axis_plot <- function(axis_list) {
  names(axis_list) <- c("x", "y", "m", "col")
  df <- data.frame(x=axis_list$x, y=axis_list$y, measure=axis_list$m)
  ggplot(df, aes(x=x, y=y)) +
    geom_segment(aes(xend=0, yend=0), colour=axis_list$col) +
    geom_text(aes(label=measure)) +
    scale_x_continuous(limits = c(-1.1, 1.1)) +
    scale_y_continuous(limits = c(-1.1, 1.1)) +
    theme_void()
}

guidedTour_app <- function(dataset, index="cmass", factors=2, PC= TRUE, ...) {
  # Subset real-valued vars (type="double") for touring
  realXs <- dataset[, which(sapply(dataset, typeof)=="double")]
  rownames(realXs) <- rownames(dataset)
  Xdataset <- as.data.frame(rescale(realXs))
  if (PC) {
    Xdataset <- as.data.frame(apply(predict(prcomp(Xdataset)), 2, scale)) # Want df so can easily reorder cols
  }
  # All possible combinations of Xs as first two variables
  reordered <- col_reorder(Xdataset) # Two output
  Xdfs <- reordered[[1]] # All combos of first two cols of Xs 
  tour_names <- reordered[[2]] 
  # Names for each tour by the leading pair names (eg."PC1&PC2"). Use in index plot
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
    init_pair <- c(init_pair, rep(tour_names[i], m))
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
  # Pull out last tour projection for each tour.
  last_projs <- lapply(XYall, last_XY) # No need for ID since not in SharedData
  # Add "ID" variable, rownames of original dataset
  XYall <- lapply(XYall, function (x) {
    ID <- rownames(dataset)
    cbind(x, ID)
  })
  # "axes" plot coords for ALL tours
  PCall <- lapply(tinterp, PCsingle)
  # Colour scale used in the index ggplot2 plot
  tour_cols <- hcl(h=seq(15, 360, 360/(p*(p-1)/2)), c=100, l=65)
  
  # Pull out last tour axes plot for each tour.
  #last_projs <- lapply(XYall, last_XY)
  last_axes <- lapply(PCall, last_PC)
  
  # Colour each last tour projection to match index plot cols
  if (p>5) {
    index_cols <- tour_cols[1:10] # Maximum of 10 pairwise combos
  } else {
    index_cols <- tour_cols 
  }
  # x, y, and col for each last proj is in a list
  last_projs <- mapply(c, last_projs, as.list(index_cols), SIMPLIFY = FALSE)
  last_axes <- mapply(c, last_axes, as.list(index_cols), SIMPLIFY = FALSE)
  P <- ifelse(p > 5, 5, p) # Maximum of 10 pairwise combos
  # Reorder plots for last_axes so that the scattermatrix will be diagonally symmetrical
  if (P==4) {
    last_axes <- list(last_axes[[1]], last_axes[[2]], last_axes[[4]],
                      last_axes[[3]], last_axes[[5]], last_axes[[6]])
  } else if (P==5) {
    last_axes <- list(last_axes[[1]], last_axes[[2]], last_axes[[5]],
                      last_axes[[3]], last_axes[[6]], last_axes[[8]],
                      last_axes[[4]], last_axes[[7]], last_axes[[9]],
                      last_axes[[10]])
  }
  # Scattermatrix plot data (max of 10 pairwise combos)
  # Lower diagonal is final proj and upper diagonal its axes components
  # List of P^2 (max 5) plots to plot using ggmatrix. Plots on diagonals (i,i) are empty
  diag_labels <- colnames(Xdfs[[1]])
  scatt_list <- list(ggally_text(diag_labels[1])) 
  tplot_list <- lapply(last_projs, last_plot)
  aplot_list <- lapply(last_axes, last_axis_plot)
  for (i in 1:(P-1)) {
    scatt_list <- c(scatt_list, tplot_list[1:(P-i)],
                    aplot_list[1:i], list(ggally_text(diag_labels[i+1]))) 
    #(P-i) is the number of tour plots and i the # of axes plots for iteration i
    #Delete merged plots
    tplot_list <- tplot_list[-c(1:(P-i))]
    aplot_list <- aplot_list[-c(1:i)]
  }
  # scatt_list contains all last projections (for each tour, up to 10 tours)
  
  # SharedData for the factors plot
  sdF <- SharedData$new(Fdataset, key=~ID, group="2Dtour") 
  
  ## Shiny app
  ui <- fluidPage(
    splitLayout(cellWidths = c("25%", "40%", "35%"),
                plotlyOutput("indexPlot"),
                plotlyOutput("tour"),
                #plotlyOutput("factors")
                plotlyOutput("pairs")
    ),
    splitLayout(cellWidths = c("40%", "60%"),
                plotOutput("scatter"),
                #plotlyOutput("pairs")
                plotlyOutput("factors")
    )
  )
  server <- function(input, output, session) {
    ## Proj pursuit index plot
    output$indexPlot <- renderPlotly({
      ggplot(pp_index, aes(x=iteration, y=pursuit_index, group=init_pair, col=init_pair)) +
        geom_point() +
        geom_line() +
        ggtitle("Projection pursuit index") +
        theme(legend.position="none") # Selecting by legend hard to reset.
      ggplotly(source="index_plot", tooltip = c("x", "y", "group")) %>%
        layout(autosize = F, width=350, height=400)
    })
    output$tour <- renderPlotly({
      s <- event_data("plotly_click", source = "index_plot")
      if (length(s)) {k <- s$curveNumber + 1} else {k <- 1}
      proj <- XYall[[k]]
      basis <- PCall[[k]]
      
      ## tour plot
      tour <- proj %>% tour_plot()
      axes <- basis %>% tour_axes()
      subplot(tour, axes, titleY=F, widths=c(0.7, 0.3), margin=0) %>%
        animation_opts(33) %>% #33 milliseconds between frames
        #animation_button(xanchor="middle", x=0, yanchor="top", y=0) %>%
        hide_legend() %>%
        layout(dragmode="select", #height=400,
               margin=list(l=0, r=0, b=0, t=0, pad=0)) %>%
        highlight(on="plotly_select", off="plotly_deselect", color="blue", 
                  persistent=T, dynamic=T)
    })
    output$factors <- renderPlotly({
      # facet_grid would allow up to 4 factors to be crossed into groups
      if (fac_n==1) {
        gp <- ggplot(sdF, aes_string(x=names(Fdataset)[1])) +
          geom_jitter(aes(y=Fdataset$All, label=ID), 
                      width=0, height=0.25) +
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
                      width=0.2, height=0.2) +
          labs(title="The first 3 factors are displayed") +
          facet_grid(.~Fdataset[,3])
      } else if (fac_n>3) {
        gp <- ggplot(sdF, aes_string(x=names(Fdataset)[1], y=names(Fdataset)[2],
                                     label1=names(Fdataset)[3],
                                     label2=names(Fdataset)[4])) +
          geom_jitter(aes(label=ID), 
                      width = 0.2, height = 0.2) +
          labs(title="The first 4 factors are displayed") +
          facet_grid(Fdataset[,4]~Fdataset[,3], scales="free", space="free")
      }
      
      if (fac_n==0) { # fac_n=0 No factors in dataset
        plotly_empty()
        warning("There are no variables that are factors in the data set.")
      } else {
        ggplotly(gp, tooltip=c("x", "y", "label", "label1", "label2"), height=400, width=600) %>%
          layout(dragmode="select") %>%
          hide_legend() %>%
          highlight(on="plotly_select", off="plotly_deselect", color="blue", 
                    persistent=T)
      }
    })
    output$scatter <- renderPlot({
      ggmatrix(scatt_list, P, P, byrow = F, showAxisPlotLabels = F,
               title="Final tour projection and axes plots")
    })
    output$pairs <- renderPlotly({
      realXs %>% 
        SharedData$new(key=~rownames(realXs), group="2Dtour") %>%
        ggpairs(upper="blank") %>%
        #ggpairs(lower="blank", upper=list(continuous = "points")) %>%
        ggplotly() %>%
        layout(dragmode="select", title="Pairs plot") %>%
        highlight(on="plotly_select", off="plotly_deselect", color="blue", 
                  persistent=T) %>%
        layout(autosize = F, width=400, height=400)
    })
  }
  shinyApp(ui, server)
}

# Testing
data("crabs")
guidedTour_app(crabs, index = "holes")

# NCEA data (Akl schools) -------------------------------------------------
nzqa2016 <- read.csv("http://www.nzqa.govt.nz/assets/Studying-in-NZ/Secondary-school-and-NCEA/stats-reports/2016/Qualification-Statistics-School-2016-29032017.csv")
# Drop vars that will not be used (eg. cumulative achievement)
nzqa <- nzqa2016[,-c(1,8,10)]
names(nzqa) <- c("Decile", "Region", "School", "Year", "Qualification", "Achieve_participate", "Achieve_roll", "Small_sch")
levels(nzqa$Qualification) <- c("L1", "L2", "L3", "UE")
# Subset to use only Year 11 with Level 1, etc
nzqa <- nzqa %>% filter(((Qualification=="L1")&(Year==11))|((Year==12) & (Qualification=="L2"))|
                          ((Year==13) & (Qualification=="L3"))|((Year==13) & (Qualification=="UE")))
# Reshape so that one row = one school
# Current.Achievement.Rate.Participation kept for analysis only
achieved <- nzqa %>% 
  spread(Qualification, Achieve_participate, fill=0) %>%
  group_by(School) %>%
  summarise_at(c("L1", "L2", "L3", "UE"), sum) %>%
  inner_join(nzqa[, c(1, 2, 3, 8)]) %>% #Add Decile and Region and Small_sch variables
  distinct() %>% #One row per school
  filter(!((L1==0)&(L2==0)&(L3==0))) #Remove schools with 0% achievement rate for all levels
# Function to replace 0% with NA
zeros <- function(col) {
  replace(col, col==0, NA)
}
achieved[2:5] <- sapply(achieved[2:5], zeros)
# Remove schools with 'small cohort' warning (obscures pattern in non-small cohorts).
achieved <- achieved[achieved$Small_sch=="",] #437 school left
achieved <- achieved[, -8] #Remove Small_sch var
# 'achieved' contains schools with ONE or more % achievement rate (by participation)
# NA's used otherwise
# Small cohort schools removed
# Remove obs with any NA values 
ach_narm <- achieved[complete.cases(achieved),] #407 schools left
ach_narm$Decile <- as.factor(ach_narm$Decile)
rownames(ach_narm) <- ach_narm$School
akl <- as.data.frame(ach_narm[ach_narm$Region=="Auckland", ]) #90 schools in Akl
rownames(akl) <- akl$School
akl <- akl[-1]
guidedTour_app(akl, factors=1, index="holes")
# PC1&2 show a bit of a separation of a group of schools from the main group 
# This group is mainly decile 5 and below, except for Grammar & Kings
# They are generally in the "scattered tail" of schools in the pairs plot.
# 'cmass' identifies outlier schools, where achievement across L1, 2, 3 and UE is inconsistent
