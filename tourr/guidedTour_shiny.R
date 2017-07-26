library(ggplot2)
library(plotly)
library(tourr)
library(shiny)
library(colorspace)


# Projection pursuit 2D guided tour (Function with Shiny app) -------------
# dataset should have rownames as appropriate ID label for each obs
# pp_index takes the same values as the index arguement in guided_tour
# cmass, holes, (To do: lda_pp)
pp2D_shiny <- function (dataset, index="cmass", group=NULL) {
  # Subset real-valued vars (type="double") for touring
  num_cols <- sapply(dataset, typeof)=="double"
  Xdataset <- dataset[, num_cols]
  # Save tour
  if (index=="cmass") {
    t <- save_history(Xdataset, guided_tour(cmass, d=2, max.tries = 50), sphere=TRUE, max=50) 
  } else if (index=="holes") {
    t <- save_history(Xdataset, guided_tour(holes, d=2, max.tries = 50), sphere=TRUE, max=50)
  } else {
    stop("Invalid 'index' argument. Choose either: cmass or holes")
  }
  tinterp <- interpolate(t) # Uses basis to create smooth tour
  X_sphere <- attr(t, "data") #(n by p) 
  # Central mass index and projection pursuit tour function
  cmass_tour <- function(basis) {
    # Projected data matrix 
    XA <- X_sphere%*%matrix(basis, ncol=2) # (n by d)
    cmass_index <- (sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1]-exp(-dim(XA)[2]/2))/(1-exp(-dim(XA)[2]/2))
    list(rescale(XA), cmass_index)
  }
  # Holes index
  holes_tour <- function(basis) {
    XA <- X_sphere%*%matrix(basis, ncol=2) # (n by d)
    holes_index <- (1-sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1])/(1-exp(-dim(XA)[2]/2))
    list(rescale(XA), holes_index)
  }
  # Apply index function to each projection basis
  pp_index <- data.frame(iteration=1:length(tinterp))
  # 3rd component of tinterp is (1 by 2p) containing x-coeffs for each measurement var followed by y-coeffs
  # matrix(3rd_comp, ncol=2) converts it into a projection matrix, A.
  if (index=="cmass") {
    t_tour <- apply(tinterp, 3, FUN = cmass_tour)
  } else if (index=="holes") {
    t_tour <- apply(tinterp, 3, FUN = holes_tour)
  }
  
  for(i in 1:length(t_tour)) {
    pp_index$index[i] <- unlist(t_tour[[i]][2])
  }
  # Colour by Group 
  if (length(group)) {
    fac <- unlist(subset(dataset, select = group)) %>% unlist() %>% as.factor()
    pal <- rainbow_hcl(length(levels(fac))) # Compute a rainbow of colours (qualitative palette)
    colour <- as.factor(pal[as.numeric(fac)])
    col <- data.frame(col=colour, group=fac)
  } else {
    col <- data.frame(col="black", group="NA")
  }
  
  # Shiny app ---------------------------------------------------------------
  
  ui <- fluidPage(navbarPage("2D Guided Tour",
                             tabPanel("Animation",
                                      mainPanel(
                                        sliderInput("iteration", "Interation", min = 1, value = 1, step = 1,
                                                    max = length(t_tour), animate = animationOptions(300), width = 500),
                                        plotlyOutput("tour")
                                      )       
                             ),
                             tabPanel("Proj Pursuit Index",
                                      mainPanel(
                                        plotlyOutput("index"),
                                        plotlyOutput("tour2")
                                      )
                             )
  ))
  
  server <- function(input, output, session) {
    output$tour <- renderPlotly({
      proj <- as.data.frame(t_tour[[input$iteration]][[1]])
      proj$ID <- rownames(dataset)
      p <- ggplot(proj, aes(x=V1, y=V2, label=ID, col=col$col, label1=col$group)) +
        geom_point() +
        scale_x_continuous(limits = c(-0.1, 1.1)) +
        scale_y_continuous(limits = c(-0.1, 1.1)) +
        theme_void() +
        theme(legend.position = "none")
      tour_p <- ggplotly(p, tooltip = c("label", "label1")) 
      basis <- as.data.frame(matrix(tinterp[[input$iteration]], ncol = 2))
      basis$measure <- colnames(Xdataset)
      basis$magnitude <- (basis$V1)^2+(basis$V2)^2
      axis_p <- ggplot(basis, aes(x=V1, y=V2)) +
        geom_segment(aes(xend=0, yend=0, col=magnitude)) +
        geom_text(aes(label=measure)) +
        scale_x_continuous(limits = c(-1.1, 1.1)) +
        scale_y_continuous(limits = c(-1.1, 1.1)) +
        theme_void() +
        theme(legend.position = "none") +
        scale_color_gradient(low = "#56B1F7", high = "#132B43")
      axis_p <- ggplotly(tooltip = "none")
      subplot(tour_p, axis_p)
    })
    
    output$index <- renderPlotly({
      ggplot(pp_index, aes(x=iteration, y=index)) +
        geom_point() +
        geom_line()
      ggplotly(source = "index_plot")
    })
    
    output$tour2 <- renderPlotly({
      s <- event_data("plotly_click", source = "index_plot")
      #print(s)
      if (length(s)) {
        iter <- s$x
      } else {
        iter <- 1
      }
      proj <- as.data.frame(t_tour[[iter]][[1]])
      proj$ID <- rownames(dataset)
      p <- ggplot(proj, aes(x=V1, y=V2, label=ID, col=col$col, label1=col$group)) +
        geom_point() +
        scale_x_continuous(limits = c(-0.1, 1.1)) +
        scale_y_continuous(limits = c(-0.1, 1.1)) +
        theme_void() +
        theme(legend.position = "none")
      tour_p <- ggplotly(p, tooltip=c("label", "label1")) 
      basis <- as.data.frame(matrix(tinterp[[iter]], ncol = 2))
      basis$measure <- colnames(Xdataset)
      basis$magnitude <- sqrt((basis$V1)^2+(basis$V2)^2)
      axis_p <- ggplot(basis, aes(x=V1, y=V2)) +
        geom_segment(aes(xend=0, yend=0, col=magnitude)) +
        geom_text(aes(label=measure)) +
        scale_x_continuous(limits = c(-1.1, 1.1)) +
        scale_y_continuous(limits = c(-1.1, 1.1)) +
        theme_void() +
        theme(legend.position = "none") +
        scale_color_gradient(low = "#56B1F7", high = "#132B43")
      axis_p <- ggplotly(tooltip = "none")
      subplot(tour_p, axis_p)
    })
  }
  shinyApp(ui, server)
}


# Testing -----------------------------------------------------------------
data("crabs")
pp2D_shiny(crabs)
pp2D_shiny(crabs, group = "sex")
pp2D_shiny(crabs, index = "holes", group = "sex")
