library(shiny)
library(plotly)
library(Bolstad)

# Troubleshooting ---------------------------------------------------------
# If you have problems with Plotly tooltips appearing please refresh the page
# Using a web browser that is NOT Internet Explorer may also help (preferably use Safari)

# Shiny app w Plotly interaction ------------------------------------------
ui <- fluidPage(navbarPage("Poisson with discrete prior", 
        # User input (Tab 1)
        tabPanel("User input",
                 helpText("This app allows users to directly enter a prior distribution or use a point estimate for the parameter to simulate a prior distribution."),
        helpText("Choose either the `User input` or `Simulation` tab to define a prior distribution for use with the `poisdp` function in the Bolstad R package.",
                 "The latter is useful when you have little information about the prior distribution."),
        # Help text for user input (Tab 1)
        helpText("Tips for the `User input` tab: Use the sliders to define a range for the mean values and then enter the prior probabilities in the input box."),
        helpText("A likelihood function plot will appear once the prior distribution is defined.",
                 "This plot reflects the `tug of war` between the likelihood and prior probabilities.",
                 "Hover over the plot to view the likelihood (`lik`) of an observation, `y`, given a mean value, `mu`, as the parameter.", 
                 "The colour scale used for each likelihood function reflects the prior probability (`p_mu`) of the respective mean value."),
        helpText("Click on a point on the likelihood function plot, to input an observed value, `y`.",
                "The prior and posterior distributions will then be compared in a stacked bar plot and summary table."),
        helpText("The `Update posterior as prior` button, triggers a new iteration where the current posterior becomes the new prior distribution."),
        
          sidebarLayout(
            sidebarPanel(
            # Enter mu for prior (slider)
            sliderInput("mu", "Enter are range of mean values for prior:", min = 1, max = 20, value = c(1, 5), step = 1),
            
            # Text input box for prior probabilities (set default value as equally likely)
            textInput("prior", "Enter prior probabilities (comma delimited):",
                      value = "0.2, 0.2, 0.2, 0.2, 0.2", width = 800),
            
            # Error/warning messages for prior probabilities
            verbatimTextOutput("result.text"),
            
            # Instructions for interaction with plot and displays Y value chosen
            verbatimTextOutput("instructions"),
            
            # Button to submit posterior as the "new" prior distn
            actionButton("new", "Update posterior as prior")
        ),
        mainPanel(
            splitLayout(cellWidths = c("65%", "35%"),
                plotlyOutput("poisdstn"),
                plotlyOutput("stackedBars")
            ),
            splitLayout(tableOutput(""), tableOutput(""),
                tableOutput("table")
            )
        )
    )),
    # Simulation (Tab 2)
    tabPanel("Simulation",
             helpText("This app allows users to directly enter a prior distribution or use a point estimate for the parameter to simulate a prior distribution."),
             helpText("Choose either the `User input` or `Simulation` tab to define a prior distribution for use with the `poisdp` function in the Bolstad R package.",
                      "The latter is useful when you have little information about the prior distribution."),
             # Help text for simulation (Tab 2)
             helpText("Tips for the `Simulation` tab: Enter a single-value estimate for the mean and a sample size to use for each bootstrap iteration.",
                      "A small sample size is recommended for capturing plausible deviations from your single-value estimate,",
                      "since future observations (`y` values) will further refine the simulated prior distribution."),
             helpText("A likelihood function plot will appear once the simulation is complete and the prior distribution defined.",
                      "This plot reflects the `tug of war` between the likelihood and prior probabilities.",
                      "Hover over the plot to view the likelihood (`lik`) of an observation, `y`, given a mean value, `mu`, as the parameter.", 
                      "The colour scale used for each likelihood function reflects the prior probability (`p_mu`) of the respective mean value."),
             helpText("Click on a point on the likelihood function plot, to input an observed value, `y`.",
                      "The prior and posterior distributions will then be compared in a stacked bar plot and summary table."),
            helpText("The `Update posterior as prior` button, triggers a new iteration where the current posterior becomes the new prior distribution."),
        sidebarLayout(
            sidebarPanel(
              # Input of single-value estimate for mean (if the user's rate exceeds the max=20 the user can input a scaled-down equivalent rate. ie. input a "per week rate" rather than a "per month rate")
              numericInput("estimate","Choose a point estimate for the mean", 5, min = 0, max = 20),
              sliderInput("n",
                    "Choose a sample size, n, for simulation",
                    min = 10,
                    max = 100,
                    value = 20
              ),
              # Prior distn results from simulation
              tableOutput("prior.sim"),
              # Instructions for interaction with plot and displays Y value chosen
              verbatimTextOutput("instructions.sim"),
              # Button to submit posterior as the "new" prior distn for simulation
              actionButton("new.sim", "Update posterior as prior")
            ),
            mainPanel(
                splitLayout(cellWidths = c("65%", "35%"),
                    plotlyOutput("plot.sim"),
                    plotlyOutput("stackedBars.sim")
                ),
                splitLayout(tableOutput(""), tableOutput(""),
                            tableOutput("table2")
                )
            )
        )
    )
))

server <- function(input, output, session) {
  # Reactive values for user input (Tab 1)
  mu <- reactive({ seq(input$mu[1], input$mu[2]) })
  s <- reactive({event_data("plotly_click", source = "obs")})
  prior <- reactiveValues() # set by user input initially, then it will be updated by posterior.
  posterior.global <- reactiveValues() # posterior values to use as prior for next round.
  newButton.reactive <- reactiveValues() # to redraw plots when button clicked
  
  # Reactive values for simulation (Tab 2)
  mu.sim <- reactiveValues()
  s.sim <- reactive({event_data("plotly_click", source = "obs.sim")})
  prior.sim <- reactiveValues() # set by simulation, then it will be updated by posterior.
  posterior.global2 <- reactiveValues() # posterior values to use as prior for next round.
  new2Button.reactive <- reactiveValues() # to redraw plots when button clicked
  initial.sim <- reactiveValues() # initial prior probabilities from simulation
  # Messages for user input (Tab 1)
  # Error/warning messages for prior probabilities
  output$result.text <- renderText({
    prior.temp = as.numeric(unlist(strsplit(input$prior, ",")))
    # Check length and sum of input for prior probabilities
    resultNum <- (input$mu[2]-input$mu[1]+1)
    # Convert prior to a numeric vector
    if (any(is.na(prior.temp))) {
      result = "Invalid input"
    } else if(length(prior.temp) != resultNum) {
      result = paste0("The length of prior probabilities must be ", resultNum, ".")
    } else if(sum(prior.temp) != 1) {
      result = "The sum of prior probabilities must be 1."
    } else {
      result = "Click to enter the observed value"
    }
    result
  })
  # Instructions for Plotly click interaction to input observed value (Y)
  output$instructions <- renderPrint({
     if (length(s())!=0) {cat("Observed, y =", s()$x[1])} 
  })
  
  # Messages for simulation (Tab 2)
  # Results from simulation
  output$prior.sim <- renderTable({
    if(is.null(mu.sim$obj)) {
      results = NULL #not showing the table
    } else {
      results = data.frame(mu = as.factor(mu.sim$obj),
                           prior = initial.sim$obj) # table of prior from simulation
    }
    results
  }, caption = paste("Simulated prior"),
  caption.placement = getOption("xtable.caption.placement", "top"))
  
  # Instructions for Plotly click interaction to input observed value (Y)
  output$instructions.sim <- renderPrint({
    if (length(s.sim())!=0) {
      cat("Observed, y =", s.sim()$x[1])
    } else {
      cat("Click to enter the observed value")
    }
  })
  
  ###### Plots for user input (Tab 1) ######
  # Plotly plot to click on to input the observed value (Y)
  output$poisdstn <- renderPlotly({newButton.reactive$obj
    # Uses the initial prior (from the user input text box) if the "update button" has not been used
	if(is.null(posterior.global$posterior)) {
          prior$obj <- as.numeric(unlist(strsplit(input$prior, ",")))
	}
    # Setting up x-axis range for plot
    if(max(mu())>3) {
      max_y <- ceiling(max(mu())+4*sqrt(max(mu())))
    } else {
      max_y <- ceiling(max(mu())+9)
    }
    # Y is a vector of the possible observed values (for each mu)
    y <- rep(0:max_y, times = (max(mu())-min(mu())+1))
    # lik will be a vector for likelihoods, P(Y|mu)
    lik <- c()
    # 'mean' will be the mu value corresponding to each likelihood 
    mean <- c()
    # p_mu will be the prior prob for the corresponding mu (and lik)
    p_mu <- c()
    for(i in 1:length(mu())) {
      lik <- c(lik, dpois(0:max_y, mu()[i]))
      mean <- c(mean, rep(mu()[i], times = max_y+1))
      p_mu <- c(p_mu, rep(prior$obj[i], times = max_y+1))
    }
    # Dataframe to collect all the info to be plotted together.
    # Color scale is applied to p_mu, the prior prob of the mu value
    probdf <- data.frame(y, lik, mean, p_mu)
    # Plot of the likelihood distns for the prior mu values 
    ggplot(probdf, aes(x=y, y=lik, group = mean, color = p_mu)) +
      geom_point(size = 2) +
      geom_line() +
      scale_color_gradient(limits=c(0,1), 
                           low = "gray", high = "red", 
                           expression(P(mu))) +
      labs(x="Observed value (y)", 
           y="Likelihood P(y|mu)") +
      theme(panel.background = element_rect(fill = "white"))
    ggplotly(source = "obs")
  })
  
  # stackedBars comparing prior and posterior (labels need to be improved)
  output$stackedBars <- renderPlotly({newButton.reactive$obj 
      if (length(s())) {
      # 'a' is a list of the output from the Bolstad function, poisdp()
      a <- poisdp(s()$x, mu(), prior$obj, plot = FALSE)
      posterior.global <<- a

      # compare_distn contains both the prior and posterior distns
      compare_distn <- data.frame(mean=rep(mu(), 2), #mu values
                                  p=c(prior$obj, a$posterior), #probs for prior n posterior
                                  distn=c(rep("prior", length(mu())), 
                                          rep("posterior", length(mu()))))

      # distn is a factor with two levels ("prior" or "posterior")
      compare_distn$distn <- factor(compare_distn$distn, 
                                    levels = c("prior", "posterior"), 
                                    ordered = TRUE)

	# Stacked bar plot comparing prior and posterior probabilities
      ggplot(compare_distn) +
        geom_bar(aes(x=distn, y=p, fill=p, color=mean),
                 stat = "identity",
                 position = "fill",
                 show.legend = FALSE) +
        scale_fill_gradient(limits=c(0,1), low="gray", high="red") +
        scale_color_gradient(low = "black", high = "black") +
        theme(panel.background = element_rect(fill = "white"),
              axis.ticks = element_line(size = 0),
              axis.title = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = rel(1.5))) 
      ggplotly(tooltip = c("x", "fill", "colour"))
    } else {
      plotly_empty()
    }
  })
  
  ###### Plots for simulation (Tab 2) ######
  output$plot.sim = renderPlotly({new2Button.reactive$obj
    # Run simulation to obtain intial prior distn
    if(is.null(posterior.global2$posterior)) {
      lambda = c()
      for (j in 1:1000) {
        # Sample of random numbers from a poisson distn with the parameter=point estimate for mean
        x = rpois(input$n,input$estimate) 
        # Collect the sample mean from each iteration
        lambda = c(lambda, mean(x))
      }
      lambda = round(lambda, 0) # Round to an integer
      lambda = data.frame(table(lambda)) # Table of counts
      mu2 <- as.numeric(as.character(lambda$lambda))
      mu.sim$obj = mu2 
      prior2 <- as.numeric(as.character(lambda$Freq))/1000
      initial.sim$obj = prior2
      prior.sim$obj = prior2
    }
      # Setting up x-axis range for plot
    if(max(mu.sim$obj)>3) {
      max_y <- ceiling(max(mu.sim$obj)+4*sqrt(max(mu.sim$obj)))
    } else {
      max_y <- ceiling(max(mu.sim$obj)+9)
    }
    # Y is a vector of the possible observed values (for each mu)
    y <- rep(0:max_y, times = (max(mu.sim$obj)-min(mu.sim$obj)+1))
    # lik will be a vector for likelihoods, P(Y|mu)
    lik <- c()
    # 'mean' will be the mu value corresponding to each likelihood 
    mean <- c()
    # p_mu will be the prior prob for the corresponding mu (and lik)
    p_mu <- c()
    for(i in 1:length(mu.sim$obj)) {
      lik <- c(lik, dpois(0:max_y, mu.sim$obj[i]))
      mean <- c(mean, rep(mu.sim$obj[i], times = max_y+1))
      p_mu <- c(p_mu, rep(prior.sim$obj[i], times = max_y+1))
    }
      # Dataframe to collect all the info to be plotted together.
      # Color scale is applied to p_mu, the prior prob of the mu value
      probdf <- data.frame(y, lik, mean, p_mu)
      # Plot of the likelihood distns for the prior mu values 
      ggplot(probdf, aes(x=y, y=lik, group = mean, color = p_mu)) +
          geom_point(size = 2) +
          geom_line() +
          scale_color_gradient(limits=c(0,1), 
                               low = "gray", high = "red", 
                               expression(P(mu))) +
          labs(x="Observed value (y)", 
               y="Likelihood P(y|mu)") +
          theme(panel.background = element_rect(fill = "white"))
      ggplotly(source = "obs.sim")
  })
  
  output$stackedBars.sim = renderPlotly({new2Button.reactive$obj
      if (length(s.sim())) {
          # 'b' is a list of the output from the Bolstad function, poisdp()
          b <- poisdp(s.sim()$x, mu.sim$obj, prior.sim$obj, plot = FALSE)
          posterior.global2 <<- b
          # compare_distn contains both the prior and posterior distns
          compare_distn <- data.frame(mean=rep(mu.sim$obj, 2), #mu values
                                      p=c(prior.sim$obj, b$posterior), #probs for prior n posterior
                                      distn=c(rep("prior", length(mu.sim$obj)), 
                                              rep("posterior", length(mu.sim$obj))))
          
          # distn is a factor with two levels ("prior" or "posterior")
          compare_distn$distn <- factor(compare_distn$distn, 
                                        levels = c("prior", "posterior"), 
                                        ordered = TRUE)
          
          # Stacked bar plot comparing prior and posterior probabilities
          ggplot(compare_distn) +
              geom_bar(aes(x=distn, y=p, fill=p, color=mean),
                       stat = "identity",
                       position = "fill",
                       show.legend = FALSE) +
              scale_fill_gradient(limits=c(0,1), low="gray", high="red") +
              scale_color_gradient(low = "black", high = "black") +
              theme(panel.background = element_rect(fill = "white"),
                    axis.ticks = element_line(size = 0),
                    axis.title = element_blank(),
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(size = rel(1.5))) 
          ggplotly(tooltip = c("x", "fill", "colour"))
      } else {
          plotly_empty()
      }
  })
  
  ###### Action buttons and Table for both tabs ######
  # Update action button for user input (Tab 1)
  observeEvent(input$new, {
    if(!is.null(posterior.global$posterior)) {
      prior$obj <- posterior.global$posterior # update prior (reactive variable) with posterior values from current round.
      newButton.reactive$obj <- Sys.time() # Once newButton.reactive values is updated, it will draw stackedBar again.    
    } else {
      # Do nothing
    }
  })
  # Update action button for simulation (Tab 2)
  observeEvent(input$new.sim, {
    if(!is.null(posterior.global2$posterior)) {
      prior.sim$obj <- posterior.global2$posterior # update prior (reactive variable) with posterior values from current round.
      new2Button.reactive$obj <- Sys.time() # Once newButton.reactive2 values is updated, it will draw stackedBar again.    
    } else {
      # Do nothing
    }
  })
  # Summary table of prior and posterior for user input (Tab 1)
  output$table<-renderTable({newButton.reactive$obj
    if(is.null(s()$x)) {
      results = NULL #not showing the table
    } else {
      a <- poisdp(s()$x, mu(),prior$obj) # reactive the posterior 
      results = data.frame(mu = mu(),
                           prior = prior$obj,
                           posterior= a$posterior)# table output with mean, prior and posterior   
    }
    results
  })
  # Summary table of prior and posterior for simulation (Tab 2)
  output$table2<-renderTable({new2Button.reactive$obj
    if(is.null(s.sim()$x)) {
      results = NULL #not showing the table
    } else {
      b <- poisdp(s.sim()$x, mu.sim$obj,prior.sim$obj) # reactive the posterior
      results = data.frame(mu = as.factor(mu.sim$obj),
                           prior = prior.sim$obj,
                           posterior= b$posterior)# table output with mean, prior and posterior   
    }
    results
  })
}
shinyApp(ui, server)