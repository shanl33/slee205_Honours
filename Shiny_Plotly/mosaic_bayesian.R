library(shiny)
devtools::install_github("ropensci/plotly")
library(plotly)
library(Bolstad)
install.packages("productplots")
library(productplots)

mu <- 1:6
prior <- c(.2, .1, .2, .1, .1, .3)
results <- poisdp(2, mu, prior)
str(results)
posterior <- results$posterior
lik <- results$likelihood

# Setting up possible y-values for x-axis range 
if (max(mu)>3) {
  max_y <- ceiling(max(mu)+4*sqrt(max(mu)))
} else {
  max_y <- ceiling(max(mu)+9)
}

# Y is a vector of the possible observed values (for each mu)
Y <- rep(0:max_y, times = (max(mu)-min(mu)+1))
# lik will be a vector for likelihoods, P(Y|mu)
lik <- c()
# 'mean' will be the mu value corresponding to each likelihood 
mean <- c()
# p_mu will be the prior prob for the corresponding mu (and lik)
p_mu <- c()
for(i in 1:length(mu)) {
  lik <- c(lik, dpois(0:max_y, mu[i]))
  mean <- c(mean, rep(mu[i], times = max_y+1))
  p_mu <- c(p_mu, rep(prior[i], times = max_y+1))
}

probdf <- data.frame(Y, lik, mean, p_mu)
# joint is the joint prob P(Y n mu) 
# This is proportional to the posterior P(mu|Y)
probdf$joint <- probdf$lik*probdf$p_mu
# head(probdf)
probdf$Y <- as.factor(probdf$Y)
probdf$mean <- as.factor(probdf$mean)
# prodplot works if ggmosaic has NOT been loaded.
prodplot(probdf, joint ~ mean + Y, c("vspine", "hspine")) + aes(fill=mean)

# Create same plot using prodcalc() and ggplot2:
mosaic_coords <- prodcalc(probdf, joint ~ mean + Y, c("vspine", "hspine"))
# head(mosaic_coords)
# sum(mosaic_coords[mosaic_coords$level==2, ".wt"])
# str(mosaic_coords)
# .wt in mosaic_coord is the joint prob
p <- ggplot(mosaic_coords[mosaic_coords$level==2,], aes(label=Y, label1=mean)) +
  geom_rect(aes(xmin=l, xmax=r, ymin=b, ymax=t, fill=.wt)) +
  scale_color_discrete (c=0, l=100) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())
ggplotly(p)


# Shiny app w Plotly click ------------------------------------------------
ui <- fluidPage(
  mainPanel(
    plotlyOutput("mosaic"),
    plotlyOutput("scatter")
  )
)

server <- function(input, output, session) {
  mosaic_coords <- reactiveValues()
  # Setting up possible Y-obs values
  if (max(mu)>3) {
    max_y <- ceiling(max(mu)+4*sqrt(max(mu)))
  } else {
    max_y <- ceiling(max(mu)+9)
  }
  # Y2 is a vector of the possible observed values (for each mu)
  Y2 <- rep(0:max_y, times = (max(mu)-min(mu)+1))
  # lik will be a vector for likelihoods, P(Y|mu)
  lik2 <- c()
  # 'mean' will be the mu value corresponding to each likelihood 
  mean2 <- c()
  # p_mu will be the prior prob for the corresponding mu (and lik)
  p_mu2 <- c()
  for(i in 1:length(mu)) {
    lik2 <- c(lik2, dpois(0:max_y, mu[i]))
    mean2 <- c(mean2, rep(mu[i], times = max_y+1))
    p_mu2 <- c(p_mu2, rep(prior[i], times = max_y+1))
  }
  # Collect all the info to be plotted together.
  probdf2 <- data.frame("Y2" = as.factor(Y2), 
                        "mean2" = as.factor(mean2),
                        "joint" = lik2*p_mu2)
  mosaic_coords <- prodcalc(probdf2, joint ~ mean2 + Y2, c("vspine", "hspine"))
  output$mosaic <- renderPlotly({
    # level = 1 or 2 since 2 vars involved. Need to plot only level 2
    mosaic_coords2 <- mosaic_coords[mosaic_coords$level==2,]
    p <- ggplot(mosaic_coords2, aes(label=Y2, label1=mean2)) +
      geom_rect(aes(xmin=l, xmax=r, ymin=b, ymax=t, fill=.wt)) +
      scale_color_discrete (c=0, l=100) +
      theme(panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank())
    ggplotly(p, source = "mos") 
  })
  # Scatterplot2 will display brushed subset from sp1.
  output$scatter <- renderPlotly({
    t <- event_data("plotly_click", source = "mos")
    if (length(t)) {
    mosaic_coords2 <- mosaic_coords[mosaic_coords$level==2,]
    # Order by wt (joint probs)
    mosaic_coords2 <- mosaic_coords2[order(mosaic_coords2[,2]),]
    # event_data() listens for the "selected" event in the "source".
    lev <- t$curveNumber + 1
    # "lev" is the ranking by .wt (joint prob) for the mosaic rectangle
    y_obs <- as.numeric(as.character(mosaic_coords2[lev, 1]))
    a <- poisdp(y_obs, mu, prior, plot = FALSE) 
    results <- data.frame("iteration"=as.factor(rep.int(1, length(mu))), 
                          "mu"=as.factor(mu), "p"=a$posterior, 
                          "Y"=rep.int(y_obs, length(mu)))
    ggplot(results, aes(x=iteration, y=p, label=Y)) +
      geom_point(aes(color=mu)) +
      labs(x="Iteration", 
           y="Posterior P(mu|y)", color="Mean")
    ggplotly()
    } else {
      plotly_empty()
    }
  })
}

shinyApp(ui, server)
