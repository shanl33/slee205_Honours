library(Bolstad)
library(plotly) #requires ggplot2 and imports dplyr.
library(shiny)

ui <- fluidPage(
  verticalLayout(
    numericInput("p1", "P(mu=1)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p2", "P(mu=2)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p3", "P(mu=3)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p4", "P(mu=4)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p5", "P(mu=5)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p6", "P(mu=6)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p7", "P(mu=7)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p8", "P(mu=8)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p9", "P(mu=9)", 0, min = 0, max = 1, width = "10%"),
    numericInput("p10", "P(mu=10)", 0, min = 0, max = 1, width = "10%")
  )
  splitLayout(cellWidths = c("75%","25%"),
    plotlyOutput(outputId = "mu_pdf"),
    plotlyOutput(outputId = "prior_post")
    )
  verbatimTextOutput("observation")
)
# Input: prior distribution
mu <- c(1:6)
prior <- c(.1, .15, .25, .25, .15, .1)

# Input: Observed value(s) Example a). y=2   b). y=2, then y=3.
a <- poisdp(2, mu, prior, plot = 0)
b <- poisdp(c(2,3), mu, prior, plot = 1)
c <- poisdp(16, mu, prior, plot = 1)

# Mosaic plot for marginal distribution.
str(a) # Output always a list of 11 components.
# a$fun(m) obtains the cumulative posterior probabilities (m are the possible mean values ie. 1:6)
# compare_distn <- data.frame(mu, prior, post=a$posterior)
p_cum <- c(cumsum(prior), cumsum(a$posterior))
p_mid <- p_cum
for(n in c(2:length(mu), seq(length(mu)+2, 2*length(mu)))) {
  p_mid[n] <- (p_cum[n-1]+p_cum[n])/2 
}
p_mid[1] <- p_cum[1]/2
p_mid[length(mu)+1] <- p_cum[length(mu)+1]/2 
compare_distn <- data.frame(parameter=rep(mu, 2), 
                            p=c(prior, a$posterior),
                            distn=c(rep("prior", length(mu)), 
                                    rep("posterior", length(mu))),
                            p_mid)
compare_distn$distn <- factor(compare_distn$distn, 
                              levels = c("prior", "posterior"), 
                              ordered = TRUE)
# Heatmap for prior and posterior distns.
ggplot(compare_distn) +
  geom_bar(aes(x=distn, y=p, fill=p),
           stat = "identity",
           color = "black",
           show.legend = FALSE) +
  scale_fill_gradient(limits=c(0,1), low="light gray", high="red") +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_line(size = 0),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(2))
    ) + 
  geom_text(aes(x=distn, y=p_mid, 
                label = paste(expression(mu), parameter, sep = "=")))

# Poisson distn of posterior w shading based on probability for mu.
if(max(mu)>3) {
  max_y <- ceiling(max(mu)+4*sqrt(max(mu)))
} else {
  max_y <- ceiling(max(mu)+9)
}
Y <- rep(0:max_y, times = max(mu))
prob <- c()
lambda <- c()
p_lambda <- c()
for(i in min(mu):max(mu)) {
  prob <- c(prob, dpois(0:max_y, i))
  lambda <- c(lambda, rep(i, times = max_y+1))
  p_lambda <- c(p_lambda, rep(prior[i], times = max_y+1))
}
probdf <- data.frame(Y, prob, lambda, p_lambda)
ggplot(probdf, aes(x=Y, y=prob, group = lambda, color = p_lambda)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_gradient(limits=c(0,1), 
                       low = "white", high = "black", 
                       expression(P(mu))) +
  labs(x="Observed value (y)", 
       y=expression(paste("P(y|", mu, ")"))) +
  theme(panel.background = element_rect(fill = "white"))
# Plotly: Tooltip for displaying the conditional probs for the selected observed y.
