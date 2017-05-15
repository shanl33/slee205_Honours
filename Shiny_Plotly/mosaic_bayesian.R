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
# Turn probs into counts out of 100
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
p <- ggplot(mosaic_coords[mosaic_coords$level==2,], aes(color=mean, label=Y)) +
  geom_rect(aes(xmin=l, xmax=r, ymin=b, ymax=t, fill=.wt)) +
  scale_color_discrete (c=0, l=100) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())
ggplotly(p)
