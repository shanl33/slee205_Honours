

t2 <- save_history(ach_narm[,2:5], guided_tour(cmass, d=2, max.tries = 50), sphere=TRUE, max=50)
t2interp <- interpolate(t2) 
ach_sphere2 <- attr(t2, "data") #(n by p) = (407 by 4)
max_axis <- 0
min_axis <- 0
cmass_tour <- function(basis) {
  # Projected data matrix 
  XA <- ach_sphere2%*%matrix(basis, ncol=2) # (n by d) = (407 by 2)
  cmass_index <- (sum(exp(-0.5*diag(XA%*%t(XA))))/dim(XA)[1]-exp(-dim(XA)[2]/2))/(1-exp(-dim(XA)[2]/2))
  list(rescale(XA), cmass_index)
}
# Apply function to each projection basis
t2_tour <- apply(t2interp, 3, FUN = cmass_tour) 
cmass_index2 <- data.frame(iteration=1:length(t2_tour))
for(i in 1:length(t2_tour)) {
  cmass_index2$index[i] <- unlist(t2_tour[[i]][2])
}

# Colour
group_t <- "Decile"
fac_t <- subset(ach_narm, select = group_t) 
fac_v <- as.factor(as.numeric(unlist(fac_t)))
pal_t <- rainbow_hcl(length(levels(fac_v))) # Compute a rainbow of colours (qualitative palette)
group_col_t <- as.factor(pal_t[as.numeric(fac_v)])
levels(group_col) <- levels(fac_v)

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

