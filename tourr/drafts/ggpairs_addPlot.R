# PCA plots:
# http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/SPSS_SC/Module9/M9_CATPCA/SPSS_M9_CATPCA.htm
# https://stats.stackexchange.com/questions/119746/what-is-the-proper-association-measure-of-a-variable-with-a-pca-component-on-a/119758#119758

# ggpairs inserting a plot
data("mtcars")
custom_car <- ggpairs(mtcars[, c("mpg", "wt", "cyl")], upper = "blank", title = "Custom Example")
custom_car
plot <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) +
  geom_text(aes(colour=factor(cyl)))
plot
custom_car[1, 2] <- plot
custom_car
ggplot(data.frame(x=1:5, y=1:5), aes(x=x, y=y)) + geom_point()
