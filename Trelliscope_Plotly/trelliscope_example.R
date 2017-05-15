devtools::install_github("hafen/trelliscopejs")

library(ggplot2)
library(trelliscopejs)

str(mpg)
qplot(class, cty, data = mpg, geom = c("boxplot", "jitter")) +
  facet_trelliscope(~ class, ncol = 7, height = 800, width = 200,
                    state = list(sort = list(sort_spec("cty_mean")))) +
  ylim(7, 37) + theme_bw()


# With rbokeh (and dplyr)-------------------------------------------------------------
library(rbokeh)
library(dplyr)
d <- mpg %>%
  group_by(manufacturer, class) %>%
  summarise(
    mean_city_mpg = mean(cty),
    mean_hwy_mpg = mean(hwy),
    panel = panel(
      figure(xlab = "City mpg", ylab = "Highway mpg",
             xlim = c(7, 37), ylim = c(9, 47)) %>%
        ly_points(cty, hwy,
                  hover = data_frame(model = paste(year, model),
                                     cty = cty, hwy = hwy))))
d %>%
  trelliscope(name = "city_vs_highway_mpg", nrow = 2, ncol = 4)

# With plotly? (and dplyr)------------------------------------------------------------
library(plotly)
# Works with facet_trelliscope 
qplot(cty, hwy, data = mpg) +
  xlim(7, 37) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ manufacturer + class, nrow=2, ncol=4, as_plotly = TRUE)

# Currently cannot get working with trelliscope() 
# Plots all of the data rather than the subset 
# if remove data=mpg in plot_ly arg then cannot find the vars cty and hwy
# Ideally want this so that can customise cognostics (summary stats for filtering etc)
# Should be similar to using rbokeh+trelliscope 
# See: above for easier example (from a blog by creater of trelliscopejs) 
# Harder <http://ryanhafen.com/blog/trelliscopejs> (same person as above)
p <- mpg %>%
  group_by(manufacturer, class) %>%
  summarise(
    mean_city_mpg = mean(cty),
    mean_hwy_mpg = mean(hwy),
    panel = panel(
      plot_ly(mpg, x=~cty, y=~hwy) %>% 
        layout(
          xaxis = list(range = c(7, 37)),
          yaxis = list(range = c(9, 47))
        )))
p
p %>%
  trelliscope(name = "testing_w_Plotly", nrow = 2, ncol = 4)
