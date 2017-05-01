install.packages("productplots")
library(productplots)
library(ggplot2)
library(dplyr)
library(ggvis)
library(plotly)

# If load ggmosaic then will need to re-install productplots
# Currently cannot get ggmosaic working
install.packages("ggmosaic")
library(ggmosaic)
# See https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html

data("happy")
str(happy)
# See: https://github.com/hadley/productplots/tree/master/R

# 1 var -------------------------------------------------------------------
prodplot(happy, ~ happy, "hspine")
# 1 var and coords --------------------------------------------------------
# Compare coords for below
prodcalc(happy, ~ happy, "hspine")
prodcalc(happy, ~ happy, "hbar")
# Coordinates: l, r, b, t (left, right, bottom, top for each rectangle)
# Coords can be passed to ggplot2 to re-create graphs
hspine_coords <- prodcalc(happy, ~ happy, "hspine")
str(hspine_coords)
ggplot(hspine_coords) + geom_rect(aes(xmin=l, xmax=r, ymin=b, ymax=t, fill=happy))

# 2 vars ------------------------------------------------------------------
# Compare: Order of vars determines which divider is applied to which
# The direction of the divider (h or v) decides which axes the var labels correspond to
# 'h' horizontal dividers divide up the horizontal x-axis
# Change hbar to hspine to see effect
prodplot(happy, ~ sex + happy, c("vspine", "hspine")) + aes(fill=sex)
prodplot(happy, ~ happy + sex, c("vspine", "hbar")) + aes(fill=happy)
prodplot(happy, ~ happy + sex, c("hbar", "vspine")) + aes(fill=happy)
# Below same as: prodplot(happy, ~ sex + happy, c("vspine", "hspine")) + aes(fill=sex)
prodplot(happy, ~ sex + happy, mosaic()) + aes(fill=sex)
# stacked() best with only 2 vars (like a trellis plot)
prodplot(happy, ~ sex + happy, stacked())

# 2 vars + ggplot2 + plotly -------------------------------------------------------
prodplot(happy, ~ marital + happy, c("vspine", "hspine"), na.rm = TRUE) + aes(fill=marital)
mosaic2_coords <- prodcalc(happy, ~ marital + happy, c("vspine", "hspine"), na.rm = TRUE)
str(mosaic2_coords) # level = 1 or 2 since 2 vars involved. Need to plot only level 2
p <- ggplot(mosaic2_coords[mosaic2_coords$level==2,]) + 
  geom_rect(aes(xmin=l, xmax=r, ymin=b, ymax=t, fill=marital, color=happy)) +
  scale_color_discrete (c=0, l=100) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(),
        axis.text.x = element_blank())
ggplotly(p, tooltip = c("fill", "color")) 

# With geom_mosaic (ggmosaic package) currently does NOT work
ggplot(happy) +
  geom_mosaic(aes(x = product(happy, sex)))
  
# 2 vars and coords and ggvis interaction ---------------------------------
# using mosiac2_coords from above
mosaic2_coords[mosaic2_coords$level==2,] %>%
  ggvis(x=~l, x2=~r, y=~b, y2=~t, fill=~marital) %>% 
  layer_rects() %>%
  add_tooltip(function(mosaic2_coords) mosaic2_coords[,1]) %>%
  add_tooltip(function(mosaic2_coords) mosaic2_coords$marital)
# ggvis will only show one tooltip at a time and will not show 'happy' status since it's not 'connected' to the coords.

# 3 vars ------------------------------------------------------------------
# Conditional: distn of happiness and gender, given their health status
# mosaic usually uses vertical spines first by defaulth
prodplot(happy, ~ happy + sex | health, mosaic("h")) + aes (fill=happy)
# different to below: distn of happiness, gender and health status 
# (widths not fixed, vary according to health status)
prodplot(happy, ~ happy + sex + health, mosaic("h")) + aes (fill=happy)

prodplot(happy, ~ marital + sex + happy, stacked()) + aes(fill = marital)
prodplot(happy, ~ marital + sex + happy, stacked(), level = 3) 
# level = 3 is complete plot, level = 2 is sex + happy, level = 1 is happy only

prodplot(happy, ~ happy + marital + sex, c("vspine", "hspine", "hspine")) + aes(fill = happy)
# 'level' shows how the plot was built up: partition by sex first
prodplot(happy, ~ happy + marital + sex, c("vspine", "hspine", "hspine"), level = 1)
# Then sex and marital status
prodplot(happy, ~ happy + marital + sex, c("vspine", "hspine", "hspine"), level = 2)
# Different order of nesting to above, fill color = last level/first variable is best
prodplot(happy, ~ marital + happy + sex, c("hspine", "vspine", "hspine")) + aes(fill = marital)
# Level 1 is same as above, but not level 2
prodplot(happy, ~ marital + happy + sex, c("hspine", "vspine", "hspine"), level = 2)
# Sex layered last
prodplot(happy, ~ sex + happy + marital, c("hspine", "vspine", "hspine")) + aes(fill = sex)
prodplot(happy, ~ sex + happy + marital, c("hspine", "vspine", "hspine"), level = 2) + aes(fill=happy)
# level = 2, same as below (mosaic = vspine, hspine,.. alternating)
# Default is to NOT remove NA's (na.rm=FALSE)
prodplot(happy, ~ happy + marital, mosaic(), na.rm = TRUE) + aes(fill = happy)
