library(rggobi)
# Get data from R to GGobi
g <- ggobi(mtcars)
# Get this data back to R (either will work)
## Returns GGobiData objects that are linked to GGobi (so will update with interactions in GGobi)
## To get a 'static copy' of the data into R use as.data.frame()
g[[1]]
g[["mtcars"]]

### Adding more data to the existing GGobi session
### g$mtcars2 <- mtcars
### g[[2]]

# Control GGobi displays from R
## Parallel plot using all vars of mtcars
d <- display(g[1], "Parallel Coordinates Display")
# variables() lists the assignment of df vars to X, Y and Z vars in the current GGobi display
# variables() <- allows you to INTERACT with the active GGobi display 
# For Parallel plots the code below changes the variables on the axes and their order

# Cannot seem to explicitly order the vars: Will order subset in ascending order of numbers 
d <- display(g[1], "Parallel Coordinates Display")
variables(d)
variables(d) <- list(X=c(2, 8, 1)) #Displays mpg(1), cyl(2), vs(8)

# The order in which you call up the variables() <- assignments results in different orderings of the axes
# Compare Example A and Example B (Order of X=#:# matters) Sometimes (See Example C)
# Compare Example B and Example C (Order of variable()<- assignment commands matter)
# The final subsetted and reordered vars in display is determined by the combination of commands
# Example A
d <- display(g[1], "Parallel Coordinates Display")
variables(d)
variables(d) <- list(X=8:6) # Displays wt(6), qsec(7), vs(8)
variables(d) <- list(X=8:1) 
variables(d) # Displays wt(6), qsec(7), vs(8), then drat(5), vs(4), .., mpg(1)

# Example B
d <- display(g[1], "Parallel Coordinates Display") #Restart a new active display
variables(d)
variables(d) <- list(X=8:6) # Displays wt(6), qsec(7), vs(8) (same result if use X=6:8)
variables(d) <- list(X=1:8) 
variables(d) # Displays wt(6), qsec(7), vs(8), then mpg(1), cyl(2), .., drat(5)

# Example C
d <- display(g[1], "Parallel Coordinates Display") #Restart a new active display
variables(d)
variables(d) <- list(X=1:8) # Displays mpg(1), cyl(2), ..., vs(8) (same result if use X=8:1)
variables(d) <- list(X=6:8) # Same result if using X=8:6 
variables(d) # Displays wt(6), qsec(7), vs(8)

# Not working at the moment  ------------------------------------------------------------
# Rggobi is more out-of-date version of rggobi?
# See GGobiMeetsR(2001) Figure 2: Control panel looks different for example use Rggobi
# ExtendingGGobi(2007/8) and IntroGGobi doesn't use Rggobi
# But Rggobi has more functions, getData.ggobi()? 
# Replaced w ggobi_find_file()? But this is not working
install.packages("Rggobi") 
#Not available for R version 3.3.2 (currently on uni lab machines)
# Get data from GGobi to R 
## Load it in GGobi first
library(Rggobi)
ggobi("mktg")
# Opens GGobi control window but does not seem to read the olive data into GGobi
