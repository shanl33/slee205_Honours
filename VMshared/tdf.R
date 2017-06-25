library(rggobi)
library(DescribeDisplay)
# DescribeDisplay plugin is NOT installed on GGobi for postgrad lab machines
# Load Tour de France 2013 data
tdf2013 <- read.delim("http://www.theusrus.de/Blog-files/TDF2013.txt")
head(tdf2013)
str(tdf2013)
summary(tdf2013)
colnames(tdf2013)
t <- ggobi(tdf2013)
# Cannot scale axes for parallelplots in GGobi
# This works in VM fine.
# Trial pull to VM.

library(GGally)
library(plotly)
library(dplyr)
# Stage times in cols 8 to 28
# ggparcoord can order variables by F-statistic, w groupColumn as the explanatory var
# Default axes in ggparcoord: 'std' standardised by mean and sd
ggparcoord(data = tdf2013, columns = 8:28)
# The usual default for pcps: Each var scaled individually
ggparcoord(data = tdf2013, columns = 8:28, scale = "uniminmax")
ggparcoord(data = tdf2013, columns = 8:28, scale = "globalminmax")
# Align at median, scaled at each individual max and min 
ggparcoord(data = tdf2013, columns = 8:28, scale = "center", 
           scaleSummary = "median")
# Cannot use align using median and use globalminmax as with Mondrian
# Unless you manually transform first by subtracting by the centering stat
as.data.frame(apply(tdf2013[,8:28], 2, 
                    function(x) x - median(x, na.rm = TRUE))) %>%
  ggparcoord(columns = 1:21, scale = "globalminmax")
ggplotly()
# Could link following histogram with pcp above
ggplot(data = tdf2013, aes(T21)) +
  geom_histogram(binwidth = 180)

