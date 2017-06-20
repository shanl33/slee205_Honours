# Data from NZQA website:
#http://www.nzqa.govt.nz/studying-in-new-zealand/secondary-school-and-ncea/find-information-about-a-school/secondary-school-statistics/consolidated-files/data-files-for-2016/
# School (CSV, 674KB) data set
# Current achievement rate includes only if obtained in 2016.
# Cumulative = obtained any time prior to and including 2016.
# Current achievement will not include students who achieved in the previous yr
# Participation = achievement rate is based on only students who have been entered in enough credits to potentially achieve the certificate
# Roll = achievement rate is based on all students who are officially on their roll (whether taking enough credits to be considering L1/2/3/schol or not)
# Use Current.Achievement.Rate.Participation/Roll with respective Year.Level and Qualification 
# ie. Year 11 with L1, Year 12 with L2, Year 13 with L3 and UE

# Problem: Which of the factors, Decile, Region,
#affect a typical-sized SCHOOL's success at NCEA Level 1, 2, 3 and UE?
# Remove schools with 'small cohort' warning. 
# Ethnicity group sizes not given so cannot offset proportions appropriately to incl in analysis.

# Wrangling data ----------------------------------------------------------
library(dplyr)
library(tidyr)
nzqa2016 <- read.csv("http://www.nzqa.govt.nz/assets/Studying-in-NZ/Secondary-school-and-NCEA/stats-reports/2016/Qualification-Statistics-School-2016-29032017.csv")
head(nzqa2016)
tail(nzqa2016)
nzqa2016[5600:5616,]
str(nzqa2016)
summary(nzqa2016) #480 levels for schools but not all a really schools (see tail)
# Drop vars that will not be used (eg. cumulative achievement)
nzqa <- nzqa2016[,-c(1,8,10)]
names(nzqa) <- c("Decile", "Region", "School", "Year", "Qualification", "Achieve_participate", "Achieve_roll", "Small_sch")
levels(nzqa$Qualification) <- c("L1", "L2", "L3", "UE")
# Subset to use only Year 11 with Level 1, etc
nzqa <- nzqa %>% filter(((Qualification=="L1")&(Year==11))|((Year==12) & (Qualification=="L2"))|
                      ((Year==13) & (Qualification=="L3"))|((Year==13) & (Qualification=="UE")))
# Check subseting
head(nzqa)
tail(nzqa)
str(nzqa)
summary(nzqa)
# Reshape so that one row = one school
# Current.Achievement.Rate.Participation kept for analysis only
achieved <- nzqa %>% 
  spread(Qualification, Achieve_participate, fill=0) %>%
  #select(School, L1:L3, UE) %>%
  group_by(School) %>%
  summarise_at(c("L1", "L2", "L3", "UE"), sum) %>%
  inner_join(nzqa[, c(1, 2, 3, 8)]) %>% #Add Decile and Region and Small_sch variables
  distinct() %>% #One row per school
  filter(!((L1==0)&(L2==0)&(L3==0))) #Remove schools with 0% achievement rate for all levels
summary(achieved)
# Schools with decile=zero?
achieved[achieved$Decile==0,] # eg.MIT
str(achieved)
head(achieved)
# Function to replace 0% with NA
zeros <- function(col) {
  replace(col, col==0, NA)
}
achieved$L1 <- zeros(achieved$L1)
achieved$L2 <- zeros(achieved$L2)
achieved$L3 <- zeros(achieved$L3)
achieved$UE <- zeros(achieved$UE)
# Remove schools with 'small cohort' warning (obscures pattern in non-small cohorts).
achieved <- achieved[achieved$Small_sch=="",] #437 school left
achieved <- achieved[, -8]
write.csv(achieved, "/Users/shanlee/Desktop/Project/slee205_Honours/Datasets/achieved.csv", row.names = F)

# Visuals analysis ----------------------------------------------------------------
load("/Users/shanlee/Desktop/Project/slee205_Honours/Datasets/achieved.csv")
# achieved.csv contains schools with ONE or more % achievement rate (by participation)
# NA's used otherwise
# Small cohort schools removed
# Static pairwise plot
plot(achieved)
plot(achieved[,c(2:6)])
# Generally positive relationship, but variance in scatter to explore further
# Box plots for Decile and Region
# Facet wrap

# Unsupervised cluster analysis: 
# Explore whether Decile and/or Region explains any structure that is 'naturally' present
# Remove obs with any NA values (Otherwise errors in dist())
ach_narm <- achieved[complete.cases(achieved),] #407 schools left
library(MASS)
# NB: method="complete": Dissimilarity clusters = MAXIMUM of dissimilarities between members
# Dissimilarity clusters = AVERAGE of dissimilarities between members
h <- hclust(dist(ach_narm[,2:4]), method = "average")
plot(h)
# Two schools are clearly separated from the rest (65 and 339?)
# A couple of schools also a bit different from the two main clusters (345 & 49?)
# Difficult to read obs #s on dendogram (interactive: move points or hover identify?)
# Above could be outliers rather than clusters.
# Two possible clusters, one very large, possibly little distinction between the two clusters.
# In the larger cluster there is a group (at the end of dendogram) that may be a bit different

# Parallel plot may pick up of the 4 possilbe outliers (404 too)
# Ordered 'naturally' by level of qualification 
# Decile at the end since UE and Decile relationship would be most interesting
library(GGally)
achieved$Decile <- factor(achieved$Decile)
# Aligned as 'raw' data since % values
ggparcoord(data = achieved, columns = 6:2, scale = "uniminmax", groupColumn = "Region")
# Align at median, scaled at each individual max and min 
ggparcoord(data = achieved, columns = 6:2, scale = "center", groupColumn = "Region",
           scaleSummary = "median")
# To do:
# Slider for Decile rather than plotly click
#'Heatmap matrix' of Euclidean distance as well as Correlation distance to select parallel plot with
# Correlation: identifies (dis)similarities in STRUCTURE
# Euclidean: identifies (dis)similarities in MAGNITUDE (appropriate for % V&R p338)
# See Cook pg105-6