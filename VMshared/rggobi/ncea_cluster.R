# For wrangling data
library(dplyr)
library(tidyr)
# For numerical cluster analysis method(s)
library(MASS)
# For graphical visualisation
library(rggobi)

# Wrangling data ----------------------------------------------------------
nzqa2016 <- read.csv("http://www.nzqa.govt.nz/assets/Studying-in-NZ/Secondary-school-and-NCEA/stats-reports/2016/Qualification-Statistics-School-2016-29032017.csv")
# Drop vars that will not be used (eg. cumulative achievement)
nzqa <- nzqa2016[,-c(1,8,10)]
names(nzqa) <- c("Decile", "Region", "School", "Year", "Qualification", "Achieve_participate", "Achieve_roll", "Small_sch")
levels(nzqa$Qualification) <- c("L1", "L2", "L3", "UE")
# Subset to use only Year 11 with Level 1, etc
nzqa <- nzqa %>% filter(((Qualification=="L1")&(Year==11))|((Year==12) & (Qualification=="L2"))|
                          ((Year==13) & (Qualification=="L3"))|((Year==13) & (Qualification=="UE")))
# Reshape so that one row = one school
# Current.Achievement.Rate.Participation kept for analysis only
achieved <- nzqa %>% 
  spread(Qualification, Achieve_participate, fill=0) %>%
  group_by(School) %>%
  summarise_at(c("L1", "L2", "L3", "UE"), sum) %>%
  inner_join(nzqa[, c(1, 2, 3, 8)]) %>% #Add Decile and Region and Small_sch variables
  distinct() %>% #One row per school
  filter(!((L1==0)&(L2==0)&(L3==0))) #Remove schools with 0% achievement rate for all levels
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
# 'achieved' contains schools with ONE or more % achievement rate (by participation)
# NA's used otherwise
# Small cohort schools removed


# Cluster analysis --------------------------------------------------------
# Aim: Partition student achievement data according to the decile of the school.
# Approach: Use a numerical method to model clusters
# Then graphical analysis to interpret and analyse clusters

# Numerical method: Heirarchical clustering using Euclidean distances and average linkage method
# Remove obs with any NA values (Otherwise errors in hclust())
ach_narm <- achieved[complete.cases(achieved),] #407 schools left
achieved.dist <- dist(ach_narm[,2:4])
achieved.dend <- hclust(achieved.dist, method = "average")
# Dendrogram plot
plot(achieved.dend)
# Cut dendrogram to produce 5 clusters (around height=0.4)
# We except 2 singleton clusters, 1 cluster w two cases, 1 large cluster (over 0.6 of cases), 1 smaller cluster
clust5 <- cutree(achieved.dend, k=5)
