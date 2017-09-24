# For wrangling data
library(dplyr)
library(tidyr)
# For reading xls binary file for 2016
library(gdata)

# Schools demographics (updated Sep 2017) ------------------------------------------------------------
# Download file from internet (school data updated 2017 Sept)
download.file("https://www.educationcounts.govt.nz/__data/assets/file/0003/62571/Directory-School-current.csv", "schools.csv")
# Explore structure as text
readLines("schools.csv", n=5)
# Metadata in first 3 lines, variable names in 4th line.
# Read file into R as data frame
schools <- read.csv("schools.csv", skip=3, header=T)
# Or if read into R as data frame, directly from internet.
# schools <- read.csv("https://www.educationcounts.govt.nz/__data/assets/file/0003/62571/Directory-School-current.csv", skip=3, header=T)
head(schools)
# Tidy up variable names
raw.cols <- readLines("schools.csv", n=4)[4] # Keep the fourth row only
colnames(schools) <- gsub("[^a-zA-z0-9]", "", strsplit(raw.cols, ",")[[1]])
summary(schools)
colnames(schools)
# Subset relevant variables
schools <- schools[c(2, 10, 15, 16, 33:39)]
# Keep only schools with secondary level students
secondary <- schools %>%
  filter(!((SchoolType=="Full Primary")|(SchoolType=="Intermediate")))
head(secondary)

# NCEA data -------------------------------------------------
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
achieved[2:5] <- sapply(achieved[2:5], zeros)
# Remove schools with 'small cohort' warning (obscures pattern in non-small cohorts).
achieved <- achieved[achieved$Small_sch=="",] #437 school left
achieved <- achieved[, -8] #Remove Small_sch var
# 'achieved' contains schools with ONE or more % achievement rate (by participation)
# NA's used otherwise
# Small cohort schools removed
# Remove obs with any NA values 
ncea <- achieved[complete.cases(achieved),] #407 schools left
ncea$Decile <- as.factor(ncea$Decile)
rownames(ncea) <- ncea$School
head(ncea)

# Merge schools and NCEA 2016 results -------------------------------------
ncea.sch <- merge(ncea, schools, by.x="School", by.y="Name") # 349 schools left
head(ncea.sch)
summary(ncea.sch)
colnames(ncea.sch)
# Change ethinicity counts to proportions
# MELAA refers to Middle Eastern/Latin American/African
ncea.sch[12:17] <- sapply(ncea.sch[12:17], function(x) {round(x/ncea.sch$TotalSchoolRoll, 2)})
ncea.sch$Other <- 1 - Reduce("+", ncea.sch[12:16]) 
# Range of school sizes (from 88 to around 4500)
summary(ncea.sch$TotalSchoolRoll)

akl.sch <- as.data.frame(ncea.sch[ncea.sch$Region=="Auckland", ]) 
# 70 schools for AKL compared to 90 when not merged with school demographic data
rownames(akl) <- akl$School
akl <- akl[-1]


# EdCounts data for 2016 (with size by Yr groups) -------------------------
download.file("https://www.educationcounts.govt.nz/__data/assets/excel_doc/0005/152609/Student-rolls-by-School-2010-2016.xlsx", "schools.xlsx")
# Takes a long time to extract
schools2016 <- read.xls("schools.xlsx", sheet="2016", skip=2, header=T)
# Download single worksheet doesn't seem to work directly (get errors about file)
#schools2016 <- read.xls(xls="https://www.educationcounts.govt.nz/__data/assets/excel_doc/0005/152609/Student-rolls-by-School-2010-2016.xlsx", sheet="2016", skip=2, header=T)
colnames(schools2016)
# Subset relevant variables
schools2016 <- schools2016[c(2, 4, 10, 13:18, 34:36)]
# Tidy up variable names
colnames(schools2016)[9] <- "EuropeanPakeha"
# Keep only schools with secondary level students
secondary2016 <- schools2016 %>%
  filter(!((Type=="Full Primary")|(Type=="Intermediate")))
head(secondary2016)

# Merge schools2016 and NCEA 2016 results -------------------------------------
nzqa.sch <- merge(nzqa, schools2016, by.x="School", by.y="School.Name") # 347 schools left
head(nzqa.sch)
colnames(nzqa.sch)
summary(nzqa.sch)
# Change ethinicity counts to proportions
# MELAA refers to Middle Eastern/Latin American/African
nzqa.sch[10:15] <- sapply(nzqa.sch[10:15], function(x) {round(x/nzqa.sch$Total, 2)})
nzqa.sch$Other <- 1 - Reduce("+", nzqa.sch[c(10:13, 15)]) 
# Minimum cohort size for Yr 11 to 13
nzqa.sch$Min.Cohort <- sapply(1:nrow(nzqa.sch), function(x) {min(nzqa.sch[x, 16:18])})
