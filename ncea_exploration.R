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

# Problem: Which of the factors, Decile, Region, School, Ethnicity, 
#affect your success at NCEA Level 1, 2, 3 and scholarship?
# Problem: Which of the factors, Decile, Region, (Ethnicity), 
#affect a SCHOOL's success at NCEA Level 1, 2, 3 and scholarship?

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
  select(School, L1:L3, UE) %>%
  group_by(School) %>%
  summarise_all(sum) %>%
  inner_join(nzqa[, 1:3]) %>% #Add Decile and Region variables
  distinct() %>% #One row per school
  filter(!((L1==0)&(L2==0)&(L3==0))) #Remove schools with 0% achievement rate for all levels
summary(achieved) #467 schools left
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
# Static pairwise plot
plot(achieved)
plot(achieved[,c(2:6)])
write.csv(achieved, "/Users/shanlee/VirtualBox VMs/stats787/achieved.csv", row.names = F)
