# Data from NZQA website:
#http://www.nzqa.govt.nz/studying-in-new-zealand/secondary-school-and-ncea/find-information-about-a-school/secondary-school-statistics/consolidated-files/data-files-for-2016/
# School by Ethnicity (CSV, 2.3MB) data set
# Current achievement rate includes only if obtained in 2016.
# Cumulative = obtained any time prior to and including 2016.
# Current achievement will not include students who achieved in the previous yr
# Participation = achievement rate is based on only students who have been entered in enough credits to potentially achieve the certificate
# Roll = achievement rate is based on all students who are officially on their roll (whether taking enough credits to be considering L1/2/3/schol or not)
# Use Cumulative.Achievement.Rate.Participation for analysis 
#but interesting to see if there's a big diff btwn Roll and Participation achievement rate

# Problem: Which of the factors, Decile, Region, School, Ethnicity, 
#affect your success at NCEA Level 1, 2, 3 and scholarship?
# Problem: Which of the factors, Decile, Region, (Ethnicity), 
#affect a SCHOOL's success at NCEA Level 1, 2, 3 and scholarship?
nzqa2016 <- read.csv("http://www.nzqa.govt.nz/assets/Studying-in-NZ/Secondary-school-and-NCEA/stats-reports/2016/Qualification-Statistics-School-Ethnicity-2016-29032017.csv")
head(nzqa2016)
str(nzqa2016)
