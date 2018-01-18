# Packages required
# For reading xls files (binary)
library(gdata)
# For interactive plots
library(ggplot2)
library(plotly)

# Download file
download.file("https://www.educationcounts.govt.nz/__data/assets/excel_doc/0005/152609/Student-rolls-by-School-2009-2017.xlsx", "schools.xlsx")
# Read in sheet with 2016 data only
schools2016 <- read.xls("schools.xlsx", sheet="2017", skip=3, header=FALSE)
# Error with xlsx file sheet names "2017" retrieves 2016 data and "2018" retrieves garbage
# Sheet names mislabelled by 1 year and no data for 2017

# Checking
head(schools2016)
levels(schools2016$V7) # Regional Council
levels(schools2016$V4) # Type (Primary, ...)
levels(schools2016$V19) # Last variable to keep (# Int fee paying)

# Auckland schools and keep only variables of interest
AKL2016 <- schools2016[schools2016$V7=="Auckland Region", c(2:6, 9:19)]
colnames(AKL2016) <- c("Name", "Decile", "Type", "Authority", "Gender", 
                       "Suburb", "Roll", "Female", "Male", "Maori", 
                       "Pasifika", "Asian", "MELAA", "Other", "European", "International")

# Secondary schools only
SecAKL2016 <- subset(AKL2016, grepl("^Secondary", AKL2016$Type))

# Drop schools with Decile > 10 
SecAKL2016 <- subset(SecAKL2016, Decile < 11)
# Drop unused levels for factors
SecAKL2016[, 3:6] <- droplevels(SecAKL2016[, 3:6])
# Trim names of factor variables' levels 
levels(SecAKL2016$Suburb) <- gsub("Auckland- ", "", levels(SecAKL2016$Suburb))
levels(SecAKL2016$Authority) <- c("Private", "Integrated", "Public")
levels(SecAKL2016$Type) <- c("Years_7-15", "Years_9-15")

# Calculate proportions from frequencies
# eg. Collapse # of female & male students to proportion male
propns <- round(SecAKL2016[, 9:16]/SecAKL2016$Roll*100, 1)
colnames(propns) <- paste0(colnames(SecAKL2016[, 9:16]), "_p")

# Merge w rest of data
SecAKL2016 <- cbind(SecAKL2016, propns)

# Check
head(SecAKL2016)
summary(SecAKL2016)

# Remove rownames
rownames(SecAKL2016) <- NULL

# Save AKL schools dataset (takes too long to download and read each time)
save(SecAKL2016, file="files/SecAKL2016.RData")

# Plots
ggplot(SecAKL2016, aes(x=Decile, y=International_p, key=Name, colour=Suburb, label=Roll)) +
  geom_point()
ggplotly()
