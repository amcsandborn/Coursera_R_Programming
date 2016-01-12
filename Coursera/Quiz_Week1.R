##########
# Quiz Week 1
##########

# Author: Avery Sandborn
# Date: January 11, 2016

##########

# Read in data
data <-"C:/Users/sandav/Downloads/quiz1_data/hw1_data.csv"
data2 <- read.csv(data)
x <- data.frame(data2)

# Get names of columns
names(x)

# Get number of rows and columns
nrow(x)
ncol(x)

# Extract first two rows of data
head(x, 2)

# Extract last two rows of data
tail(x, 2)

# Get value of Ozone column in row 47
x[47,1]

# Count how many N/As there are in the Ozone Column
NA_values <- sum(is.na(x[1]))
NA_values

# Calculate mean of Ozone Column
mean_of_Ozone <- mean(x[, "Ozone"], na.rm = TRUE)
mean_of_Ozone

# Extract rows where Ozone > 31 and Temp > 90
# Get mean of Solar.R in this subset
subset1 <- subset(x,Ozone >31 & Temp > 90)
mean_of_SolarR <- mean(subset1[,"Solar.R"], na.rm = TRUE)
mean_of_SolarR

# Get average of Temp in month 6
month6 <- subset(x, Month == 6)
mean_of_month6 <- mean(month6[,"Temp"], na.rm = TRUE)
mean_of_month6

# Get max Ozone value in month 5
month5 <- subset(x, Month == 5)
max_of_month5 <- max(month5[,"Ozone"], na.rm = TRUE)
max_of_month5
