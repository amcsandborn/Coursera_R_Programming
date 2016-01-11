##########
# Programming Assignment 1: Part 1
##########

##########
# Author: Avery Sandborn
# Date: January 11, 2016
##########

##########
# Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#   (sulfate or nitrate) across a specified list of monitors. The function reads
#   that monitor's particulate matter data from the directory and returns the mean 
#   of the polutant accross all of the monitors, ignoring all NA values
##########

# Set working directory
setwd("N:/USERS/Avery/R_Coursera")

# Define new function with 3 arguments
pollutantmean <- function(directory, pollutant, id = 1:332){
  
  # Indicate location of csv files
  my_directory <- paste(setwd("N:/USERS/Avery/R_Coursera"), directory, sep = "/")

  # Create vector to hold data in case of multiple id
  mean_vector <- c()
  
  # For each id, add placeholder zeroes in because of the naming convention
  for (x in id) {
    
    if (x <= 9) {
      new_id = paste("00", x, sep = "")
    }
    else if (x >= 10 & x <= 99) {
      new_id = paste("0", x, sep = "")
    }
    else {
      new_id = x
    }
    
    # Get name of first csv
    my_csv <- read.csv(paste(paste(my_directory, new_id, sep = "/"), ".csv", sep = ""), header = T, sep = ",")
    
    # Remove NA rows
    NA_removed <- my_csv[!is.na(my_csv[, pollutant]), pollutant]
    
    # Add the edited data (no NAs) to the vector
    mean_vector <- c(mean_vector, NA_removed)
  }
  
  # Print the mean of the polutant accross all monitors
  result <- mean(mean_vector)
  print(result)
    
}
