##########
# Programming Assignment 1: Part 3
##########

# Author: Avery Sandborn
# Date: January 11, 2016

##########

# Write a function that takes a directory of data files and a threshold for 
#   complete cases and calculates the correlation between sulfate and nitrate for 
#   monitor locations where the number of completely observed cases (on all 
#   variables) is greater than the threshold. The function should return a vector 
#   of correlations for the monitors that meet the threshold requirement. If no 
#   monitors meet the threshold requirement, then the function should return a 
#   numeric vector of length 0.

##########

# Set working directory
setwd("N:/USERS/Avery/R_Coursera")

# Define new function with 2 arguments
corr <- function(directory, threshold = 0){
  
  # Indicate location of csv files
  my_directory <- paste(setwd("N:/USERS/Avery/R_Coursera"), directory, sep = "/")

  # Find out the number of files in the directory
  number_of_files <- length(list.files(my_directory))
  id <- 1:number_of_files
  
  # Create vector to hold data in case of multiple id
  corr_vector <- c()

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
    my_csv <- read.csv(paste(paste(my_directory, new_id, sep = "/"), ".csv", sep = ""), header = T, sep = ",", na.strings = "NA")
        
    # Subset the data to remove rows with NAs
    my_subset <- na.omit(my_csv)
    print(my_subset)
    print(nrow(my_subset))
    
    if(nrow(my_subset) > threshold) {
      
      corr_answer <- cor(my_subset[,2], my_subset[,3])
    
      corr_vector <- c(corr_vector, corr_answer) 
      
    }
     
  }
  
  print(corr_vector)
  
}
