##########
# Programming Assignment 1: Part 2
##########

##########
# Author: Avery Sandborn
# Date: January 11, 2016
##########

##########
# Write a function that reads a directory full of files and reports the number of 
#   completely observed cases in each data file. The function should return a data 
#   frame where the first column is the name of the file and the second column is 
#   the number of complete cases. A prototype of this function follows
##########

# Set working directory
setwd("N:/USERS/Avery/R_Coursera")

# Define new function with 2 arguments
complete <- function(directory, id = 1:332){
  
  # Indicate location of csv files
  my_directory <- paste(setwd("N:/USERS/Avery/R_Coursera"), directory, sep = "/")
  
  # Create vector to hold data in case of multiple id
  row_vector <- c()
  
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
    
    # Subset the data to remove rows with NAs
    my_subset <- subset(my_csv, sulfate != "NA", nitrate !="NA")
    my_rows <- nrow(my_subset)
    
    # Add the edited data (no NAs) to the vector
    row_vector <- c(row_vector, my_rows)
      
  }
  
  # Print the results in a table
  result <- data.frame(id = id, nobs = row_vector)
  print(result)
  
}
