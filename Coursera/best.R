##########
# Programming Assignment 3, Part 2
##########

# Author: Avery Sandborn
# Date: January 13, 2016

##########

# Set the working directory
setwd("N:/USERS/Avery/R_Coursera/Assignment3")

# Write a function called "best" that takes 2 arguments
best <- function(state, outcome){
  
  # Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
  # Check that state is a valid response
  state_list <- unique(data$State)
  if (!(state %in% state_list)) {
    stop("invalid state")
  }
  
  # Check that outcome is a valid response
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% outcome_list)) {
    stop("invalid outcome") 
  }
    
  # Write a function that figures out which column to look in and finds the best hospital
  get_hosp_name <- function(data, state, col_num) {
    
    # Subet the data by state
    state_subset <- subset(data, data[, 7] == state)
    
    # Convert only the mortality column to numeric
    state_subset[, col_num] <- as.numeric(state_subset[, col_num])
    
    # Get minimum (best) hospital, and find it's row
    min_mortality <- min(state_subset[, col_num], na.rm = TRUE)
    min_index <- which(state_subset[, col_num] == min_mortality)
    
    # Name the hospital
    hospital_name <- state_subset[min_index, 2]
    print(hospital_name)
    
  }
  
  # use the function to return the best result
  if (outcome == "heart attack"){
    best_result <- get_hosp_name(data, state, 11) 
  }
    else if (outcome == "heart failure"){
    best_result <- get_hosp_name(data, state, 17)
  }
    else if (outcome == "pneumonia"){
    best_result <- get_hosp_name(data, state, 23)
  }    
  
}
