##########
# Programming Assignment 3, Part 2
##########

# Author: Avery Sandborn
# Date: January 13, 2016

##########

# Set the working directory
setwd("N:/USERS/Avery/R_Coursera/Assignment3")

# Write a function called "rankhospital" that takes 3 arguments
rankhospital <- function(state, outcome, num = "best"){
  
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
  
  # Check that num is a valid resposne
  if (num == "best") {
    new_num <- 1
  }
  
  else if (num == "worst") {
    new_num <- "worst"
  }
  
  else if (num <= 0) {
    stop("invalid rank") 
  }
  
  else if (num%%1 != 0) {
    stop("invalid rank") 
  }
  
  else if (num%%1 == 0) {
    new_num <- num
  }
  
  # Write a function that figures out which column to look in and finds the best hospital
  get_hosp_name <- function(data, state, col_num) {
    
    # Subet the data by state
    state_subset <- subset(data, data[, 7] == state)
    
    # Convert only the mortality column to numeric
    state_subset[, col_num] <- as.numeric(state_subset[, col_num])
    
    last_rank <- dim(state_subset[!is.na(state_subset[, col_num]), ])[1]
    
    # See what number rank "worst" is
    if (new_num == "worst") {
      rank <- last_rank
    }
    
    else if (new_num > last_rank) {
      rank <- "NA"
    }
    
    else {
      rank <- new_num
    }
    
    # Get ranked hospital
    hospital_name <- state_subset[,2][order(state_subset[, col_num], state_subset[, 2])[rank]]
  
    print(hospital_name)
      
  }
  
  # use the function to return the best result
  if (outcome == "heart attack") {
    rank_result <- get_hosp_name(data, state, 11) 
  }

  else if (outcome == "heart failure") {
    rank_result <- get_hosp_name(data, state, 17)
  }

  else if (outcome == "pneumonia") {
    rank_result <- get_hosp_name(data, state, 23)
  }  
  
}
