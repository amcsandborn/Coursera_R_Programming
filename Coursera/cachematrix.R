##########
# Programming Assignment 2: Lexical Scoping
##########

# Author: Avery Sandborn
# Date: January 12, 2016

##########
# Description
##########

# Write an R function that is able to cache potentially time-consuming 
#   computations. For example, taking the mean of a numeric vector is typically 
#   a fast operation. However, for a very long vector, it may take too long to 
#   compute the mean, especially if it has to be computed repeatedly (e.g. in a 
#   loop). If the contents of a vector are not changing, it may make sense to 
#   cache the value of the mean so that when we need it again, it can be looked 
#   up in the cache rather than recomputed. This Programming Assignment will take 
#   advantage of the scoping rules of the R language and how they can be 
#   manipulated to preserve state inside of an R object.

##########
# Assignment Answer
##########

# Create a function called makeCacheMatrix that does the following: 
#   1) Set the value of the matrix
#   2) Get the value of the matrix
#   3) Set the value of the inverse
#   4) Get the value of the inverse

# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  my_inverse <- NULL
  
  # Set the value of the vector
  set <- function(y) {
    
    x <<- y
    my_inverse <<- NULL
    
  }
  
  # Get the value of the vector
  get <- function() x
  
  # Set the value of the inverse
  set_inverse <- function(inverse) my_inverse <<- inverse
  
  # Get the value of the inverse
  get_inverse <- function() my_inverse
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}

# Calculate the inverse of the special matrix created above.
#   1) Check to see if the inverse has been calculated and is unchanged
#   2) If so, get inverse from cache and skip computation
#   3) Else, calculate inverse and set value of inverse in cache

cacheSolve <- function(x, ...) {
  
  # get mean
  my_inverse <- x$get_inverse()
  
  # If the mean has already been calculated
  if(!is.null(my_inverse)) {
    
    # Get mean from cache
    message("getting cached data")
    return(my_inverse)
    
  }
  
  # Else, get data
  data <- x$get()
  
  # calculate mean
  my_invserse <- solve(data, ...)
  
  # set value of mean in cache
  x$set_inverse(my_invserse)
  
  # Return the mean 
  my_invserse
  
}
