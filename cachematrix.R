## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
##
## This function creates an object that caches (i.e., saves in memory) an invertible matrix 
## and its calculated inverse.
##
## Assumptions (for this exercise's purpose):
##   1. The input matrix is not NULL.
##   2. The input matrix is a square matrix.
##   3. The input matrix is invertible.
##   4. The input matrix has values in all its entries.
##
## Responses from this function:
##   1. The cached input matrix.
##   2. The inverse matrix.
##   3. Response of whether or not the input matrix has changed.
##   4. Response of whether or not the inverse has been calculated.

makeCacheMatrix <- function(x = matrix()) {
  
  # ---------------------
  # INSTANCE VARIABLES
  # ---------------------
  
  # Initialize the invertible, input matrix
  invertible <- matrix(0)
  
  # Initialize a matrix used to determine if the input matrix has changed
  compare <- matrix(0)
  
  # Initialize the inverse, output matrix to NULL (i.e., nothing has been calculated)
  inverse <- NULL
  
  # Set a boolean flag that determines whether or not the inverse has been calculated
  inverseCalculated <- FALSE
  

  # ---------------------
  # ACCESSORS
  # ---------------------
  
  # Set invertible with the value of argument: invertibleMatrix
  setInvertible <- function(invertibleMatrix) {
    
    # First, set compare with the value of invertible
    compare <<- invertible
    
    # Set invertible with the value of the argument
    invertible <<- invertibleMatrix
    
    # If the cache matrix changed, set the inverseCalculated flag to FALSE
    # and set inverse to its initial value
    if (hasInvertibleChanged()) {
      
      inverseCalculated <<- FALSE
      inverse <<- NULL
  
    }  # END if
    
    # Respond the cache matrix
    invertible
    
  }  # END setInvertible
  
  # Respond invertible
  getInvertible <- function() { invertible }
  
  # Set inverse with the value of argument: inverseMatrix
  setInverse <- function(inverseMatrix) {
    
    # Set inverse with the argument
    inverse <<- inverseMatrix
    
    # Here, we assume that the argument inverseMatrix is indeed the calculated matrix.
    # Therefore, set the inverseCalculated flag to TRUE
    inverseCalculated <<- TRUE
    
    # In order to preserve the calculated inverse, both invertible and compare must be the same
    compare <<- invertible
    
    # Respond the inverse matrix
    inverse
    
  }  # END setInverse
  
  # Respond inverse
  getInverse <- function() { inverse }
  
  # Respond compare
  getCompare <- function() { compare }
  
  # Respond inverseCalculated
  isInverseCalculated <- function() { inverseCalculated }
  
  
  # ---------------------
  # DATA OPERATIONS
  # ---------------------
  
  # Function for determining if all corresponding entries of x and y are equal
  equalEntries <- function(x, y) {
    
    for (i in nrow(x)) {
      
      for (j in ncol(x))
        if (x[i,j] != y[i,j]) return(FALSE)
      
    }  # END for
    
    TRUE
    
  }  # END equalEntries
  
  # Respond whether or not invertible has changed by comparing it to compare. If both 
  # are equal, respond TRUE. Otherwise, respond FALSE
  # 
  # MATRIX EQUALITY
  # Two matrices are said to be equal if they have the same size and the corresponding
  # entries in the two matrices are equal
  hasInvertibleChanged <- function() {

    if ((dim(getInvertible())[1] != dim(getCompare())[1]) || 
        (dim(getInvertible())[2] != dim(getCompare())[2]) || 
        !equalEntries(getInvertible(), getCompare()))
      return(TRUE)
    
    FALSE
    
  }  # END hasInvertibleChanged
  

  # ---------------------
  # FUNCTION RETURN VALUE
  # ---------------------
  
  # Return a list of accessors and data operation functions
  list(setInvertible = setInvertible, getInvertible = getInvertible, 
       setInverse    = setInverse,    getInverse    = getInverse,
       hasInvertibleChanged = hasInvertibleChanged,
       isInverseCalculated  = isInverseCalculated)
  
}  # END makeCacheMatrix


## Write a short comment describing this function
##
## Return a matrix that is the inverse of 'x'
##
## Assumptions (for this exercise's purpose):
##   1. The argument x is not NULL.
##   2. The argument x is an object of type ''makeCacheMatrix'.
##   3. The argument x contains a cached input matrix.
##
## Process:
##   1. If the inverse is cached in x, and the input matrix in x has not changed, 
##      the function responds the cached inverse.
##   2. Otherwise, the function will calculate the inverse of the input matrix in x.
##   3. If there is an error in the calculation, solve() presents a message and exits.
##      Otherwise, the calculated inverse will be cached and responded.

source("makeCacheMatrix.R")

cacheSolve <- function(x, ...) {
  
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache
  if (x$isInverseCalculated() && !x$hasInvertibleChanged()) {
    
    print("Retrieving inverse from cache...")
    return(x$getInverse())
    
  }  # END if
  
  # Proceed to calculate the inverse, and respond it
  print("Calculating inverse...")
  x$setInverse( solve(x$getInvertible()) )
  
}  # END cacheSolve
