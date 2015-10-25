## This script implements matrix objects with cachable inverse.
##
## It provides two functions:
## * makeCacheMatrix - creates matrix object that is 
##                     able to store its inverse.
## * cacheSolve      - calculates and stores an inverse of an matrix.

## Creates matrix that is able to cache its invert. 
## Use cacheSolve to calculate invert and cache the result. 

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setinverted <- function(i) inverted <<- i
  getinverted <- function() inverted
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## This function calculates matrix invert and caches the results.
## It should be used with objects returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inverted <- x$getinverted()
  if(!is.null(inverted)) {
    message("Using cached data.")
    return(inverted)
  }
  X <- x$get()
  inverted <- solve(X)
  x$setinverted(inverted)
  inverted
}
