## Programming Assignment 2: Caching the Inverse of a Matrix
## makeCacheMatrix creates a matrix object and cache its inverse
## cacheSolve, gets the inverse of the matrix object returned by makeCacheMatrix. 
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix has 4 functions in it
## set takes the matrix and stores it in the parent environment
## get retrieves the matrix from the parent environment
## setsolve defines the setter for the inverse invMatrix
## getsolve defines the getter for the inverse invMatrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() 
    x
  setsolve <- function(solve) 
    invMatrix <<- solve
  getsolve <- function() 
    invMatrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve is used to retrieve the inverse from the matrix object

cacheSolve <- function(x, ...) {
  invMatrix <- x$getsolve()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setsolve(invMatrix)
  return(invMatrix)
}