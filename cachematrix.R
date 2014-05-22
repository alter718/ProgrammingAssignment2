## Put comments here that give an overall description of what your
## functions do

## This function creates a list using a matrix as an argument
## list$get() returns the matrix and list$set() allows for changing the matrix
## list$getSolve() returns a previously stored solution while list$setSolve() stores the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function solves the inverse of the matrix if it does not exist and 'gets' the 
## cached solution if previous solved.
## The first time it is run it will perform the inverse. The next time it will return the cached 
## solution.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
