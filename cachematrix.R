# R Assignment 2 functions - caching the inverse of a matrix
makeCacheMatrix <- function(x = matrix(), a = numeric(), b = numeric()) {
  # function to cache the inverse of a matrix,
  # results to be used in later function
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # function to solve for the inverse of a matrix
  # using previously cached function results
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}