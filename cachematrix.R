## Put comments here that give an overall description of what your
## functions do

## This function creates four objects to manipulate a matrix:
#  set - will set the value of the matrix if none exists
#  get - will return the value of the matrix
#  setinverse - will set the value of the inverted matrix
#  getinverse - will return the inverted value
#  All of these will be in the global environment for use by other functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve(x)
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function uses the functions created in makeCacheMatrix to actually create the inverse matrix

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}