## This function creates four objects to manipulate a matrix.  They are stored in a list in the global
# environment so that other functions will be able to use them.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #  set - will set the value of the matrix if none exists.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #  get - will simply return the value of the matrix.
  get <- function() x
  #  setinverse - will set the inverted matrix.
  setinverse <- function(solve) i <<- solve(x)
  #  getinverse - will return the inverted value.
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function uses the functions created in makeCacheMatrix to actually create the inverse matrix

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of x.
  i <- x$getinverse()
  # Check to see if the inverse is already in the cache.  If so, return the cached matrix.
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  # Since no cached inverse matrix was found, invert, cache, and return the inverse.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}