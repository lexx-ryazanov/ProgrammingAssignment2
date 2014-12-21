## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # cached value of inverted matrix
  cache <- NULL
  
  # set the value of the matrix, clear cache
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverted matrix
  setinverted <- function(inverted) cache <<- inverted
  
  # get the value of the inverted matrix
  getinverted <- function() cache
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Try to get inverted matrix value from cache.
  m <- x$getinverted()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If cache is empty, solve matrix and put result in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverted(m)
  m
}
