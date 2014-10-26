## A small library to generate a matrix data structure that
## can retain a cache of its own inverse, and a method for
## calculating and caching the inverse matrix.

## Returns a list representing a matrix 
## that can cache its own inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix = NULL
  
  set = function(my_matrix) {
    x <<- my_matrix
    inverse_matrix <<- NULL
  }
  
  get = function() x
  
  setInverse = function(my_inverse) {
    inverse_matrix <<- my_inverse
  }
  
  getInverse = function() inverse_matrix
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Provided a matrix returned by 'makeCacheMatrix',
## either return the cached inverse or calculate
## and cache the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix = x$getInverse()
  if (!is.null(inverse_matrix)) {
    message("getting cached data")
  } else {
    inverse_matrix = solve(x$get(), ...)
    x$setInverse(inverse_matrix)
  }
  
  inverse_matrix
}
