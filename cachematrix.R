## The pair of functions makeCacheMatrix and cacheSolve allow users to cache the inverse of a matrix

## makeCacheMatrix creates a matrix-type object ('cacheMatrix') for which the inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  ## Returns 'cache-Matrix' - a list of functions get and set value of the matrix and get and set the value of the inverse
  ## inputs: x = invertible matrix
  ## outputs: cacheMatrix
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) xInv <<- inverse
  getInv <- function() xInv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## casheSolve returns the inverse of 'cacheMatrix' object by either:
## 1. if the inverse of the current matrix is stored in cache, retrieving it.
## 2. if not, computing the inverse and caching it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## inputs: cacheMatrix
        ## outputs: matrix inverse
  xInv <- x$getInv()
  # check whether inverse is cached
  if (!is.null(xInv)) {
    message('getting cached data')
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInv(xInv)
  xInv
}

