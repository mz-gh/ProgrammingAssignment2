## Two functions to provide caching functionality for calculating the inverse of a matrix.
##
## Example usage showing that the second invocation of cacheSolve() on the same
## "special" matrix reads the inverse from cache:
## > matr <- matrix(rnorm(10000), nrow=100, ncol=100)
## > specmatr <- makeCacheMatrix(matr)
## > specmatrInv <- cacheSolve(specmatr)
## > specmatrInv <- cacheSolve(specmatr)
## getting cached data

## Creates a "special" matrix, which can store it's own inverse, meaning that it needs
## to be calculated only once.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## reset the matrix
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x
  set_inverse <- function(solve) inv <<- solve
  get_inverse <- function() inv

  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## Calculates the inverse of a "special" matrix (see makeCacheMatrix above) only if
## it was not calculated (and cached) beforehand, in which case it only returns
## the cached value .
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  
  if (!is.null(inv)) {
    ## The default value of the inverse of the "special" matrix is NULL,
    ## so arriving here means, that the inverse was already calculated.
    message("getting cached data")
    return(inv)
  }
  
  ## The inverse was not yet calculated, so let's do it now and also store (cache) it.
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  
  inv
}
