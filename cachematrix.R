## This program provides two functions to allow creation of a special
## matrix object that is able to cache its inverse, which is an
## expensive operation

## Creates a special matrix object able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inv) {
    inverse <<- inv
  }
  
  getinverse <- function() {
    inverse
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function to resolve a 'special' matrix object, determining if the inverse
## is already calculated. If it is not, the matrix is inversed if possible
## NOTE: if the matrix is not invertible, an error is returned

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return (inv)
  }
  mat <- x$get()
  if (nrow(mat) != ncol(mat)) {
    stop('Matrix is not invertible')
  }
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
