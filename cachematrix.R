## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(mat){
    x <<- mat
    
    # invMatrix is made to be available in the main function and setMatrix function
    invMatrix <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {invMatrix <<- inverse}
  getInverse <- function() {invMatrix}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if (!is.null(invMatrix)) {
    return(invMatrix)
  }
  mat <- x$get()
  invMatrix <- solve(mat, ...)
  x$setInverse(invMatrix)
  invMatrix
}