## Put comments here that give an overall description of what your
## functions do

## The purpose of these functions is to cut down the user time of the code by
## not having to recompute a value. Instead of having to re-valuate the inverse
## of a matrix, if it has already been computed, it will be cached so that if it
## is needed again, the cached data will be used instead of calculating the
## inverse all over again.

## Write a short comment describing this function
## This function uses the <<- operator to assign a value to an object in a
## in a different environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse of a matrix by using the solve function.
## But before it does that, it first checks if the matrix has already been
## solved. If it has, it takes that instead of recalculating the mean.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
