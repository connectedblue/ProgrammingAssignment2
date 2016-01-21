## These two functions are for Programming Assignment 2
## Two matrix functions are needed.  One to create a special matrix which holds
## a cache of the matrix inverse.  the second is a function which uses this special
## matrix to return the inverse from the cache if it exists.


## This function allows a special CacheMatrix to be created

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function allows the inverse to be returned from the cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("inverse being retrieved from cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
  }
