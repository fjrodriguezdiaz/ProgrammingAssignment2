## This script implements a pair of functions that cache 
## the inverse of a matrix. Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly


## This function creates a special object "matrix" that cache its invese

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## This function computes the inverse of the special object "matrix" returned by the makeCacheMatrix function. 
## If the inverse has already been calculated, then the cachesolve function retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
