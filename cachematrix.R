## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ## setting our environment and functions that will go in our special
     ## matrix + functions list
     m <- NULL
     ## get simply returns the value (the matrix) that was previously stored
     get <- function() x
     ## setsolve sets the stored value of the solved matrix (the inverse)
     setsolve <- function(solve) m <<- solve
     ## getsolve returns the previously cached solved matrix
     getsolve <- function() m
     ## we're returning a list that contains first the matrix we were passed
     ## followed by our member functions
     list(x = x, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## first grab the cached m if there is one
     m <- x$getsolve()
     ## if it already exists, it must be stored previously, so return it
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     ## if we're down here, we didn't return above, so make the calculations
     ## and store them so we don't have to do this again
     ## get the stored value of the matrix
     data <- x$get()
     ## run solve and store it where we can get to it
     m <- solve(data, ...)
     ## set the solve value so we don't have to do this again
     x$setsolve(m)
     ## return the answer
     m
}
