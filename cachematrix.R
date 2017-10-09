## These functions provide functionality to calculate and store the inverse
## of a matrix, avoiding recalculation of the inverse when used later
## in the calculations

## The function makeCacheMatrix takes a matrix as input, and provides means
## to store both the matrix and its inverse. It returns a cacheable matrix 
## (actually a list) containing the
## get and set operators for both the matrix and its inverse. When setting the
## matrix, any possible previously calculated inverse is reset.

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
      set <- function(y) {
                x <<- y
                xinv <<- NULL
      }
      get <- function() x
      setinv <- function(inv) xinv <<- inv
      getinv <- function() xinv
      list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve providesa means to calculate the inverse of a
## cacheable matrix avoiding recalculation if the inverse had been calculated previously

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        mtrx <- x$get()
        xinv <- solve(mtrx, ...)
        x$setinv(xinv)
        xinv
}
