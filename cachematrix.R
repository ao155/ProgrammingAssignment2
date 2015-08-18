## The following two functions allow the inversion of a matrix and caching of
## the inversed matrix, so that the inversion function runs faster when
## inversing the same matrix multiple times

## This function creates 4 functions for a matrix x:
##    x$set(y) - sets the values of the matrix x to y
##    x$get() - return the values of the matrix x
##    x$setInverse(y) - sets the values of the inversed matrix of x to y
##    x$getInverse() - returns the values of the inversed matrix of x
## Once this function has been run for a matrix x, the 4 mentioned functions
## can be used by the function cacheSolve(x) to cache an inverted matrix for x.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function inverts a matrix x (after makeCacheMatrix(x) has been run)
## and caches the inverted matrix. When the function is called the second time
## for the matrix x, it returns the cached inverted matrix.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}


