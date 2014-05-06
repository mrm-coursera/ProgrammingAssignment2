## This file contains a function (makeCacheMatrix)to construct an 'object'
## containing a matrix and, if it has been previously computed, the inverse of
## the matrix, so that the inverse need only be computed once. It also
## contains a function to return the inverse of a CacheMatrix, either
## returning the cached inverse or computing, caching, and returning the
## inverse.

## makeCacheMatrix(x) constructs a representation of the matrix x that can
## cache it's inverse; if the inverse of a cacheable matrix is computed, it
## is stored so that it can be returned easily if needed later.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve(x, ...) returns the cached inverse of x if it is available;
## otherwise, it computes the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
