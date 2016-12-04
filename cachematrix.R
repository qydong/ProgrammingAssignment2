## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse,
## which i a list containing functions to
## set: set the value of the matrix
## get: get the value of the matrix
## setinv: set the value of the inverse matrix
## getinv: get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        if (!identical(x, y)) {
            message("setting")
            x <<- y
            inv <<- NULL
        }
    }
    get <- function() {x}
    setinv <- function(inverse) { inv <<- inverse }
    getinv <- function() {inv}
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
