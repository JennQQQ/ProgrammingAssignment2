## Put comments here that give an overall description of what your
## functions do

## this function cache the input's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    if (det(x)!=0) {inv <- solve(x)}
    cacheinverse <- function() inv
    list(get=get,cacheinverse = cacheinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$cacheinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    new_x <- x$get()
    new_inv <- solve(new_x)
    new_inv
}
