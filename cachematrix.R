makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             # sets the value of m to NULL
    set <- function(y) {    # set the value of the matrix
        x <<- y               # caches the inputted matrix so that cacheSolve can check whether it has changed
        inv <<- NULL          # sets the value of m (the matrix inverse if used cacheSolve) to NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {                              # check to see if cacheSolve has been run before
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)               # sets the value of the inverse in the cache via the setInverse function.
    inv
}
