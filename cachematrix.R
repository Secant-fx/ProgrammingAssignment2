## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    getinverse <- function() {
        inverse
    }
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function returns the inverse of the matrix. If the inverse has already been calculated, it returns the cached value.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
