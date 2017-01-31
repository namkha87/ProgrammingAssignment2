## Below functions are used to create a special object that stores a numeric matrix 
## and cache's its inverse.

# creates a special "matrix" which is a list containing functions:
# - set: set the value of the matrix
# - get: get the value of the matrix
# - setinverse: set the inverse of the matrix
# - getinverse: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created by makeCacheMatrix.
## It first checks if the inverse has already been calculated. If so, it returns 
## the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via the 
## setinverse function for later retrieving.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
