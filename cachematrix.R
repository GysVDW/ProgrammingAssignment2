## The following functions create Matrix objects that can calculate the Matrix inverse
## and cache the value for quick subsequent retrieval.

## The makeCacheMatrix function creates a Matrix object.
## The Matrix object contain methods (e.g. set, get, set_inv and get_inv)
## that can be used to populate or return a matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) x_inv <<- inverse
    get_inv <- function() x_inv
    list(set = set, 
         get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

## The cacheSolve function returns the inverse of a Matrix object (see makeCacheMatrix).
## If the inverse has been calculated before, it can simply be retrieved from cache
## using the get_inv method of the Matrix object. Alternatively, it will be calculated
## here and will then be cached using the set_inv method of the Matrix object.

cacheSolve <- function(x, ...) {
    x_inv <- x$get_inv()
    if (!is.null(x_inv)) {
        message("getting cahced inverse")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data)
    x$set_inv(x_inv)
    x_inv
}
