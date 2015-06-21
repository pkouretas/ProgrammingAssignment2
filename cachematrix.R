## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix object as an argument and returns 
# a list of functions. These functions are actually the 
# getters and setters for both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	xInverse <- NULL
	set <- function(m) {
        x <<- m
        xInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) xInverse <<- i
    getInverse <- function() xInverse

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve tries to access the inverse of a cacheMatrix object through its 
# inverse getter. If it's NULL, it computes the inverse matrix and calls 
# the setter function in order to cache the value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting matrix inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}