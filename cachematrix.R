## makeCacheMatrix creates a "cache" environment in which both the original matrix
## and its computed inverse can be cached.
## cacheSolve uses the cache environment to return the inverse (calculates it if not cached)

## makeCacheMatrix
## makeCacheMatrix essentially creates an environment for caching a matrix and itys associated inverse
## by exposing the functions setMatrix(), getMatrix(), setInverse(), getInverse()

makeCacheMatrix <- function(x = matrix()) {
	cachedMatrix <- NULL
	cachedInverse <- NULL

	setMatrix <- function(m) cachedMatrix <<- m
	getMatrix <- function() cachedMatrix
	setInverse <- function(m) cachedInverse <<- m
	getInverse <- function() cachedInverse

	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve
## cacheSolve takes as input the cacheMatrix returned by makeCacheMatrix
## Before calling cacheSolve, the matrix has to be set using the setMatrix call for the first time
## It checks if the inverse for the matrix is already cached
## else computes the inverse using solve() and caches it using setInverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
        	return(inverse)
        }
        inverse <- solve(x$getMatrix())
        x$setInverse(inverse)
        inverse
}