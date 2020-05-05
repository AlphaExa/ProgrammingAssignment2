# This script includes a pair of functions that compute the inverse of a matrix and store it as a cache.
# Caching the solution is beneficial for repeated usage.


## makeCacheMatrix: This function creates a list of 4 functions for storing matrix and preparing to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get.matrix <- function() x
    set.inverse <- function(solution) inverse <<- solution
    get.inverse <- function() inverse
    
    list(set = set, get.matrix = get.matrix, set.inverse = set.inverse, get.inverse = get.inverse)
    
}


## cacheSolve: This function will evaluate whether there is a valid cache of the inverse (stored in the 'makeCacheMatrix' above).
## If the inverse is cached, the inverse will be retrieved; Otherwise, this function will calculate and cache the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$get.inverse()
    
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    data <-x$get.matrix()
    inverse <- solve(data,...)
    x$set.inverse(inverse)
    inverse
}