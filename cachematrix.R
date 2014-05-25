# The following code Caches the Inverse of a Matrix

# makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# It contains functions to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then cacheSolve should retrieve the inverse from
# the cache.
# Note: This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Usage:
# source("cachematrix.R")
# myMatrix <- matrix(c(1, 3, 3, 1, 4, 3, 1, 3, 4), 3, 3, byrow = TRUE)
# cacheMatrix <- makeCacheMatrix(myMatrix)
# cacheMatrix$get()
# cacheSolve(cacheMatrix)
# cacheSolve(cacheMatrix)

