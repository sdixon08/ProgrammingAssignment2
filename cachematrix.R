## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL # this sets the value of m to NULL 
y <- NULL # this sets the value of y to NULL
setmatrix <- function(y) { #this sets the value for the matrix
    x <<- y ## this caches the matrix input so that cacheSolve can check whether it has changed
    m <<- NULL ## this sets the value of m to NULL
}
list(setmatrix = setmatrix, getmatrix = getmatrix, # this creates a list of the four functions
   setinverse = setinverse,
   getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 

cacheSolve <- function (x, ...) {
# this is to compare matrix to prior
m <- x$getinverse() # this is if an inverse has already been calculated
if(!is.null(m)){ # this is to check to see if cacheSolve has been run prior
    if(x$setmatrix() == x$getmatrix()) { # this checks that the matrix hasn't changed
    return(m)
    }
y <- x$getmatrix() # this runs the getmatrix function to get the value of the input matrix
x$setmatrix(y) # this runs the setmatrix function on the input matrix
m <- solve(y, ...) # this computes the value of the inverse of the input matrix
x$setinverse(m) # this runs the setinverse function on the inverse
m # this returns the inverse
}
