## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The following function create a Matrix and Return a list of
## depending the use of the Matrix the functions are being called.

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        setmatrix <-function(y){
                x <<- y
                inv_m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) inv_m <<- solve
        getinverse <- function() inv_m 
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculate the inverse of
## above created matrix and if it was already done,
## then it will take the value from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getinverse()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$getmatrix()
        inv_m <- solve(data, ...)
        inv_m <- x$setinverse(inv_m)
        inv_m
}
