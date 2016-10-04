## The following two functions 1. create a list object to hold the cache for an 
## input matrix and 2. return the matrix inverse re-using the cache if possible.

## This function creates an object (a matrix function list) that contains 
## functions to set/get the (input) matrix, and to set/get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        invMtrx <- NULL
        setMtrx <- function(y) {
                x <<- y
                invMtrx <<- NULL
        }
        getMtrx <- function() x
        setInv <- function(invM) invMtrx <<- invM
        getInv <- function() invMtrx
        list(setMtrx = setMtrx, getMtrx = getMtrx,
             setInv = setInv,
             getInv = getInv)
}


## This function calls the functions in the "matrix list" object to get 
## the cached inverse of the matrix (if it has already been calculated);
## otherwise, it gets the matrix, calculates and saves the inverse matrix 
## in the cache (for the next time this function is called without first
## calling the makeCacheMatrix function to (re)set the matrix), and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtrx <- x$getInv()
        if(!is.null(invMtrx)) {
                message("getting cached data")
                return(invMtrx)
        }
        data <- x$getMtrx()
        invMtrx <- solve(data)
        x$setInv(invMtrx)
        invMtrx
}
