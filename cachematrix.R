# Assignment: Caching the Inverse of a Matrixless 
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# For this assignment, assume that the matrix supplied is always invertible.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        # set value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x # get the value of the matrix
        setInv <- function(solve) m <<- solve # set the value of the inverse
        getInv <- function() m # get the value of the inverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

#   cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#               If the inverse has already been calculated (and the matrix has not changed)
#               , then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInv() # call to object's get inverse function 
        if(!is.null(m)) {
                message("getting cached data")
                return(m) # returns inverse if cached
        }
        data <- x$get() # else, makes computation
        m <- solve(data, ...)
        x$setInv(m) 
        m ## Return a matrix that is the inverse of 'x'
}