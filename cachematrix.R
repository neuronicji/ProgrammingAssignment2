## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss 
## here). Your assignment is to write a pair of functions that cache the 
## inverse of a matrix.

## Write the following functions:
##      1. makeCacheMatrix: This function creates a special "matrix" object that
##         can cache its inverse.
##      2. cacheSolve: This function computes the inverse of the special 
##         "matrix" returned by makeCacheMatrix above. 
##         If the inverse has already been calculated (and the matrix has not 
##         changed), then the cachesolve should retrieve the inverse from the 
##         cache.

## Computing the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, then solve(X) returns 
## its inverse.

# This function creates a special "matrix" object that can cache its inverse.

## The function creates a closure that stores a matrix and its inverse, provides 
## methods to set and retrieve both, and uses <<- to ensure these values are 
## maintained in the parent environment for persistent caching across function 
## calls.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize the inverse property to NULL
        i <- NULL
        
        # Define a function to set the matrix and reset the cached inverse
        set <- function(y) {
                x <<- y      # Assign the new matrix to x in the parent environment
                i <<- NULL   # Reset the cached inverse in the parent environment
        }
        
        # Define a function to get the current matrix
        get <- function() x
        
        # Define a function to set the inverse of the matrix
        setInverse <- function(inverse) i <<- inverse
        
        # Define a function to get the cached inverse
        getInverse <- function() i
        
        # Return a list of the above functions to interact with the matrix and its inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve retrieves the cached inverse of a matrix or computes and stores 
## it if not already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Attempt to retrieve the cached inverse
        i <- x$getInverse()
        
        # If the inverse is already cached, return it with a message
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # Otherwise, retrieve the original matrix
        matrx <- x$get()
        
        # Compute the inverse of the matrix
        i <- solve(matrx, ...)
        
        # Cache the newly computed inverse for future use
        x$setInverse(i)
        
        # Return the inverse
        i
}
