## This pair of functions caches the inverse of a matrix via a special "matrix"
## object

## The first function creates a special "matrix" object that can cache its
## inverse. The special "matrix" object is  a list containing functions to:
## (1) set the matrix
## (2) get the matrix
## (3) set the inverse of the matrix
## (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialise a null variable to contain the inverse
    inv <- NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse of the matrix
    getInverse <- function() inv
    
    # Putting all the functions together in a list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The second function returns a matrix that is the inverse of special 
## "matrix" created with the first function. First, it checks if the inverse
## has already been calculated. If yes, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    
    # Try to get the inverse of the matrix from the cache
    inv <- x$getInverse()
    
    # If there is an inverse in the cache, return it (without doing computation)
    if(!is.null(inv)) {
        # Print a message saying that the inverse is being taken from the cache
        message("getting inverse from the cache")
        return(inv)
    }
    
    # If not, get the matrix...
    mat <- x$get()
    # compute the inverse of the matrix...
    inv <- solve(mat, ...)
    # set the computed inverse in the cache...
    x$setInverse(inv)
    # and return the computed inverse
    inv
}


    

