## A Set of functions that return the inverse of a matrix

## Create functions for storing and retrieving-from-cache: A matrix
## and it’s inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Create function to check if an inverse 
## exists in cache and if it doesn’t calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}