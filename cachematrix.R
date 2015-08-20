# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# setMatrix      set the value of a matrix
# getMatrix      get the value of a matrix
# cacheInverse   get the cahced value (inverse of the matrix)
# getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
        
        # holds the cached value or NULL if nothing is cached
        cache <- NULL
        
        # stores the matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # flushes cache
                cache <<- NULL
        }

        # returns stored matrix
        getMatrix <- function() {
                x
        }

        # cache given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get cached value
        getInverse <- function() {
                cache
        }
        
        # returns a list, each element is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# calculates the inverse of a the matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
        inverse <- y$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        inverse
}
