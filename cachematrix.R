## The functions in this file create a matrix that can cache its inverse


## makeCacheMaxrix creates a list that contains functions for getting and setting
## a matrix, and for getting and setting the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }    
    
    get <- function() {
        x
    }
    
    set.inverse <- function(inverse) {
        m <- inverse
    }
    
    get.inverse <- function () {
        m
    }
    
    list(get = get, 
         set = set, 
         set.inverse = set.inverse, 
         get.inverse = get.inverse)
}


## cacheSolve returns the inverse of a matrix that was created with makeCacheMatrix
## If a cached value exists, it is returned, otherwise a new value is computed, cached and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x.getInverse()
        if (!is.null(m))
        {
            message("Getting cached data")
            return(m)
        }
        data <- x.get()
        m <- solve(data)
        x$set.inverse(m)
        m
}
