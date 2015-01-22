## makeCacheMatrix and cachesolve two functions that are used to 
## create a special object that stores a matrix 
## and caches its inverse using the built-in solve function.

## used to get or set the matrix whose inverse is to be calculated, possibly multiple times.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}




## Caches the inverse. For the first time the inserve is calculated, henceforth for the same matrix returns the cached
## value instead of recalculating the inverse again.

cachesolve <- function(x,...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}

