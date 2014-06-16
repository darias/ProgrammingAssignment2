## Use makeCacheMatrix to create a list that contains a matrix and its inverse.
## Use cacheSolve to create or reuse the inverse in a makeCacheMatrix list.

## makeCacheMatrix stores a matrix and its inverse into a list providing
## functions for caching and retrieving the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        icache <- NULL
        set <- function(y) {
                x <<- y
                icache <<- NULL
        }
        get <- function() x
        setinv <- function(inv) icache <<- inv
        getinv <- function() icache
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix stored in a makeCacheMatrix
## list. If the inverse has not be cached, it is created and cached in the
## list. If the inverse has been cached, it is returned directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## Check for cached inverse
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	## Calculate the inverse of x
        data <- x$get()
        m <- solve(data, ...)

	## Cache the inverse
        x$setinv(m)
        m
}
