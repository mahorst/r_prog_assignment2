
# R Programming
# Assignment 2
# Mike Horst
# mahorst@lghealth.org
# User ID: 10091314
# 11/20/2014

# This function demonstrates the scoping rules of R and stores the inverse
# of a matrix by caching it to avoid repeated computations.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmat <- function(invmat) m <<- invmat
        getinvmat <- function() m
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinvmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmat(m)
        m
}
