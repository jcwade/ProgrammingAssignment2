## `CacheMatrix`: a matrix with cached computations.
## 
## A `CacheMatrix` adds caching functionality to a matrix.  The underlying 
## matrix can be accessed through the `get` and `set` methods.  If the matrix 
## has been modified, the user *must* use the `set` method to update the matrix 
## and invalidate the cache.
## 
## The function `makeCacheMatrix` creates a new `CacheMatrix` and `cacheSolve` 
## will compute the inverse of the matrix along with caching the result.

## Examples
## 
## > m <- makeCacheMatrix(diag(c(1, 2, 4), 3, 3))
## > inv <- cacheSolve(m)
## 
## Is m*m^-1 == 1?
## > all(m$get() %*% inv == diag(rep(1, 3), 3, 3))
## [1] TRUE
## 
## Test caching.
## > m$set(diag(c(4, 2, 1), 3, 3))
## > is.null(m$get.inverse())
## [1] TRUE
## > invisible(cacheSolve(m))
## > all(m$get() %*% m$get.inverse() == diag(rep(1, 3), 3, 3))
## [1] TRUE

## Create a `CacheMatrix` from a matrix; or, an empty matrix is used as a
## default.
makeCacheMatrix <- function(x = matrix()) {
        # We implement the cache using closures.
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set.inverse <- function(inv) inverse <<- inv
        get.inverse <- function() inverse
        list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}

## `CacheMatrix` inverse with caching.  Additional arguments are passed to 
## `solve`.
cacheSolve <- function(x, ...) {
        inv <- x$get.inverse()
        if (!is.null(inv)) {
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$set.inverse(inv)
        inv
}
