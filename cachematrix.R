## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.  It is a implementation a pair of functions that cache the
## inverse of a matrix.  The first function named makeCacheMatrix creates a
## special "matrix" object that can cache its inverse.  The second function
## named cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix.

## This function creates a special "matrix" object has a few functions.
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        ## Return the list that contains functions that perform the
        ## following operations: setting or getting the value of the matrix
        ## and its inverse matrix, verifying that the inverse matrix is not
        ## calculated.
        list( set    = function(y) { x <<- y; cache <<- NULL }
            , get    = function( ) x
            , setInv = function(...) cache <<- solve(x, ...)
            , getInv = function( ) cache
            , notInv = function( ) is.null( cache )
            )
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.  If the inverse has already been calculated (and
## the matrix has not changed), then the function will retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
        ## Calculate the inverse of the special "matrix" if it is necessary.
        if ( x$notInv() )
                x$setInv(...)
        ## Retrieve a inverse from the cache.
        x$getInv()    
}
