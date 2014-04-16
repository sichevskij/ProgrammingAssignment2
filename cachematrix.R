
makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    
    ## Set the value of the matrix and reset the cache.
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    
    ## Get the value of the matrix.
    get <- function() x
    
    ## Set the value of the inverse matrix.
    setInv <- function(i) c <<- i

    ## Get the value of the inverse matrix.
    getInv <- function() c
    
    ## Return the list that contains manipulation functions.
    list( set = set
        , get = get
        , setInv = setInv
        , getInv = getInv
        )
}


cacheSolve <- function(x, ...) {
    
    ## Try to get a matrix that is the inverse of 'x'
    i <- x$getInv()
    
    ## If a cache is not null return the chached inverse of 'x'
    if( ! is.null(i) ) return (i)
    
    # Get a matrix and obtain the inverse of this matrix
    i <- solve( x$get() )
    
    ## Set a cache
    x$setInv(i)
    
    return (i)
}
