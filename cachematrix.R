
## This function creates a special "matrix" object that can cache its inverse.
## If the contents of a matrix are not changing, it may make sense to cache 
## the value of the inverse so that when we need it again, it can be looked up 
## in the cache rather than recomputed.
##
## x is the matrix to be used to calculate its inverse. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the value of the inverse of the matrix
    setinv <- function(inv) i <<- inv
    
    ## get the value of the inverse of the matrix
    getinv <- function() i
    
    ## returns "matrix" list object that can cache its inverse.
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
##
## x is the matrix object returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    
    ## get the possible cached inverse of the matrix.   
    i <- x$getinv()
    
    # if it is cached, then return it.
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
     
    ## the inverse is not cached, so we need to calculate it.
    ## get the matrix to find its inverse.
    data <- x$get()
    
    # calculate the inverse of the matrix
    i <- solve(data, ...)
    
    # cache the calculated matrix
    x$setinv(i)
    
    ## return a matrix that is the inverse of 'x'
    i
}
