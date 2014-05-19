## The following pair of functions cache the inverse of a matrix. 
## These function save valuable computation cost by returning the inverse from 
## the cache when there is no change to the matrix rather than recalculating
## inverse every time

##  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    ## The function below set the value for the metrix and also set the inverse to null and assign to cache
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## The function below returns the metrix
    get <- function() x
    
    ## The function below sets the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## The function below returns the inverse of the matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
