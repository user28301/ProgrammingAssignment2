## This file create a matrix with cached inverse.

## To create a cached matrix: cm <- makeCacheMatrix (m)

makeCacheMatrix <- function (x = matrix ())
{
    cachedInverse <- NULL
    
    set <- function (y)
    {
        x <<- y
        cachedInverse <<- NULL
    }
    
    get <- function ()
    {
        x
    }
    
    setInverse <- function (inverse)
    {
        cachedInverse <<- inverse
    }
    
    getInverse <- function () 
    {
        cachedInverse
    }
    
    list (set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function solve, cache, and return the inverse to a cache matrix
## usage: inv <- cacheSolve (cm)

cacheSolve <- function (x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse ()
    
    if (!is.null (inverse))
    {
        message ("getting cached data")
        return (inverse)
    }
    
    data <- x$get ()
    inverse <- solve (data, ...)
    x$setInverse (inverse)
    inverse    
}
