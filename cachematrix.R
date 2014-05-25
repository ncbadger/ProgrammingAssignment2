## This R script contains two functions.
## The makeCacheMatrix is used as cache for a matrix and it's inverse.
## The cachSolve function uses a cache matrix and the stored matrix and inverse
## to return the inverse of the matrix stored in the cache.

## The makeCacheMatrix function returns a matrix cache that can be used
## to keep track of a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) 
{
    inverse <- NULL
    
    ## function used to set the matrix
    setMatrix <- function(theMatrix)
    {
        x <<- theMatrix
        inverse <<- NULL
    }
    
    ## function used to get the matrix
    getMatrix <- function()
    {
        x
    }
    
    ## function used to set the inverse
    setInverse <- function(theInverse) 
    {
        inverse <<- theInverse
    }
    
    ## funciton used to get the inverse
    getInverse <- function() inverse
    
    
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## The cacheSolve function is used to get the inverse of a matrix
## If the inverse has been calculated and is in the cache, the cached inverse is returned
cacheSolve <- function(x, ...) 
{
    inv <- x$getInverse()
    
    if(!is.null(inv)) 
    {
        message("getting inverse matrix from the cache")
        return(inv)
    }
    
    ## Get the matrix, find the inverse, set the inverse for the cache
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
