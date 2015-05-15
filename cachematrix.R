## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    result <- NULL
    
    set <- function(y) {
        x <<- y
        result <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse) result <<- inverse
    getInverse <- function() result
    
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting inverse data in memory")
        return(inverse)
    }
    
    message("can't get inverse in memory, compute now")
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    
    inverse
}
