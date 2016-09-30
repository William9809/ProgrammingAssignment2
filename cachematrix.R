## The following functions cache the result of matrix inversion. 
## This can be an advantage as it may avoid the need for repetition of this 
## computationally demanding process.
##
## To use these functions to invert a matrix 'x':
## 1) Assign the result of makeCacheMatrix(x) to a cache variable
## 2) Call cacheSolve with this cache variable as the argument, 
##    and the inverse matrix is returned
##
## Code note: The course notes show makeVector including a subfunction 'set'. 
## I have omitted 'set' as it is not called and does not change the functionality.


## The makeCacheMatrix function creates a cache. The cache is a list of functions which 
## calculate and cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix(NA)
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list (get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function returns the inverse matrix, the first time by calculation,
## the second and subsequent times from the cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!identical(i,matrix(NA))) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
