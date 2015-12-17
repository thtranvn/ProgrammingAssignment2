## In this file there are 2 functions for caching the inverse of a matrix
##    makeCacheMatrix is to create special "matrix" object that can cache its inverse
##    cacheSolve to calculate or get cached value of inverse of the matrix

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse matrix
##   get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    im <- matrix(nrow=0,ncol=0) # Empty matrix
    
    set <- function(y) {
        x <<- y
        im <<- matrix(nrow=0,ncol=0) # Empty matrix
    }
    
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    
    if(nrow(im)>0 & ncol(im)>0) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)  # Return a matrix that is the inverse of 'x'
    x$setinverse(im)
    im
}
