## Put comments here that give an overall description of what your
## functions do

## This function create a special matrix and also can get and set
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## This function first check the inverse matrix in the cache 
## and return if found. If not found in cache the create inverse matrix, store
## in cache and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
