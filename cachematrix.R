# Assignment Instructions : “Matrix inversion is usually a costly  
# computation and there may be some benefit to caching the inverse of
# a matrix rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the 
# inverse of a matrix.”

# makeCacheMatrix creates an matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#cacheSolve computes the inverse of the matrix if it is not yet 
# calculated and retrieves it if it has been 

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
    if(!is.null(inv)) {
        message(“getting cached data" )
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
