## This function computes and caches the inverse of a matrix

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    ## check if inverse has been solved
    ## if inverse is solved, return from cache
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    
    ## if inverse is not solved, solve here
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
