## The following functions create a matrix that can cache its inverse, 
## then calculate the inverse of the matrix, unless the inverse had
## already been calculated, in which case the inverse will be retrieved

## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- inverse
        getinverse <- function() inv
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix
## If the inverse has already been calculated, the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        inv
}
