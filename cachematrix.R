## These functions work together to store a matrix in a cache,
## check to see if the matrix inverse is stored in the cache, and
## if it is not stored in the cache, calculate the inverse matrix.

## makeCacheMatrix - this function stores a matrix, 'x', in a cache.
## It also defines a function to store the inverse of 'x' once calculated.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    get <- function() x
    setinverse <- function(inver) m <<- inver
    getinverse <- function() m
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve - this function tries to find an inverse matrix of 'x' in the cache.
## If it can't find the inverse of matrix 'x', it will calculate the inverse of
## matrix 'x'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }  
