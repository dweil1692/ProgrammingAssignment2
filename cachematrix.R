## The following scripts generates a matrix object that can cache its inverse.
#It then returns the cached matrix.  If there is no cached matrix, it computes the inverse,
#then saves it to the cache for use later.

## Generates the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinverse = function(solve) m <<- solve
  getinverse = function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Checks if there is a cached matrix. If so, it returns the inverse of the matrix
# from the cache.   If not, it calculates and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m = x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}
