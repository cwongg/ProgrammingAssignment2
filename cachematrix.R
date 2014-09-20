## These functions calculate the inverse of a matrix, and store it in the cache.  If the inverse is needed 
## again and the matrix has not been changed, the inverse is taken from the cache, instead of calculating it again

# Creates a special "matrix", which is really a list containing a function to 
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the inverse of the matrix
# 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

dim

## Calculates the inverse of a special matrix created with makeCacheMatrix.  If the inverse was already
## calculated, and the matrix has not changed, gets the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
