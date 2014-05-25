## Functions that create a matrix object that can cache it's inverse,
## rather than creating it repeatedly

## makeCacheMatrix() takes a matrix 
## m stores inverse of matrix
## each time makeCacheMatrix is set m is set to null
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


## cacheSolve() returns a matrix that is the inverse of inpu
## reads cached inverse if available
## sets cached inverse if computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getcminverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
