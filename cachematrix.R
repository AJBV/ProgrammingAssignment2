## Utilize two functions calculate, store, and display the inverse of
## a matrix 

## makeCacheMatrix creates a matrix object that can store the inverse 
## of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve solves the inverse of the previous matrix from makeCacheMatrix
## If above matrix does not change, cacheSolve will get the inverse from
## from the stored cache.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
}

