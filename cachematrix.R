## The following functions will provide utilities to calculate the inverse of a matrix 
## using a cache to improve performance.

## The function makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() x
  setCacheMatrix <- function(cM) cacheMatrix <<- cM
  getCacheMatrix <- function() cacheMatrix
  list (set = set, get = get, setCacheMatrix = setCacheMatrix, getCacheMatrix = getCacheMatrix)
}


## The following function calculates the inverse of a special matrix created with the
## above function. However, it first checks to see if the inverse has already been calculated.
## If so, it get's the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the data and sets the value of the inverse in the cache via
## the setCacheMatrix function

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getCacheMatrix()
  if (!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  originalMatrix <- x$get()
  inverseMatrix <- solve(originalMatrix)
  x$setCacheMatrix(inverseMatrix)
  inverseMatrix
}
