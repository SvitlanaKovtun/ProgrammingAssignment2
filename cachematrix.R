## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing
## four functions
## 1) function "set" sets the value of the matrix
## 2) function "get" gets the value of the matrix
## 3) function "setsolve" sets the value of the inverse matrix
## 4) function "getsolve" gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setsolve <- function(solve) 
    m <<- solve
  getsolve <- function() 
    m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function calculates the inverse of the special "matrix" 'x' created with the above 
## function makeCacheMatrix and returns the inverse of 'x'. 
## However, it first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips 
## the computation using getsolve function. 
## Otherwise, it calculates the inverse matrix of the given matrix and sets the value of 
## the matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
