## Two functions, namely makeCacheMatrix and cacheSolve functions,
## are written in this R file to cache the inverse of a matrix. 

## makeCacheMatrix function creates a matrix object that
## can cache its inverse. 
## Specifically, the makeCacheMatrix function involves:
## 1. setting the value of the matrix
## 2. getting the value of the matrix
## 3. setting the inverse of the matrix
## 4. getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setIM <- function(z) m <<- z
  getIM <- function() m
  list(set = set, get = get,
       setIM = setIM,
       getIM = getIM)
}


## cacheSolve matrix function computes the inverse of the matrix
## returned by makeCacheMatrix function.  
## The cacheSolve function works by checking if the inverse of 
## the matrix has already been calculated. If so, it retrieves 
## the inverse of the matrix from the cache. Otherwise, it 
## calculates the inverse of the matrix and set it in the cache. 

cacheSolve <- function(x, ...) {
  m <- x$getIM()
  if( !is.null(m) ) {
    message("Remark: getting the inverse of matrix from cached data")
    return(m)
  }
  data <- x$get()
  message("Remark: calculating the inverse of matrix for this time")
  m <- solve(data, ...)
  x$setIM(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
