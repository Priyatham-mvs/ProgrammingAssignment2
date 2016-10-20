## To get a pair of functions that cache the inverse of a matrix.

## 1. Function to create a special object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## "matrix" object is createed that can cache its inverse.


## 2. Function to compute the inverse of the special object returned by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## Computing the inverse of matrix using the "solve" function
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
## The inverse of the "matrix" is calculated