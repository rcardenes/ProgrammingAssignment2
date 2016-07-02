## This script provides functions to calculate the inverse of a square matrix,
## memoizing it in the process (finding the inverse is an expensive process).
##
## We achieve this creating a list object that will hold the values of the matrix and
## its inverse (using makeCacheMatrix), and a function that calculates, memoizes, and
## returns the inverse itself (cacheSolve)
##
## The general structure of the functions is based on the "memoize mean" example.


##   makeCacheMatrix(x = matrix())
##     Accepts a matrix as initial value. By default, creates an empty one.
##     The environment for this function holds two values: the matrix, and the memoized
##     value of its inverse (NULL, until it's calculated for the first time)
##     Returns a list object that holds as elements a number of functions:
##      - set: replaces the matrix passed during initialization with a new one
##      - get: retriaves the currently stored matrix
##      - setinv: replaces the value of the inverse matrix
##      - getinv: retrieves the value of the inverse matrix
##
## NB: The function won't check its input, and store anything as "x" 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(get = get, set = set,
       setinv = setinv,
       getinv = getinv)
}


##   cacheSolve(x, ...)
##     Accepts as input a value returned by makeCacheMatrix. It will return the
##     inverse of the matrix stored in x (if it exists). If the inverse had been
##     calculated before, it will be returned straight away. It will be calculated
##     and memoized, otherwise. The inverse will be found using the "solve" function.
##
##     Any additional arguments will be passed to "solve.
##
##   Note: if the input value does not contain a SQUARE matrix, or the inverse for
##         the matrix does not exist, an error will be triggered.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (is.null(inv)) {
    inv <- solve(x$get())
    x$setinv(inv, ...)
  }
  inv
}