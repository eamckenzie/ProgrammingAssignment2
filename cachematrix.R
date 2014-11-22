## These 2 functions find the inverse of a matrix more efficiently
## by caching the inverse the first time it's calculated.

## Example of usage--
## First run makeCacheMatrix and cacheSolve to store the functions.
## In the console, run:
## > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##   to cache a 2x2 matrix named "amatrix".
## > amatrix$get()
##   to return the matrix
## > cacheSolve(amatrix)
##   to return and cache the inverse of the matrix
## > amatrix$getinverse()
##   to return the inverse of the matrix as cached by cacheSolve
## Use amatrix$set() and amatrix$setinverse() to play with the cached
## values of amatrix. You can set an incorrect inverse matrix using
## amatrix$setinverse() and correct it using cacheSolve(amatrix).

## makeCacheMatrix creates a "matrix" object that can cache its
## inverse. Actually, it is a list of functions for storing the
## matrix, retrieving its values, finding and storing its inverse,
## and retrieving its inverse.

makeCacheMatrix <- function(a = matrix()) {
  b <- NULL
  set <- function(c) {
      a <<- c
      b <<- NULL
  }
  get <- function() a ## Not sure this works.
  setinverse <- function(solve) b <<- solve
  getinverse <- function() b
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates & stores the inverse of the "matrix" from
## makeCacheMatrix, checking first whether the inverse is already
## calculated.

cacheSolve <- function(a, ...) {
  b <- a$getinverse()
  if(!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  data <- a$getMatrix()
  b <- solve(data, ...)
  a$setinverse(b)
  b
}

