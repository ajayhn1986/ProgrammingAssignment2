## Methods to create and read the matrix and its inverse.
## function makeCacheMatrix - to set and get matrix and its inverse.
## function cacheSolve - to read inverse matrix from cache if available, else it computes the invese and returns it.

## makeCacheMatrix creates an list containg methods set, get, setinverse, and getinverse
#### Input
## a matrix() object
## Specifically written for invertible matricies. So kindly input only invertible matrix if you want to read its inverse
#### Output
## list containg methods set, get, setinverse, and getinverse
## where set and get methods sets or get a matrix
## where in setinverse and getinverse will set or get an inverse of the above set matrix

makeCacheMatrix <- function(matrixX = matrix()){
  inverseMatrix <- NULL
  set <- function(y){
    matrixX <<- y
    inverseMatrix <<- NULL
  }
  get <- function() matrixX
  setinverse <- function(inverseMatrixX) inverseMatrix <<- inverseMatrixX
  getinverse <- function() inverseMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix, created using makeCacheMatrix function, from cache if available else computes and returns it
#### Input
## A list object created  trrough makeCacheMatrix function
#### Output
## Inverse of the matrix created in makeCacheMatrix function

cacheSolve <- function(makeMatrixX, ...){
  inverseMatrix <- makeMatrixX$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- makeMatrixX$get()
  m <- solve(data, ...)
  makeMatrixX$setinverse(m)
  m
}
