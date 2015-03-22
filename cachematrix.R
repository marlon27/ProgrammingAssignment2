## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- x  
  matri<-x
  setMatrix <- function(y) {
    matri <<- y
    inverse <<- y
  }
  getMatrix <- function() matri
  setInvMatriz <- function(Inv) inverse <<- Inv
  getInvMatriz <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatriz = setInvMatriz,
       getInvMatriz = getInvMatriz)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  n <- x$getMatrix()
  m <- x$getInvMatriz()
  if(!identical(n, m)) {
    message("getting cached data")
    return(m)
  }
  dataInv <- x$getMatrix()
  m <- solve(dataInv)
  x$setInvMatriz(m)
  m
}