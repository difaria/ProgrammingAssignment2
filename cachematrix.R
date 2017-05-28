## A set of functions to do Matrix inversion, with cache

## This function returns a list of functions that helps you cache the inverse of a Matrix
makeMatrix <- function(matrix = matrix()) {
  mInvertida <- NULL
  
  set <- function(y){
    matrix <<- y
    mInvertida <<- NULL
  }
  
  get <- function() matrix
  setInverso <- function(matrixInvertida) mInvertida <<- matrixInvertida
  getInverso <- function() mInvertida
  
  list(set = set, get=get,
       setInverso = setInverso,
       getInverso = getInverso)
}


## Taking the return of the makeMatrix, and checking if there is cache for the inverse of the matrix.
## If there is none, calculates the inverse of the Matrix, store on cache structure and return it
cacheSolve <- function(matrix, ...) {
  iMatrix <- matrix$getInverso()
  
  if (!is.null(iMatrix)){
    message("there is cache!")
    return (iMatrix)
  }
  
  data <- matrix$get()
  dataInv <- solve(data, ...)
  matrix$setInverso(dataInv) 
  dataInv
}
