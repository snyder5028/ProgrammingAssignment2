## Together, these two functinos, makeCacheMatrix and cacheSolve, create a cached inverse matrix for 
## a give matrix so that the inverse will not need to be calculated multiple times.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
     cachedInverse <- NULL
     getOrigMatrix <- function() {
          x
     }
     setInverse <- function(calculatedInverse){
          cachedInverse <<- calculatedInverse 
     }
     getInverse <- function() {
          cachedInverse
     }
     list(getOrigMatrix = getOrigMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve either computes the inverse of the special "matrix" returned by makeCacheMatrix above or
## retrieves the the inverse from the cache if it has been computed already.

cacheSolve <- function(x, ...) {
     inverseMatrix <- x$getInverse()
     if (!is.null(inverseMatrix)) {
          print("getting cached inverse")
          inverseMatrix
     } else {
          origMatrix <- x$getOrigMatrix()
          inverseMatrix <- solve(origMatrix)
          x$setInverse(inverseMatrix)
          inverseMatrix
     }
}
