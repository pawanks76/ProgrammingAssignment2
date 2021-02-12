## Creating functions that wil cache the inverse of a matrix.


## The following code will create a object (matrix) that can cache its inverse. 

makeCacheMatrix <- function( x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function will the inverse of the matrix output of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(!is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data) %*% data
  x$setInverse(i)
  i
}

