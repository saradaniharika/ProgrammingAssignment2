## This pair of functions will allow the inverse
## of a square matrix to be cached.

## This function generates a list of functions which
## will set & get the value of the matrix, and 
## set & get the value of the inverse of the matrix

makeCacheMatrix <- function(x= matrix(1:4,nrow = 2,ncol = 2)){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(inverse) x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function calculates the inverse of the matrix
## (unless it is already cached, in which case it uses that)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
}
