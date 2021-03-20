## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {  ##Checking if inverse has already been calculated
    message("getting inversed matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setsolve(inv)
  inv  ##Returns the inverse of mat (matrix)
}
