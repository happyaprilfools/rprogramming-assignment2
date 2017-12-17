## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub func and methods
  
  inv <- NULL
  set <- function(y) {
    x <<- y 
    ## assigned the input matrix y to the variable x in the environment
    inv <<- NULL
  }
  get <- function() x
  ##return the matrix x
  setInverse <- function(inverse) inv <<- inverse
  ##set the cache inv equal to the inverse of the matrix x
  getInverse <- function() inv
  ##return the cached inverse of x
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special matrix returned by `makeCacheMatrix` above. If the inverse has...

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
