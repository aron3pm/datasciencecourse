#Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#ANSWER: This function is capable of giving the inverse of a matrix, as can be seen you transform it by setting the variables. 
#Then, you create two functions to set and get, combining them into a list, so that they work optimally.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
  
  ## Write a short comment describing this function
  #You can retrieve the inverse function if it has already been calculated by checking if it was already inversed or not. 
  #After creating the parameter if, to check it, it will return the inverse function if it hasn't been converted yet.
  
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