## makeCacheMatrix provides getters and setters for m and x 
## which are free variables with global scope. Using global 
## variables in this manner probably isn't good practice
## due to the fact that other functions have access to the same
## variables and can overwrite them leading to unpredictable 
## behavior. 
## cacheSolve checks if m is null. If it is then it calls ginv on
## the matrix, otherwise it uses the previously saved global value.
## To test, try the following:
## foo = matrix(1:9, nrow=3, ncol=3)
## boo = makeCacheMatrix(foo)
## cacheSolve(boo)
## execute cacheSolve(boo) again to confirm that is uses cache on
## subsequent calls

## see above for comment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  setsolve <- function(solve) { m <<- solve }
  getsolve <- function() { m }
}


## see above for comment

cacheSolve <- function(x, ...) {
  require(MASS)
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setsolve(m)
  m
}
