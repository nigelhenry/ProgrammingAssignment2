## This file contains two functions that is the solution to assignment 2 in coursera "Introduction to R"
## The solution is made of two functions, one that sets up the special "matrix" the other that calculates and caches the inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ## the inverse is set using the function below
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## if the inverse is set, it will display
  if(!is.null(inv)) {
    message("getting cached data");
    return(inv);
  }
  ## else, it is recalculated here
  data <- x$get();
  inv <- solve(data, ...)
  x$setinv(inv);
  inv
}
