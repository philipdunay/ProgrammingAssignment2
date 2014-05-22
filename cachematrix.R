## Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
# Set the value of the matrix
# Get the value of the matrix
# Set the value of inverse of the matrix
# Get the value of inverse of the matrix
# List out the values computed

# Accept input
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
# Set and get inverses
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

# List outcomes
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# --------------------------------------------------------------------------------
# Next, this function will check to see whether the invertible matrix inverse has been computed. 
# If it has, it will be returned, if it has not, it will be calculated.

# Accept input
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
# Compute inverse if necessary
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
