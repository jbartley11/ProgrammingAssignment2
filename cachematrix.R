# This file contains two functions, makeCacheMatrix and cacheSolve
# THey cache the inverse of a given matrix, since inverse is computationally
# intensive it makes sense to store the inverse instead of calculating every
# time the value is needed


# makeCacheMatrix returns a list containing 4 functions:
# set: sets the value of the matrix
# get: returns the value of the matrix
# set_inverse: sets the value of the inverse
# get_inverse: returns the inverse value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


# the cacheSolve function checks to see if the inverse 
# was already cached, if not it calculates and caches it
# it returns the inverse value

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
