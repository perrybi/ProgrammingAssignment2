## This pair of functions cache the inverse of a matrix 

## makeCacheMatrix creates a 'special matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # set inv to NULL as 'placeholder' for future assignment
  set <- function(y) { # function set, sets matrix x, to a new matrix y and resets inv to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() x # function get returns the matrix x
  setinv <- function(inv_comp) inv <<- inv_comp # function setinv, sets inv to the computed inv_comp
  getinv <- function() inv # function getinv, returns the cached inverse 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  # function list, returns a special vector containing all the defined funcitons
}


## cacheSolve matrix computes the inverse of the special matrix computed in the function above: 
##   if the inverse (for the same x) is already computed, cacheSolve returns the inverse
##   from the cache and skips the recomputation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # get the cached inv for matrix x
  if(!is.null(inv)) { # checks if the cached inverse is not NULL (cached inverse already computed)
    message("getting cached matrix inverse")
    return(inv) # returns the cached inverse and skips the recomputation of the inverse
  }
  data <- x$get() # otherwise, get the matrix x
  inv <- solve(data, ...) # compute the inverse
  x$setinv(inv) # saves the computed inverse in the cahce
  inv # returns the inv
}
