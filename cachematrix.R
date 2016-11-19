## This script contains 2 functions -
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve will retrieve the inverse from the cache.

## The makeVector function,  creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mat) inv <<- mat
  getinv <- function() inv
  
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## The following function calculates the inverse of the matrix passed to 
## the above function. However, it first checks to see if the inverse matrix has 
##already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the inverse matrix
##in the cache via the setinv function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
   if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data , ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
