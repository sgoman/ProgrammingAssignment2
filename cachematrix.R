## A pair of functions for programming assignment 2 of the R programming course

## makeCacheMatrix will hold both the input matrix as well as a cache for its 
## inverse once computed
makeCacheMatrix <- function(x = matrix()) {
  # c is the cache, defaults to NULL on initialization
  c <- NULL
  
  # setter method resets x to the new matrix and c back to NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  
  # getter method to return the matrix value x
  get <- function() {
    x
  }
  
  # setter method to store an inverted matrix in the cache for future reference
  setInverse <- function(inv) {
    c <<- inv
  }
  
  # getter method to return the cached inverse of the matrix (might be NULL!)
  getInverse <- function() {
    c
  }
  
  # set the return list to enumerate over this objects methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will compute the inverse of an input matrix 
## (which is guaranteed to be inversable)
cacheSolve <- function(x, ...) {
  ## get the cached value of x
  i <- x$getInverse()
  
  if (!is.null(i)) {
    ## the cache is not NULL, therefore just return its value
    message("getting cached data")
    return (i)
  }
  
  ## if we reach this point the cache was NULL
  ## get the original matrix into data
  data <- x$get()
  
  ## solve data to get the inverse matrix
  i <- solve(data)
  
  ## store the inverse in the original object using its method
  x$setInverse(i)
  
  ## return the newly computed inverse
  i
}