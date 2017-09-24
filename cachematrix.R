## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function takes a matrix and returns 
# a list of four functions: set, get, setInverse, and getInverse.
#Set assigns a new matrix as the inputs and NULL as the inverse,
#Get simply returns the input matrix
#setInverse assign a value to the cached value of the inverse
#getInverse returns the cached value of the inverse
makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    m <<- newMatrix
    inverse <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  getInverse <- function() {
    inverse
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#This function takes a cached matrix as defined in the makeCacheMatrix function
#It first checks if the "inverse" value on the cached matrix; if it is not NULL, it returns it
#Otherwise, it calls the built in "solve" function to inverse the input matrix.
#Before returning it, it sets the results into cache by calling setInverse, so the next time
#we call it, we don't need to compute the value again.
cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- cacheMatrix$get()
  inverse <- solve(matrix, ...)
  cacheMatrix$setInverse(inverse)
  ## Return a matrix that is the inverse of 'm'
  inverse
}
