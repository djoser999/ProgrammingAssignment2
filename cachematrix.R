## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  get <- function() x
  setSolved <- function(solved) inv <<- solved
  getSolved <- function() solved
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getSolved()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setSolved(inv)
  inv  
          ## Return a matrix that is the inverse of 'x'
}
