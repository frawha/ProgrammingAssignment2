## Put comments here that give an overall description of what your
## functions do


## This function creates a list corresponding to a matrix. The ist returned is contains functions. 
## These funtions allow to set the matrix, get the matrix, set its inverse and get its inverse in 
## in the case itis inverse has been previously calculated and cahced. 

makeCacheMatrix <- function(x = matrix()) 
{
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Allows the user to obtain the inverse os a matrix that has been pased trough the makeCacheMatrix function.
## This function first checks to see if the inverse is cached, if so the inverse is returned, if not the 
## inverse is calculated, cached and returned.

cacheSolve <- function(x, ...)
{
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
