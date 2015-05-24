## SYNOPSIS   : This file contains a pair of functions that cache the inverse of a matrix
## DESCRIPTION: Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
## ASSUMPTION : The functions can be used only if the matrix supplied are invertible

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##                  This function takes an invertible matrix as its argument
##                  This function returns a list containing function to
##                    1. set the value of the matrix
##                    2. get the value of the matrix
##                    3. set the value of the inverse
##                    4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then cacheSolve should retrieve the inverse from the cache.
##             This function takes the matrix to be inverted as its argument
##             This function returns the inverted matrix
cacheSolve <- function(x, ...) {        
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
