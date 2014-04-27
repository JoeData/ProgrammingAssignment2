## Coursera - R Programming - Programming Assignment 2 (peer assessment)
## 
## Author - https://github.com/JoeData/ProgrammingAssignment2/
##
## Functions - makeCacheMatrix, cacheSolve


## makeCacheMatrix accepts matrix, gets its inverse and stores inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  
  ## setting variables 
  m  <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  
  ## inverting matrix and storing in cache
  get  <- function() x
  setInverse  <- function(solve) m <<- solve  
  getInverse  <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve accepts matrix, checks if its inverse is cached 
## if cached, returns the cached inverted matrix
## if not cached, inverts the matrix and returns

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  ## checking if inverted matrix is cached and if so, returns
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if not in cache, inverts and returns
  data <- x$get()
  m <- solve(data, ...) 
  x$setInverse(m)
  m 
}
