## cachematrix.R contains functions makeCacheMatrix and cacheSolve
## makeCacheMatrix produces an object on top of the normal R matrix
## with a setter and getter, and then a setter and getter for the inverse 
## of the matrix. We do this because that way we can have an object that
## contains the matrix and also caches its inverse in case we need
## to access the inverse of the matrix repeatedly. Matrix inversion
## is a costly computation, so it is useful to store this value.

## makeCacheMatrix is a function that contains multiple functions 
## and stored values, i.e. a function that is also an object
## essentially it's a constructor for the CacheMatrix object
## and takes a matrix, stores it and has setting and getting
## functions for the matrix and its invers

makeCacheMatrix <- function(x = matrix()) {
  ## m is the variable containing the inverse
  m <- NULL
  ## set(y) takes takes a vector
  ## and chances the value of the stored vector
  ## we don't really need to use this. it also
  ## sets the cached matrix to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get() returns the matrix x
  get <- function() x
  ## setinverse sets the inverse
  ## its used by cacheSolve
  ## but can also be called using the $ operator
  setinverse <- function(inv) m <<- inv
  ## get inverse returns the inverse of the matrix x
  ## stored in the variable m
  getinverse <- function() m
  ## this list object basically serves as an environment
  ## connects symbols within the makeCacheMatrix function
  ## with their corresponding values, which are also functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## cacheSolve takes a cacheMatrix object 
## and calculates the inverse if the inverse is not already calculated
## ie the input 'x' is not a matrix but a cacheMatrix containing a matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## first it checks to see if the inverse is already calculated
  ## by calling the getinverse() method of the cacheMatrix object
  m <- x$getinverse()
  ## if the inverse is already calculated and cached, it just returns 
  ## the value in cache, ie the precalculated inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## or else it gets the matrix from the cacheMatrix object
  data <- x$get()
  ## calls the builtin matrix inversion method, solve()
  m <- solve(data, ...)
  ## and sets that as the cached inverse matrix within the cacheMatrix object
  x$setinverse(m)
  m
}
