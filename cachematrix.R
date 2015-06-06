## Assignment 2 -- to write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## init a matrix
  m <- NULL
  
  ##  changes the vector stored in the main function
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  
  ##  returns the matrix stored in the main function
  get <- function() x
  
  ## store the inversed matrix
  setsolve <- function(solve) m <<- solve
  
  ## returns the inverted matrix stored in the main function
  getsolve <- function() m
  
  ## creates a list of funstions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## gets the matrix from cache
  n <- x$getsolve()
  
  ## if found something in the cache, then use it
  if(!is.null(n) ) {
    message("getting cached data")
    return(n)
  }
  ## nothing found in the cache, get the matrix
  data <- x$get()
  
  ## calculate inverted matrix
  n <- solve(data, ...)
  
  ## save inverted matrix in the cache
  x$setsolve(n)
  
  ## return the inverted matrix
  n
}
