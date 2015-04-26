## This program computes the inverse of a matrix.
## If the inverse has already been calculated then
## it attempts to get the inverse from the cache.

## The following function takes a matrix as input
## and gives a list of functions that can be
## used to set/get the matrix and set/get the 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	if (!is.matrix(x)) {
    		message("enter a matrix as input")
    		return(x)
  	}
  	m <- NULL
  
    ## This function sets x and m in the main 
    ## function makeCacheMatrix()
  	set <- function(y) {
    		x <<- y
    		m <<- NULL
  	}
  
  	get <- function() x
  
	  ## This function sets m in the main 
    ## function makeCacheMatrix()
  	setInverse <- function(matrix) m <<- matrix
  
  	getInverse <- function() m

  	list(set = set,
        	get = get,
        	setInverse = setInverse,
        	getInverse = getInverse)
}


## The following function take the list of 
## functions from the previous function as an 
## input and computes the inverse of a matrix
## using data returned from using these 
## functions. If the inverse has already been 
## calculated then it doesn't compute but 
## returns the value stored

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
