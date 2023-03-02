## This set of functions uses caching to optimize 
## calculation of inverse of a matrix

## makeCacheMatrix - takes argument matrix and 
## returns list of functions to support optimized 
## calculation of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
      set <- function(y) {
      	x <<- y
            minv <<- NULL
      }
      get <- function() x
	setinverse <- function(m) minv <<- m
	getinverse <- function() minv

	list(set=set, get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}


## cacheSolve - takes argument of list of functions 
## returned from makeCacheMatrix and 
## returns the inverse of the matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	minv <- x$getinverse()
	if(!is.null(minv)){
		message("getting cached data")
		return(minv)
	}
	m <- x$get()
	minv <- solve(m)
	x$setinverse(minv)
	minv
}
