## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set,
	     get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
	
	
}


## This function returns a matrix that is the inverse of argument 'x'
## Returns from cache if exists already otherwise calls makeCacheMatrix
cacheSolve <- function(x, ...) {
	
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	# Data was not found in the cache above...solve it
	
	data <- x$get()

	# Check if matrix can be inversed. Singular matrixes 
	# cannot be inversed. No point in proceeding otherwise.
	
	f <- function(j) class(try(solve(j),silent=T))=="matrix"

	if(f(data)) {
		m <- solve(data, ...)
		x$setInverse(m)
		m
	}
	else
	{
		# Provide a meaningful error and bail out
		message("Matrix is singular and cannot be inversed")
		stop("Retry with different data")
	}
		
	
	
}
