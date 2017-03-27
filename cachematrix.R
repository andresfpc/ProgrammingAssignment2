## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 'makeCacheMatrix' is a R function that creates a special matrix object that cache its inverse so it's not necessary to calculate it every time.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## Write a short comment describing this function
## Using the built object returned by 'makeCacheMatrix' function the inverse of this special matrix is computed
cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("  Getting cached data...\n  Showing inverse of this matrix\n")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	return(m)
	## Return a matrix that is the inverse of 'x'
}
