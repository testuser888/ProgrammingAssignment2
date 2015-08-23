## These functions takes any valid square matrix as input
## and returns its inverse, from cache if it already exists

## makeCacheMatrix takes a valid square matrix and returns
## a list to feed into the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y) {
		x <<- y
		invx <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invx <<- inverse
	getinverse <- function() invx
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## cacheSolve takes a list as input from makeCacheMatix
## and checks if the inverse is cached already, if not it
## uses the solve() function to return the inverse
## NOTE: You must pass the return value of makeCacheMatrix
	 as the argument to this funtion, *not* the original
	 square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invx <- x$getinverse()
	if(!is.null(invx)) {
		message("getting cached data")
		return(invx)
	}
	data <- x$get()
	invx <- solve(data, ...)
	x$setinverse(invx)
	invx
}
