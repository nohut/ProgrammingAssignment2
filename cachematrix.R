## Function: makeCacheMatrix
## Param x: matrix
##
## Creates a special matrix object that caches the  
## given x matrix value and its inverse 
##
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	
	list(set = set, get= get, setinv = setinv, getinv = getinv)
}
## Function: cacheSolve
## param x: matrix
## Calculates the inverse of the given x value if it is not null.
## Otherwise, it reads from the cache
##
##

cacheSolve <- function(x) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	data <- x$get()

	## Check if the inverse of the data is not null and
	## if the matrix has been changed.

	if (!is.null(i) && !is.null(prevdata) && !is.null(data) && dim(prevdata) == dim(data) && all(prevdata == data)) {
		message("getting cached data.")
		return(i)
	}

	prevdata <<- data
	i <- solve(data)
	x$setinv(i)
	i
}
	