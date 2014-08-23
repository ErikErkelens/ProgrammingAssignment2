## Functions to hold and calculate a matrix and its inverse
## Allow for caching of the inverse


makeCacheMatrix <- function(x = matrix()) {
	# Creates a object that can hold a matrix its inverse
	#
	# Args:
	#	x: the matrix
	#
	# Returns:
	#	List with accessor functions for the matrix and inverse

	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) i <<- inverse
	
	getinverse <- function() i

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
	# Returns the inverse of a matrix. Uses cached result if available
	#
	# Args:
	#	x:   the matrix
	#	...  optional aruguments for the 'solve' function
	#
	# Returns:
	#	Inverted matrix

    i <- x$getinverse()
    if(!is.null(i)) {
    	message("getting cached data")
    	return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
